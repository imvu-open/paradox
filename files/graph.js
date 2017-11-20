define('graph', ["loader", "util", "widget", "graphsurface", "jquery-cookie", "context"], function(Loader, util, Widget, GraphSurface) {
/* A simple in-browser app to browse counters out of istatd. */
/* Copyright 2011 IMVU, Inc. Author: jwatte@imvu.com */
/*

todo:
- arrange colors and counter ordering explicitly
- render modes: area, stacked, line, min/max (in addition to "bands")
*/

/*  HEY Let's highjack DYGRAPH plugins! */
console.log(Dygraph.PLUGINS);
Dygraph.PLUGINS = Dygraph.PLUGINS.slice(1);
Dygraph.PLUGINS.unshift(ISTATD_Legend);
console.log(Dygraph.PLUGINS);

var pageIsHidden;
var visibilityChange;
var hidden;
var visibilitySupported = true;
if (typeof document.hidden !== "undefined") {
    pageIsHidden = function() {
        return document.hidden;
    }
    visibilityChange = "visibilitychange";
    hidden = "hidden";
} else if (typeof document.mozHidden !== "undefined") {
    pageIsHidden = function() {
        return document.mozHidden;
    }
    visibilityChange = "mozvisibilitychange";
    hidden = "mozHidden";
} else if (typeof document.msHidden !== "undefined") {
    pageIsHidden = function() {
        return document.msHidden;
    }
    visibilityChange = "msvisibilitychange";
    hidden = "msHidden";
} else if (typeof document.webkitHidden !== "undefined") {
    pageIsHidden = function() {
        return document.webkitHidden;
    }
    visibilityChange = "webkitvisibilitychange";
    hidden = "webkitHidden";
} else {
    // Assume the browser does not support page visibility
    pageIsHidden = function() {
        return false;
    }
    visibilitySupported = false;
}

var document_hidden = visibilitySupported ? document[hidden] : false;

CONSOLE_LOG_ALERT = false;
if(typeof(console) === 'undefined') {
    console = {};
}

if(!console.log) {
    console.log = function() {
        if(CONSOLE_LOG_ALERT) {
            alert(Array.prototype.join.call(arguments, ' '));
        }
    }
}

var guiRefresh = function(shittyGlobals) {
    var end = begin('refresh');
    util.refresh(shittyGlobals);
    end();
}

var toggleEvents = function(shittyGlobals) {
    var end = begin('toggleEvents');
    shittyGlobals.theGrid._showEvents = !shittyGlobals.theGrid._showEvents
    for (var k in shittyGlobals.theGrid._allGraphs) {
        graph = shittyGlobals.theGrid._allGraphs[k];
        graph._showEvents = shittyGlobals.theGrid._showEvents;
    }

    if (shittyGlobals.theGrid._showEvents) {
        $('.tbutton.events').addClass('toggle');
    }else{
        $('.tbutton.events').removeClass('toggle');
    }

    shittyGlobals.theGrid.repaintAll();

    end();
}

function promptDialog(prmp, cb) {
    console.log('prompt: ' + prmp);

    $('.dialog.prompt').detach();
    var $div = $("<div class='dialog prompt'><span class='closebox buttonbox'></span><div class='text'></div>" +
        "<input type='text' class='answer'/><div class='tbutton' name='prompt_done'>OK</div></div>");
    str = util.htmlescape(prmp);
    $('div.text', $div).html(str);
    $div.prependTo($('body'));
    $('span.closebox', $div).click(function() {
        $div.remove();
    });
    $('.answer', $div).val('');
    var eHandler = util.guard(function(ev) {
        ev.stopPropagation();
        if(cb($('.answer', $div).val()) !== false) {
            $div.remove();
        }
    });
    $('.tbutton', $div).click(eHandler);
    $('.answer', $div).keydown(function(ev) {
        if (ev.which == 13) {
            $('#counter_jstree').jstree('deselect_all');
            eHandler(ev);
        }
    });
    return $div;
}

function promptDialog2(prmp, cb) {
    var graph = theShittyGlobals.theGrid.selected;
    var queries = _.map(graph._series, function(val, key) {
        return key;
    }).join("\n");
    console.log('prompt: ' + prmp);

    $('.dialog.prompt').detach();
    var $div = $("<div class='dialog prompt'><span class='closebox buttonbox'></span><div class='text'></div>" +
        "<textarea cols='60' rows='5' class='answer'/><div class='tbutton' name='prompt_done'>OK</div></div>");
    str = util.htmlescape(prmp);
    $('div.text', $div).html(str);
    $div.prependTo($('body'));
    $('span.closebox', $div).click(function() {
        $div.remove();
    });
    $('.answer', $div).val(queries);
    var eHandler = util.guard(function(ev) {
        ev.stopPropagation();
        if(cb($('.answer', $div).val()) !== false) {
            $div.remove();
        }
    });
    $('.tbutton', $div).click(eHandler);
    return $div;
}


//////////////////////////////
// begin date hack
function parseDate(x) {
    if (!x || typeof(x) != 'string') {
        return false;
    }
    var ret = new Date();
    var captures = /^ *(20[0-9][0-9])-0?(1?[0-9])-0?([123]?[0-9]) +([012][0-9]):([0-5][0-9]) *$/.exec(x);
    if (captures) {
        ret.setFullYear(parseInt(captures[1], 10));
        ret.setMonth(parseInt(captures[2], 10)-1);
        ret.setDate(parseInt(captures[3], 10));
        ret.setHours(parseInt(captures[4], 10), parseInt(captures[5], 10));
        return ret;
    }
    captures = /^ *(20[0-9][0-9])-0?(1?[0-9])-0?([123]?[0-9]) *$/.exec(x);
    if (captures) {
        ret.setFullYear(parseInt(captures[1], 10));
        ret.setMonth(parseInt(captures[2], 10)-1);
        ret.setDate(parseInt(captures[3], 10));
        return ret;
    }
    return false;
}

function resetDateRange(dFrom, dTo) {
    var ft = dFrom.getTime();
    var tt = dTo.getTime();
    if (tt < 1000000000) {
        tt = util.getAdjustedNow().getTime();
    }
    if (ft >= tt || ft < tt - 5 * 366 * 25 * 60 * 60 * 1000) {
        ft = tt - 15 * 60 * 1000;
    }
    theTimeSlider.setManual(true);
    theShittyGlobals.theOriginalDates.start = dFrom;
    theShittyGlobals.theOriginalDates.stop = dTo;
    util.restoreOriginalInterval(theShittyGlobals);
    setAutoRefresh(false);
    util.calcReloadInterval(theShittyGlobals);
    theShittyGlobals.theGrid.reloadAll();
}

function select_daterange() {
    util.promptDialogN('Please enter a date range (YYYY-MM-DD [HH:MM]):',
        {'from': 'From:',
        'to': 'To:',
        'maxSamples': 'Max Samples:<br/>(Optional. If not given, defaults to graph width.)'},
        util.guard(function(range) {
            console.log('select_daterange(' + JSON.stringify(range) + ')');
            var dFrom = parseDate(range.from);
            var dTo = parseDate(range.to);
            if (!dFrom) {
                util.errorDialog('The start date is not valid.');
                return false;
            }
            else if (!dTo) {
                util.errorDialog('The end date is not valid.');
                return false;
            }
            else if (dFrom.getTime() >= dTo.getTime()) {
                util.errorDialog('The end date must be after the start date.');
                return false;
            }
            else {
                if(!range.maxSamples.match(/^[0-9]*$/)) {
                    util.errorDialog('Max sample count must be a positive integer, or left blank.');
                    return false;
                }
                theShittyGlobals.theGraphMaxSamples = parseInt(range.maxSamples);
                if(isNaN(theShittyGlobals.theGraphMaxSamples)) {
                    theShittyGlobals.theGraphMaxSamples = undefined;
                }

                resetDateRange(dFrom, dTo);
                return true;
            }
        })
    ).addClass('select_daterange');

}
//
// end date hack
//////////////////////////////


function configureEvents(from_to, keep_old)
{
}

function configure_events() {
    configureEventsDialog('Configure events for this graph grid', configureEvents);
}

function configureEventsDialog(prmp, cb) {
    console.log('configure_events: ' + prmp);

    $('.dialog.prompt').detach();
    var $div = $("<div class='dialog prompt wider'><span class='closebox buttonbox'></span><div class='text'></div>" +
        "<table><thead class='headings' style='width: 100%'></thead>" + "<tbody class='inputs'></tbody></table><br><div class='tbutton prompt_add' name='prompt_add'>Add</div><div class='tbutton prompt_ok' name='prompt_ok'>Ok</div></div>");
    str = util.htmlescape(prmp);
    $('div.text', $div).html(str);
    $headings = $('thead.headings', $div);
    $heading = $(""
                    + "<td class='heading' style='width: 16%'> name </td>"
                    + "<td class='heading' style='width: 16%'> ident </td>"
                    + "<td class='heading' style='width: 16%'> keys </td>"
                    + "<td class='heading' style='width: 16%'> line color </td>"
                    + "<td class='heading' style='width: 16%'> thickness </td>"
                    + "<td class='heading' style='width: 16%'> filters </td>"
                    + "");
    $heading.appendTo($headings);
    $inputs = $('tbody.inputs', $div);

    _.map(this.loader._events, function(conf, key) {
        $input = $("<tr class='event_row'>"
                    + "<td><input type='text' class='name_' value ='" + conf.name + "'></td>"
                    + "<td><input type='text' class='ident_' value ='" + conf.ident + "'></td>"
                    + "<td><input type='text' class='keys_' value ='" + conf.keys.join(",") + "'></td>"
                    + "<td><input type='text' class='color_' value ='" + conf.color + "'></td>"
                    + "<td><input type='text' class='thickness_' value ='" + conf.thickness + "'></td>"
                    + "<td><input type='text' class='filters_' value ='" + conf.filters.join(",") + "'></td>"
                    + "<td><span class='closebutton buttonbox inline-block'></span></td>"
                    + "</tr>");
        var me = $input;
        $('span.closebutton', $input).click(function() {
            me.detach();
        });
        $input.appendTo($inputs);
    });

    $div.prependTo($('body'));
    $('span.closebox', $div).click(function() {
        $div.remove();
    });
    $('.answer', $div).val('');
    var eHandler = util.guard(function(ev) {
        ev.stopPropagation();
        $input = $("<tr class='event_row'>"
                    + "<td><input type='text' class='name_' value =''></td>"
                    + "<td><input type='text' class='ident_' value =''></td>"
                    + "<td><input type='text' class='keys_' value =''></td>"
                    + "<td><input type='text' class='color_' value =''></td>"
                    + "<td><input type='text' class='thickness_' value =''></td>"
                    + "<td><input type='text' class='filters_' value =''></td>"
                    + "</tr>");
        $input.appendTo($inputs);
    });
    var self = this;
    var eHandlerOk = util.guard(function(ev) {
        ev.stopPropagation();
        var events = $('tr.event_row', $inputs);
        var allEvents = {};
        _.map(events, function(el) {
            var name = $('.name_', el).get(0).value;
            var ident = $('.ident_', el).get(0).value;
            var keys = $('.keys_', el).get(0).value.split(",");
            var color = $('.color_', el).get(0).value;
            var thickness = $('.thickness_', el).get(0).value;
            var filters = $('.filters_', el).get(0).value.split(",");
            allEvents[name] = { name: name, ident: ident, keys:keys, color:color, thickness:thickness, filters:filters};
        });
        self.loader.configureEvents(allEvents);
        guiRefresh(theShittyGlobals);
        $div.remove();
    });
    $('.tbutton.prompt_add', $div).click(eHandler);
    $('.tbutton.prompt_ok', $div).click(eHandlerOk);
    $('.answer', $div).keydown(function(ev) {
        if (ev.which == 13) {
            $('#counter_jstree').jstree('deselect_all');
            eHandlerOk(ev);
        }
    });
}


//////////////////////////////
// xref hack

function xref_change()
{
    xrefSelectDialog('Alter graph to use cross-references instead. See source for more details.', change_counters);
}


function xrefSelectDialog(prmp, cb) {
    console.log('xrefSelect: ' + prmp);

    $('.dialog.prompt').detach();
    var $div = $("<div class='dialog prompt'><span class='closebox buttonbox'></span><div class='text'></div>" +
        "<div class='inputs'></div><div class='tbutton' id='prompt_change' name='prompt_change'>Change</div><div class='tbutton' id='prompt_add' name='prompt_add'>Add</div></div>");
    str = util.htmlescape(prmp);
    $('div.text', $div).html(str);
    $inputs = $('div.inputs', $div);
    $inputs.append('From ');
    $input = $("<select id=\"xrefFrom\"><option selected=\"selected\">Xref From</option></select>");
    $input.appendTo($inputs);
    $inputs.append(' to ');
    $input = $("<select multiple size=10 id=\"xrefTo\"><option selected=\"selected\">Xref To</option></select>");
    $input.appendTo($inputs);

    $div.prependTo($('body'));
    $('span.closebox', $div).click(function() {
        $div.remove();
    });
    $('.answer', $div).val('');
    var eHandler = util.guard(function(ev) {
        ev.stopPropagation();
        if (ev.target) {
            targ = ev.target;
        } else if (ev.srcElement) {
            targ = ev.srcElement;
        }
        if (targ.nodeType == 3) { // safari bug
            targ = targ.parentNode;
        }

        var xref_from = document.getElementById('xrefFrom');
        var xref_from_selected = xref_from.options[xref_from.selectedIndex].text;
        var xref_to = document.getElementById('xrefTo');

        if (targ.id == "prompt_change") {
            var xref_to_selected = xref_to.options[xref_to.selectedIndex].text;
            cb(xref_from_selected + ":" + xref_to_selected, 0);
        } else {
            // iterate over all selections when adding and add them individually
            for (x=0 ; x < xref_to.options.length ; x++)
            {
                if (xref_to.options[x].selected)
                {
                    cb(xref_from_selected + ":" + xref_to.options[x].text, 1);
                }
            }
        }

        $div.remove();
    });
    $('.tbutton', $div).click(eHandler);
    $('.answer', $div).keydown(function(ev) {
        if (ev.which == 13) {
            $('#counter_jstree').jstree('deselect_all');
            eHandler(ev);
        }
    });

    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() {
        if (xhr.readyState == 4) {
            var data = JSON.parse(xhr.responseText);
            var x = computeXref(data.agents);
            populateXrefFrom(x, "");
            document.getElementById('xrefFrom').onchange = function() { populateXrefTo(x); };
        }
    }
    xhr.open('GET', '/?a=*', true);
    xhr.send();
}


function populateXrefFrom(xrefs, filter) {
    var xref_from = document.getElementById('xrefFrom');
    var graphed_counters = getGraphedCounters();
    var option_categories = new Array();
    for (var cat in xrefs) {
        for (var ctr in graphed_counters) {
            var counter = graphed_counters[ctr];
            var category = String(cat);
            if (counter.substr(-1 * category.length) == category) {
                option_categories[String(cat)] = 1;
            }
        }
    }
    for (var catStr in option_categories)  {
       xref_from.options[xref_from.options.length] = new Option(catStr, catStr);
    }
}

function populateXrefTo(xrefs) {
    var xref_from = document.getElementById('xrefFrom');
    var xref_from_selected = xref_from.options[xref_from.selectedIndex].text;
    var xref_to = document.getElementById('xrefTo');
    xref_to.options.length = 1;
    var xref_array = new Array();
    for (var cat in xrefs[xref_from_selected]) {
        xref_array[xref_array.length] = String(cat);
    }

    xref_array.sort();
    for (var k in xref_array) {
        xref_to.options[xref_to.options.length] = new Option(xref_array[k], xref_array[k]);
    }
}

function computeXref(x) {
    var ret = new Array();
    var empty = [];
    for (var ix in x) {
        var xx = x[ix];
        if ("istatd_categories" in xx) {
           var categories = xx["istatd_categories"].split(",");
           for(var cat in categories) {
               if (!(categories[cat] in ret)) {
                   ret[categories[cat]] = new Array();
               }
               for (var cat2 in categories) {
                   if (!(categories[cat2] in ret[categories[cat]])) {
                       ret[categories[cat]][categories[cat2]] = 1;
                   }
               }
           }
        }
    }
    for (var k in ret) {
       delete ret[k][k];
       ret[k].sort();
    }
    return ret;
}

function getGraphedCounters()
{
    var all_counters = new Array();
    for (var k in theShittyGlobals.theGrid._allGraphs) {
        graph = theShittyGlobals.theGrid._allGraphs[k]
        for (var path in graph._series) {
            all_counters[path] = 1;
        }
    }
    return(util.arrayKeys(all_counters));
}


//
//////////////////////////////

function done() {
    console.log('done');
}

var nesting = 0;

function begin(label) {
    console.log("progress begin " + (label == undefined ? ":" : label + ":"), nesting);
    var $container;
    var $progress;

    $container = $('div#progressbar');
    $progress = $('.progressfg', $container);

    if (nesting == 0) {
        $container.css('visibility', 'visible');
        $progress.animate({width: $container.width() / 2}, 500.0, 'linear', function() {
            $container.css('visibility', 'visible');
        });
    }

    nesting = nesting + 1;

    return function() {
        if (nesting > 0) {
            nesting -= 1;
            if (nesting == 0) {
                $container.css('visibility', 'visible');
                $progress.animate({width: $container.width()}, 500.0, 'linear', function() {
                    $progress.css({width: 0});
                    $container.css('visibility', 'hidden');

                    $progress = null;
                });
            }
        }
    }
}

function arrayRemove(a, e) {
    for (var k in a) {
        if (a[k] == e) {
            a.splice(k, 1);
            break;
        }
    }
}

function arrayFindIndex(a, e) {
    for (var k in a) {
        if (a[k] == e) {
            return k
        }
    }
}

function objDump(arg) {
    var ret = "" + typeof(arg) + "{\n";
    ret += util.keys(arg).join(",\n");
    ret = ret + "\n}";
    return ret;
}

function twoDigit(val) {
    if (val < 0) return '00';
    val = val % 100;
    if (val < 10) return '0' + Math.floor(val);
    return Math.floor(val);
}

function strftime(fmt, date) {
    var keys = {
        '%a': ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'][date.getDay()],
        '%d': twoDigit(date.getDate()),
        '%e': date.getDate(),
        '%b': ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'][date.getMonth()],
        '%m': twoDigit(date.getMonth()),
        '%y': twoDigit(date.getFullYear()),
        '%Y': date.getFullYear(),
        '%H': twoDigit(date.getHours()),
        '%M': twoDigit(date.getMinutes()),
        '%S': twoDigit(date.getSeconds())
    };
    for (var k in keys) {
        fmt = fmt.replace(new RegExp(k, 'g'), keys[k]);
    }
    return fmt;
}


function loadSettings(scope, filter, success, error) {
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = util.guard(function() {
        if (xhr.readyState == 4) {
            var json = JSON.parse(xhr.responseText || "{}");
            if (xhr.status > 299) {
                console.log('loadSettings error ' + xhr.status + ' ' + xhr.statusText);
                if (error) {
                    error(json);
                }
                return;
            }
            success(json);
        }
    });
    xhr.open('GET', "/settings?s=" + encodeURIComponent(scope) + "&sk=" +
        encodeURIComponent(filter), true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.send();
}

function loadTemplates(success, error) {
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = util.guard(function() {
        if (xhr.readyState == 4) {
            var json = JSON.parse(xhr.responseText || "{}");
            if (xhr.status > 299) {
                console.log('loadTemplates error ' + xhr.status + ' ' + xhr.statusText);
                if (error) {
                    error(json);
                }
                return;
            }
            success(json);
        }
    });
    xhr.open('GET', "/templates", true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.send();
}

function loadTemplate(name, success, error) {
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = util.guard(function() {
        if (xhr.readyState == 4) {
            var json = JSON.parse(xhr.responseText || "{}");
            if (xhr.status > 299) {
                console.log('loadTemplates error ' + xhr.status + ' ' + xhr.statusText);
                if (error) {
                    error(json);
                }
                return;
            }
            success(json);
        }
    });
    xhr.open('GET', "/template/" + name, true);
    xhr.setRequestHeader("Content-Type", "application/json");
    xhr.send();
}

function zoomOutIntervals(shittyGlobals) {
    shittyGlobals.theGrid.$self.find('canvas').css('opacity', 0.5);
    util.zoomOutIntervals(shittyGlobals);
    guiRefresh(shittyGlobals);
 }


function DashboardList(id, owner) {
    this.$self = $('#' + id);
    this.$inner = $('.picklist', this.$self);
    this.itemsByName = {};
    this.currentDashboard = name;
    new Widget(this, this.$self, owner);
}
DashboardList.prototype.reload = function DashboardList_reload() {
    console.log('DashboardList.reload() ' + theUserName);
    var self = this;
    $('li', self.$inner).remove();
    if (theUserName) {
        loadSettings(theUserName, 'dashboard.*',
            function(json) {
              self.on_userDashboards(theUserName, json);
            },
            function(json) {
              self.on_userDashboards(theUserName, {});
            }
        );
    }
    else {
        self.on_userDashboards(theUserName, {});
    }

}
DashboardList.prototype.setCurrentDashboard = function DashboardList_setCurrentDashboard(name) {
    this.currentDashboard = name;
    this.updateCurrentDashboard();
}
DashboardList.prototype.updateCurrentDashboard = function DashboardList_updateCurrentDashboard() {
    this.$inner.find('li').removeClass('current');
    if(!this.currentDashboard) {
        return;
    }
    var $li = this.itemsByName[this.currentDashboard];
    if($li) {
        $li.addClass('current');
    }
}
DashboardList.prototype.on_userDashboards = function DashboardList_onUserDashboards(ctx, json) {
    console.log('DashboardList.on_userDashboards(' + ctx + ')');
    var self = this;
    for (var k in json) {
        var ss = k.substr(0, 10);
        if (ss == 'dashboard.') {
            var $li = $("<li class='pick'></li>");
            var ds = k.substr(10);

            $li.attr('title', ds);
            $('<span class="text"></span>').text(ds).appendTo($li);
            $remove_button = $('<span class="remove_button" title="Remove dashboard"></span>').appendTo($li);
            /*$('<div class="clear"></div>').appendTo($li);*/

            (function() {
                var name = '' + ds;
                $li.click(util.guard(function() {
                    load_dashboard({dashboard:name}, [ctx], begin());
                }));
                $remove_button.click(util.guard(function(ev) {
                    ev.stopPropagation();

                    util.choiceDialog("Are you sure sure you want to delete '" + name + "'?",
                        {'delete': 'Delete', 'no': 'Cancel'},
                        function(opt) {
                            if (opt == 'delete') {
                                saveSettings(theUserName, 'dashboard.' + name, "", function() {
                                    self.reload();
                                    if(name == self.currentDashboard) {
                                        window.location.href = window.location.href.split('#')[0];
                                    }
                                });
                            }
                        });
                }));
            }
            )();
            self.itemsByName['' + ds] = $li;
            self.$inner.append($li);
        }
    }
    if (ctx != 'global') {
        self.$inner.append($("<li class='disabled separator'></li>"));
        loadSettings('global', 'dashboard.*',
            function(json) {
              self.on_userDashboards('global', json);
              loadTemplates(function(innerjson) { self.on_templates(innerjson); } );
            },
            function(json) {
              self.on_userDashboards('global', {});
              loadTemplates(function(innerjson) { self.on_templates(innerjson); } );
            });
    }

    self.updateCurrentDashboard();
}
DashboardList.prototype.on_templates = function DashboardList_onTemplates(json) {
    console.log('DashboardList.on_templates()');
    var self = this;
    self.$inner.append($("<li class='disabled separator'></li>"));
    for (var k in json) {
        var $li = $("<li class='pick'></li>");

        $li.attr('title', k);
        $('<span class="text"></span>').text(k).appendTo($li);
        //$remove_button = $('<span class="remove_button" title="Remove dashboard"></span>').appendTo($li);
        /*$('<div class="clear"></div>').appendTo($li);*/

        (function() {
            var name = '' + k; // This is critical due to javascript closure instanity
            $li.click(util.guard(function(ev) {
                ev.stopPropagation();
                util.promptDialogN('Fill in the template',
                    json[name].replacements,
                    util.guard(function(replacements) {
                        var newRep = _.map(replacements, function(it, ke) {
                          return { value: it, replacer: json[name].replacements[ke], key: ke};
                        });
                        openTemplate(name, json[name].template, newRep, begin());
                    })
                ).addClass('select_daterange');
            }));
        }
        )();
        self.itemsByName['' + k] = $li;
        self.$inner.append($li);
    }
    //self.updateCurrentDashboard();
}

function load_template(args, contexts, cb) {
  var rest = _.map(_.omit(args, 'template'), function(n) {
      return JSON.parse(n);
    }
  );
  loadTemplate(args.template, function(innerjson) { openTemplate(args.template, innerjson[args.template].template, rest) } );
}

function openTemplate(name, template, replacements) {
    graphsJ = JSON.parse(_.reduce(replacements, function(acc, n, k) {
      var regexp = new RegExp(n.replacer, 'g');
      return acc.replace(regexp, n.value);
    }, template));
    json = { graphs : graphsJ }
    $('input#arg_filename').val(name);

    $div = $("<div class='template_tools'><div class='inputs'></div><div class='tbutton' name='done'>APPLY</div></div>");

    $('#toptab_extra #tools_extra').empty();

    $inputs = $('div.inputs', $div);

    var fldNames = [];
    for (var k in replacements) {
        var replacement = replacements[k];
        fldNames.push(replacement.key);
        $input = $("<div class='input " + replacement.key + "'><label for='" + replacement.key + "'>" + replacement.replacer +
            "</label> <input type='text' class='answer " + replacement.key + "' name='" +
            replacement.key + "' value='" + replacement.value + "'/></div> ");
        $input.appendTo($inputs);
    }

    var eHandler = util.guard(function(ev) {
        ev.stopPropagation();
        var kv = {};
        for (var k in fldNames) {
            var fn = fldNames[k];
            kv[fn] = $('.answer.' + fn, $inputs).val();
        }

        var newRep = _.map(kv, function(it, ke) {
          return { value: it, replacer: _.findWhere(replacements, { key:ke }).replacer, key: ke};
        });
        openTemplate(name, template, newRep, begin());

    });

    $div.prependTo($('#toptab_extra #tools_extra'));
    $('.tbutton', $div).click(eHandler);

    theShittyGlobals.theGrid.clear();
//    if (!_.isUndefined(json.left_tab_visibility)) {
//        if (json.left_tab_visibility) {
//            if ($('#lefttab').hasClass('closed')) {
//                theShittyGlobals.leftTabToggler();
//            }
//        }else{
//            if (!$('#lefttab').hasClass('closed')) {
//                theShittyGlobals.leftTabToggler();
//            }
//        }
//    }
//    if (!_.isUndefined(json.top_tab_visibility)) {
//        if (json.top_tab_visibility) {
//            if ($('#toptab').hasClass('closed')) {
//                theShittyGlobals.topTabToggler();
//            }
//        }else{
//            if (!$('#toptab').hasClass('closed')) {
//                theShittyGlobals.topTabToggler();
//            }
//        }
//    }
      if ($('#toptab_extra').hasClass('closed')) {
          theShittyGlobals.topTabExtraToggler();
      }
//    if (json.size) {
//        var $item = null;
//        if(json.timeSlider == 'manual') {
//            $item = null;
//        } else if(typeof(json.sizeDropdown) == 'number') {
//            $item = $('.tdropdown.chart_size .tmenuitem').eq(json.sizeDropdown);
//        }
//        graph_size(json.size.width, json.size.height, $item);
//    }
    console.log(json);
    var graphs = json.graphs;
    for (var k in graphs) {
        var nu = theShittyGlobals.theGrid.newGraph();
        if (json.formats) {
            nu._format = bars_ix_to_fmt[json.formats[k]] || "errorBars";
        }
        else {
            nu._format = "errorBars";
        }
        var serii = graphs[k];
        for (var i in serii) {
            var ser = serii[i];
            nu.toggleSeries(ser);
        }
        if (json.collapseds) {
            var collapsed = json.collapseds[k];
            if (collapsed) {
                nu._collapse();
            }
        }

    }
//    if (json.timeInterval) {
//        theShittyGlobals.theOriginalDates.start = new Date(theShittyGlobals.theOriginalDates.stop.getTime() - json.timeInterval * 1000);
//        util.restoreOriginalInterval(theShittyGlobals);
//    }
//    if(json.timeSlider == 'manual') {
//        theTimeSlider.setManual(true);
//    } else if(typeof(json.timeSlider) == 'number') {
//        theTimeSlider.setIndex(json.timeSlider);
//    }
//    if (json.events) {
//        theShittyGlobals.theGrid._loader.configureEvents(json.events);
//    }else {
//        theShittyGlobals.theGrid._loader.configureEvents({});
//    }
    util.calcReloadInterval(theShittyGlobals);
//    setAutoRefresh(json.autoRefresh);
    theShittyGlobals.theGrid.repaintAll();
    
    var qArgs = _.map(replacements, function(n) {
      return encodeURIComponent(n.key) + "=" + encodeURIComponent(JSON.stringify(n));
    });

    window.location.hash = '#?template=' + name + '&' + qArgs.join("&");
    document.title = "Paradox - Template - " + name;
    theDashboards.setCurrentDashboard(name);
}

function TabCollection(id) {
    this.$self = $('#' + id);
    new Widget(this, this.$self, null);
    this.$activetab = $('.active.tab', this.$self);
    this.rebuildTabs();
}
TabCollection.prototype.rebuildTabs = function TabCollection_rebuildTabs() {
    var $labels = $('>div.labels', this.$self);
    $('>div', $labels).remove();
    var i = 0;
    $('>div.tab', this.$self).each(function(ix, el) {
        i += 1;
        var $tab = $("<div class='tabtop'></div>");
        var $el = $(el);
        var id = $el.attr('id');
        $tab.text(id.charAt(0).toUpperCase() + id.substring(1));
        $tab.attr('title', $tab.text());

        if($el.hasClass('active')) {
            $tab.addClass('active');
        }
        $labels.append($tab);

        $tab.click(util.guard(function() {
            $('>div', $labels).removeClass('active');
            $tab.addClass('active');
            $('>div.tab', $el.parent()).removeClass('active');
            $el.addClass('active');
        }));
    });
}

function GraphGrid(id, loader) {
    this._allGraphs = [];
    this._loader = loader;
    this._showEvents = true;
    this._left_tab_visibility = true;
    this._top_tab_visibility = true;
    this._top_tab_extra_visibility = false;
    context.init({preventDoubleContext: false});
    var self = this;
    var menuItems = [
        {header: 'Grid Tools'},
        {text: 'New Empty Graph', action: function(e) {
                var s = self.newGraph();
            }
        },
        {text: 'New Graph...', action: function(e) {
                var s = self.newGraph();
                s.showManualQuery();
            }
        },
        {text: 'Toggle legend collapse', action: function(e) {
                for (var k in self._allGraphs) {
                    var graph = self._allGraphs[k];
                    graph._collapseToggle();
                }
            }
        },
        {text: 'Legend collapse', action: function(e) {
                for (var k in self._allGraphs) {
                    var graph = self._allGraphs[k];
                    graph._collapse();
                }
            }
        },
        {text: 'Legend uncollapse', action: function(e) {
                for (var k in self._allGraphs) {
                    var graph = self._allGraphs[k];
                    graph._uncollapse();
                }
            }
        },
        {text: 'Show StdDev on all graphs', action: function(e) {
                for (var k in self._allGraphs) {
                    var graph = self._allGraphs[k];
                    graph._format = 'errorBars';
                    graph.repaint();
                }
            }
        },
        {text: 'Show Lines on all graphs', action: function(e) {
                for (var k in self._allGraphs) {
                    var graph = self._allGraphs[k];
                    graph._format = 'noBars';
                    graph.repaint();
                }
            }
        },
        {text: 'Show Min/Max on all graphs', action: function(e) {
                for (var k in self._allGraphs) {
                    var graph = self._allGraphs[k];
                    graph._format = 'customBars';
                    graph.repaint();
                }
            }
        },
    ];
    context.attach($('#' + id), menuItems);
    new Widget(this, $('#' + id), null);
}
GraphGrid.prototype.newGraph = util.guard(function GraphGrid_newGraph() {
    var $ret = $("<div class='graph'><span title='Show/Hide summary' class='summarybox buttonbox'/><span title='Settings for display' class='settingsbox buttonbox'/><span title='Settings for manual query' class='querybox buttonbox'/><span title='Restore default zoom' class='zoomoutbox buttonbox'/><span title='Close' class='closebox buttonbox'/><div class='legend'/><div class='graphdiv'></div><div class='summary'></div></div>");
    $ret.width(theShittyGlobals.theGraphSize.width);

    var $graphs = $('#graphs');

    $graphs.append($ret);
    var surface = new GraphSurface($ret, theShittyGlobals.theGrid.widget, this._loader, theShittyGlobals);

    $('.legend', $ret).on( "click", "span.closeit", util.guard(function() {
        $(this).parent().unbind();
        surface.tryRemoveSeries($(this).parent().contents()[1].wholeText.slice(1))
    }));


    return surface;
})
GraphGrid.prototype.select = util.guard(function GraphGrid_select(graph) {
    $('.selected', this.$self).removeClass('selected');
    this.selected = graph;
    if (graph) {
        this.selected.$self.addClass('selected');
    }
})
GraphGrid.prototype.click = function GraphGrid_click(ev) {
    $('.selected', this.$self).removeClass('selected');
    this.selected = null;
}
GraphGrid.prototype.repaintAll = function GraphGrid_repaintAll() {
    //  don't lock up the browser for the entire time if
    //  you have lots of graphs.
    var val = 1;
    var self = this;
    for (var k in this._allGraphs) {
        (function() {
            var graph = self._allGraphs[k];
            setTimeout(function() {
                graph.repaint();
            }, val);
        })();
        val = val + 1;
    }
}
GraphGrid.prototype.reloadAll = function GraphGrid_reloadAll() {
    for (var k in this._allGraphs) {
        var graph = this._allGraphs[k];
        graph.reload();
    }
}
GraphGrid.prototype.add = function GraphGrid_add(graph) {
    this._allGraphs.push(graph);
}
GraphGrid.prototype.remove = function GraphGrid_remove(graph) {
    if (graph == this.selected) {
        this.selected = null;
    }
    arrayRemove(this._allGraphs, graph);
    if(this._allGraphs.length == 0) {
        util.restoreOriginalInterval(theShittyGlobals);
    }
}
GraphGrid.prototype.getDashboard = function GraphGrid_getDashboard() {
    var ret = {
        size: theShittyGlobals.theGraphSize,
        autoRefresh: isAutoRefresh,
        timeInterval: (theShittyGlobals.theCurrentDates.stop.getTime() - theShittyGlobals.theCurrentDates.start.getTime())/1000,
        timeSlider: theTimeSlider.isManual() ? 'manual' : theTimeSlider.getIndex(),
        sizeDropdown: typeof(theSizeDropdownValue) == 'number' ? theSizeDropdownValue : 'manual'
    };
    var graphs = [];
    var formats = [];
    var collapseds = [];
    for (var g in this._allGraphs) {
        var graph = this._allGraphs[g];
        graphs.push(graph.getSeries());
        formats.push(bars_fmt_to_ix[graph._format]);
        collapseds.push(graph._collapsed);
    }
    ret.graphs = graphs;
    ret.formats = formats;
    ret.collapseds = collapseds;
    ret.events = this._loader.getEvents();
    ret.left_tab_visibility = theShittyGlobals.theGrid._left_tab_visibility;
    ret.top_tab_visibility = theShittyGlobals.theGrid._top_tab_visibility;
    return ret;
}
GraphGrid.prototype.clear = function GraphGrid_clear() {
    this.selected = null;
    var copy = this._allGraphs.slice(0, this._allGraphs.length);
    //  close all graphs
    for (var k in copy) {
        copy[k].close();
    }
}

function HSplitter(id) {
    var $self = $('#' + id);
    this.$self = $self;

    $self.css('cursor', 'w-resize');
    var move = function(ev) {
        $('body').css('cursor', 'w-resize');

        $(document).mousemove(function(ev) {
            var x = ev.pageX;
            if(x >= 0) {
                $('#lefttab').width(x - 10);
            }
        });
        $(document).mouseup(function() {
            $('body').css('cursor', '');
            $(document).unbind('mousemove');
            $(document).unbind('mouseup');
        });
    };

    $self.mousedown(move);
    new Widget(this, $self, null);
}

function hide_lefttab(shittyGlobals) {
    old_element_width = $('#lefttab').css('width');
    $('#lefttab').css("width", "0px");
    $('#hsplit').addClass('closed');
    $('#lefttab').addClass('closed');
    $('#hide_lefttab').addClass('closed');
    shittyGlobals.theGrid._left_tab_visibility = false;
    return old_element_width;
}

function show_leftab(old_element_width, shittyGlobals) {
    $('#lefttab').css("width", old_element_width);
    $('#hsplit').removeClass('closed');
    $('#lefttab').removeClass('closed');
    $('#hide_lefttab').removeClass('closed');
    shittyGlobals.theGrid._left_tab_visibility = true;
}

function toggle_lefttab_visibility(shittyGlobals) {
    var old_element_width = "";
    return function(ev) {
        if (!_.isUndefined(ev)) {
            ev.stopPropagation();
        }
        if ($('#lefttab').hasClass('closed')) {
            show_leftab(old_element_width, shittyGlobals);
        }
        else {
            old_element_width = hide_lefttab(shittyGlobals);
        }

        shittyGlobals.theGrid.repaintAll();
    };
};

function hide_toptab(shittyGlobals) {
    old_element_height = $('#toptab').css('height');
    $('#toptab').css("height", "0px");
    $('#hsplit').addClass('closed');
    $('#toptab').addClass('closed');
    $('#hide_toptab').addClass('closed');
    shittyGlobals.theGrid._top_tab_visibility = false;
    return old_element_height;
}

function show_toptab(old_element_height, shittyGlobals) {
    $('#toptab').css("height", old_element_height);
    $('#menubar').removeClass('closed');
    $('#toptab').removeClass('closed');
    $('#hide_toptab').removeClass('closed');
    shittyGlobals.theGrid._top_tab_visibility = true;
}

function hide_toptab_extra(shittyGlobals) {
    old_element_height = $('#toptab_extra').css('height');
    $('#toptab_extra').css("height", "0px");
    $('#hsplit_extra').addClass('closed');
    $('#toptab_extra').addClass('closed');
    $('#hide_toptab_extra').addClass('closed');
    shittyGlobals.theGrid._top_tab_extra_visibility = false;
    return old_element_height;
}

function show_toptab_extra(old_element_height, shittyGlobals) {
    $('#toptab_extra').css("height", old_element_height);
    $('#menubar_extra').removeClass('closed');
    $('#toptab_extra').removeClass('closed');
    $('#hide_toptab_extra').removeClass('closed');
    shittyGlobals.theGrid._top_tab_extra_visibility = true;
}


function toggle_toptab_visibility(shittyGlobals) {
    var old_element_height = "";
    return function(ev) {
        if (!_.isUndefined(ev)) {
            ev.stopPropagation();
        }
        if ($('#toptab').hasClass('closed')) {
            show_toptab(old_element_height, shittyGlobals);
        }
        else {
            old_element_height = hide_toptab(shittyGlobals);
        }

        shittyGlobals.theGrid.repaintAll();
    };
};

function toggle_toptab_extra_visibility(shittyGlobals) {
    var old_element_height = "";
    return function(ev) {
        if (!_.isUndefined(ev)) {
            ev.stopPropagation();
        }
        if ($('#toptab_extra').hasClass('closed')) {
            show_toptab_extra(old_element_height, shittyGlobals);
        }
        else {
            old_element_height = hide_toptab_extra(shittyGlobals);
        }

        shittyGlobals.theGrid.repaintAll();
    };
};

function TimeSlider(id) {
    var self = this;
    var $self = $('#' + id);
    this.$self = $self;
    this.manual = false;
    this.$thumb = $self.find('.thumb');
    this.$gauge = $self.find('.gauge');
    this.$value = $self.find('.value');
    this.$marker_holder = $self.find('.markers');
    this.$markers = this.$marker_holder.children('.marker');

    this.width = this.$gauge.width();
    this.snap = this.width / (this.$markers.length - 1);
    this.index = null;
    this.dragging = false;

    this.$marker_holder.attr('unselectable', 'on');
    this.$markers.attr('unselectable', 'on');
    this.$markers.each(function(ix, item) {
        $(item).css('left', ix * self.snap + 2);
    });

    var dfl = $self.attr('default');
    if(dfl) {
        this.setIndex(dfl);
    }

    var move = function(ev) {
        self.move(ev);
    }

    this.$thumb.mousedown(move);
    this.$gauge.mousedown(move);
    this.$markers.mousedown(move);

    new Widget(this, $self, null);
}
TimeSlider.prototype.getIndex = function() {
    return this.index;
}
TimeSlider.prototype.setIndex = util.guard(function TimeSlider_setIndex(index) {
    index = Math.min(Math.max(index, 0), this.$markers.length - 1);
    this._moveThumb(index);
    this._update(index);
});
TimeSlider.prototype.isManual = function() {
    return this.manual;
}
TimeSlider.prototype.setManual = function(manual) {
    this.$self.toggleClass('manual', manual);
    this.manual = manual;
}
TimeSlider.prototype._moveThumb = function TimeSlider_moveThumb(index) {
    var x = index * this.snap + 4;
    this.$value.width(x + 'px');
    this.$thumb.css('left', x + 'px');
    this.setManual(false);
};
TimeSlider.prototype._update = function TimeSlider_updateIndex(index) {
    if(index != this.index) {
        var txt = this.$markers.eq(index).attr('action');
        eval(txt);
    }
    this.index = index;
}
TimeSlider.prototype._calculateIndex = function TimeSlider_updateIndex(x) {
    var index = Math.round((x - this.$gauge.offset().left) / this.snap);
    return Math.min(Math.max(index, 0), this.$markers.length - 1);
}
TimeSlider.prototype.move = function TimeSlider_move(ev) {
    this._moveThumb(this._calculateIndex(ev.pageX));
    $('body').css('cursor', 'pointer');

    if(!this.dragging) {
        this.dragging = true;

        var self = this;
        $(document).mousemove(function(ev) {
            self.move(ev);
        });
        $(document).mouseup(util.guard(function(ev) {
            $(document).unbind('mousemove');
            $(document).unbind('mouseup');

            $('body').css('cursor', '');
            self.dragging = false;

            self._update(self._calculateIndex(ev.pageX));
        }));
    }
}

function EventLoader() {
    this._nextEvent = null;
    this._nextEventTime = 0;
    this._getting = {};
    this._cbObjs = {};
    this._xhr = null;
    this._id = 0;
}


function toggleGraphing(path, graph_offset)
{
    if (typeof(graph_offset) == 'undefined') {
        if (!theShittyGlobals.theGrid.selected)
        {
            var nu = theShittyGlobals.theGrid.newGraph();
            theShittyGlobals.theGrid.select(nu);
        }
        graph = theShittyGlobals.theGrid.selected;
    } else {
        graph = theShittyGlobals.theGrid._allGraphs[graph_offset];
    }

    graph.toggleSeries(path);
    return graph;
}


var saveSettings = util.guard(function _saveSettings(scope, name, value, cb) {
    var done = begin();
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = util.guard(function _saveSettings_readyStateChange() {
        if (xhr.readyState == 4) {
            done();
            if (xhr.status > 299) {
                util.errorDialog("saveSettings error: " + xhr.status + " " + xhr.statusText +
                    "\n" + xhr.responseText);
                if (cb) {
                    cb();
                }
                return;
            }
            var json = JSON.parse(xhr.responseText);
            if (!json.success) {
                util.errorDialog("saveSettings error: \n" + json.message);
                if (cb) {
                    cb();
                }
                return;
            }
            if (cb) {
                cb();
            }
        }
    });
    xhr.open('POST', '/settings?s=' + escape(scope), true);
    xhr.setRequestHeader("Content-Type", "application/json");
    var obj = {};
    if(value === '')
    {
        obj[name] = '';
    }
    else
    {
        obj[name] = JSON.stringify(value);
    }
    xhr.send(JSON.stringify(obj));
});

function make_permalink_png()
{
    console.log('make_permalink_png()');
    var theDash = theShittyGlobals.theGrid.getDashboard();
    if (theUserName) {
        var mkLink = function() {
            makePermalinkPng(theDash, function() {
            });
        }
        mkLink();
    }
    else {
        util.errorDialog('You must log in to make permalinks');
    }
}

var makePermalinkPng = util.guard(function _makePermalinkPng(value, cb) {
    var done = begin("makePermalinkPng");
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = util.guard(function _makePermalinkPng_readyStateChange() {
        if (xhr.readyState == 4) {
            done();
            if (xhr.status > 299) {
                util.errorDialog("makePermalinkPng error: " + xhr.status + " " + xhr.statusText +
                    "\n" + xhr.responseText);
                if (cb) {
                    cb();
                }
                return;
            }
            var json = JSON.parse(xhr.responseText);
            if (!json.pngLink) {
                util.errorDialog("makePermalinkPng error: \n" + json.message);
                if (cb) {
                    cb();
                }
                return;
            }
            util.choiceDialog(
                "Link: " + json.pngLink + "",
                {'send': 'Send', 'no': 'Cancel'},
                function(js) { return function(opt) {
                    if (opt == 'send') {
                        util.promptDialogN('Send to slack webhook',
                            {
                                'channel': 'Channel:',
                                'title': 'Title:',
                                'text': 'Text:'
                            },
                            util.guard(function(vals) {
                                var send = function() {
                                    var slack = new XMLHttpRequest();
                                    slack.onreadystatechange = util.guard(function _slack_readyStateChange() {
                                        if (slack.readyState == 4) {
                                        }
                                    });
                                    slack.open('POST', "https://hooks.slack.com/services/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", true);
                                    slack.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
                                    var json = {
                                        "channel": vals.channel,
                                        "username": "paradox-bot",
                                        "attachments": [
                                            {
                                                "fallback": vals.text,
                                                "title": vals.title,
                                                "title_link": js.pngLink,
                                                "text": vals.text,
                                                "image_url": js.pngLink,
                                                "color": "#764FA5"
                                            }
                                        ]
                                    };
                                    var data = "payload=" + encodeURIComponent(JSON.stringify(json));
                                    slack.send(data);
                                }

                                var donePost = begin("isImgReady");
                                var isImgReady = new XMLHttpRequest();
                                isImgReady.onreadystatechange = util.guard(function _isImgReady_readyStateChange() {
                                    if (isImgReady.readyState == 4) {
                                        if (isImgReady.status == 200) {
                                            send();
                                            donePost();
                                        }

                                        if (isImgReady.status > 299) {
                                            util.errorDialog("makePermalinkPng error: " + isImgReady.status + " " + isImgReady.statusText +
                                                "\n" + isImgReady.responseText);
                                            donePost();
                                        }
                                    }
                                });
                                isImgReady.open('GET', js.pngLink, true);
                                isImgReady.send();
                                return true;
                            })
                        );
                    }
                } } (json)
            );

            if (cb) {
                cb();
            }
        }
    });
    xhr.open('POST', '/permalink', true);
    xhr.setRequestHeader("Content-Type", "application/json");
    var obj = {};
    obj.maxSamples = 1000;
    obj.start = Math.floor(theShittyGlobals.theCurrentDates.start.getTime()/1000);
    obj.stop = Math.floor(theShittyGlobals.theCurrentDates.stop.getTime()/1000);
    var graphs = {};
    _.map(value.graphs, function (value, key) {
         graphs[key] = {
            'keys' : value,
            'drawOptions' : {}
         };
    });
    obj.graphs = graphs;

    xhr.send(JSON.stringify(obj));
});

var emailUser = util.guard(function _saveSettings(fn, value, pngOrSvg, cb) {
    var done = begin();
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = util.guard(function _emailUser_readyStateChange() {
        if (xhr.readyState == 4) {
            done();
            if (xhr.status > 299) {
                util.errorDialog("emailUser error: " + xhr.status + " " + xhr.statusText +
                    "\n" + xhr.responseText);
                if (cb) {
                    cb();
                }
                return;
            }
            var json = JSON.parse(xhr.responseText);
            if (!json.success) {
                util.errorDialog("emailUser error: \n" + json.message);
                if (cb) {
                    cb();
                }
                return;
            }
            if (cb) {
                cb();
            }
        }
    });
    if (pngOrSvg === true) {
        xhr.open('POST', '/emailpng', true);
    }else{
        xhr.open('POST', '/email', true);
    }
    xhr.setRequestHeader("Content-Type", "application/json");
    var obj = {};
    obj.maxSamples = 1000;
    obj.email = fn;
    obj.start = Math.floor(theShittyGlobals.theCurrentDates.start.getTime()/1000);
    obj.stop = Math.floor(theShittyGlobals.theCurrentDates.stop.getTime()/1000);
    var graphs = {};
    _.map(value.graphs, function (value, key) {
         graphs[key] = {
            'keys' : value,
            'drawOptions' : {}
         };
    });
    obj.graphs = graphs;

    xhr.send(JSON.stringify(obj));
});

var theUser = null;
var theUserName = null;
var theTabs = null;
var theCounters = null;
var theDashboards = null;
var theSplitter = null;
var theEventLoader = null;
var theTimeSlider = null;
var theSizeDropdownValue = null;

var tmpDate = util.getAdjustedNow();
var tmpStart = new Date(tmpDate.getTime() - 3600*1000);

var theShittyGlobals = {
        'theInteractionModel' : null,
        'theGraphSize' : { width: 600, height: 240 },
        'theGraphMaxSamples' : null,
        'theCurrentDates' : { 'start': tmpStart, 'stop': tmpDate },
        'pageIsHidden' : pageIsHidden,
        'theReloadInterval' : 10000,
        'theOriginalDates' : { 'start': tmpStart, 'stop': tmpDate },
        'theGrid' : null,
        'zoomOutIntervals' : zoomOutIntervals,
    };

function setGraphSize(sz)
{
    theShittyGlobals.theGraphSize = sz;
    $('#grid .graph').width(sz.width);
    $('#grid .graphdiv').css(sz);
    theShittyGlobals.theGrid.repaintAll();
}

function set_interval(i) {
    theShittyGlobals.theOriginalDates.start = new Date(theShittyGlobals.theOriginalDates.stop.getTime() - i * 1000);
    theShittyGlobals.theGraphMaxSamples = undefined;
    util.restoreOriginalInterval(theShittyGlobals);
    util.calcReloadInterval(theShittyGlobals);
    guiRefresh(theShittyGlobals);
}

function graph_size(w, h, $item) {
    $('.tdropdown.chart_size .tmenuitem').removeClass('current');
    theSizeDropdownValue = null;
    if($item) {
        $item.addClass('current');
        theSizeDropdownValue = $item.parent().children().index($item);
    }
    setGraphSize({width: w, height: h});
}

function size_setCustom(sizeStr)
{
    var a = sizeStr.split('x', 2);
    if (a.length != 2) {
        a = sizeStr.split(',');
    }
    if (a.length != 2) {
        util.errorDialog("Please specify size as WxH (or W,H).");
        return false;
    }
    var wStr = a[0].replace(/[^0-9]*/g, '');
    var hStr = a[1].replace(/[^0-9]*/g, '');
    if (wStr == '' || hStr == '') {
        util.errorDialog("Please specify integer width and height as WxH (or W,H).");
        return false;
    }
    var w = parseInt(wStr);
    var h = parseInt(hStr);
    if (w < 10 || w > 4000 || h < 10 || h > 2000) {
        util.errorDialog("Please specify width and height within reasonable ranges.");
        return false;
    }
    setGraphSize({width: w, height: h});
    $('.tdropdown.chart_size .tmenuitem').removeClass('current');
    return true;
}

function size_custom()
{
    promptDialog('Please enter a graph size: (WxH)', size_setCustom);
}

function change_counters(from_to, keep_old)
{
    var keep = 0;
    if (typeof(keep_old) != 'undefined') {
        keep = keep_old
    }

    var ft = from_to.split(":",2);
    for (var k in theShittyGlobals.theGrid._allGraphs) {
        graph = theShittyGlobals.theGrid._allGraphs[k]
        for (var path in graph._series) {
            if (path.indexOf(ft[0]) >= 0) {
                var new_path = path.replace(ft[0], ft[1]);
                graph.toggleSeries(new_path);
                if (keep == 0) {
                    graph.toggleSeries(path);
                }
            }
        }
        graph.reload();
    }
}

function add_counters(match_add)
{
    change_counters(match_add, 1);
}

function regexp_change()
{
    promptDialog('Match counters, and replace them with another (can be substring).\n[Syntax: match:substitution]', change_counters);
}

function regexp_add()
{
    promptDialog('Match counters, and add their substitution to same graphs (can be substring).\n[Syntax: match:substitution]', add_counters);
}

function save_dashboard_named(name)
{
    var fn = name;
    console.log('save_dashboard(' + fn + ')');
    if (!fn || fn.match(/[^a-zA-Z_0-9]/)) {
        util.errorDialog("Please enter a proper file name first. Alphanumeric and underscore characters only.");
        return;
    }
    var theDash = theShittyGlobals.theGrid.getDashboard();
    if (theUserName) {
        var save = function() {
            saveSettings(theUserName, 'dashboard.' + fn, theDash, function() {
                theDashboards.reload();
            });
            window.location.hash = '#?dashboard=' + fn;
        }
        if(theDashboards.itemsByName[fn]) {
            util.choiceDialog("Dashboard named '" + fn + "' already exists. Overwrite it?",
                {'save': 'Save', 'no': 'Cancel'},
                function(opt) {
                    if (opt == 'save') {
                        save();
                    }
                }
            );
        } else {
            save();
        }
    }
    else {
        util.errorDialog('You must log in to save dashboards');
    }
}

function save_dashboard()
{
    var $div = promptDialog(
        'Save dashboard as: [Valid characters: A-Z, a-z, 0-9, _]',
        save_dashboard_named
    );
    var $answer = $('.answer', $div);
    var dash = theDashboards.currentDashboard;
    if(dash) {
        $answer.val(dash);
    }
    $answer.select();
}

function email_user_named(name)
{
    var fn = name;
    console.log('email_user_named(' + fn + ')');
    if (!fn || fn.match(/[^a-zA-Z_0-9@.+]/)) {
        util.errorDialog("Please enter a proper name first. Alphanumeric and underscore and +@. characters only.");
        return;
    }
    var theDash = theShittyGlobals.theGrid.getDashboard();
    if (theUserName) {
        var email = function() {
            emailUser(fn, theDash, false, function() {
            });
        }
        email();
    }
    else {
        util.errorDialog('You must log in to email users');
    }
}
function email_user()
{
    var $div = promptDialog(
        'email a user the following graphs',
        email_user_named
    );
    var $answer = $('.answer', $div);
    var dash = theDashboards.currentDashboard;
    if(dash) {
        $answer.val(dash);
    }
    $answer.select();
}

function email_png_user_named(name)
{
    var fn = name;
    console.log('email_png_user_named(' + fn + ')');
    if (!fn || fn.match(/[^a-zA-Z_0-9@.+]/)) {
        util.errorDialog("Please enter a proper name first. Alphanumeric and underscore and +@. characters only.");
        return;
    }
    var theDash = theShittyGlobals.theGrid.getDashboard();
    if (theUserName) {
        var email = function() {
            emailUser(fn, theDash, true, function() {
            });
        }
        email();
    }
    else {
        util.errorDialog('You must log in to email users');
    }
}
function email_png_user()
{
    var $div = promptDialog(
        'email a user the following graphs png',
        email_png_user_named
    );
    var $answer = $('.answer', $div);
    var dash = theDashboards.currentDashboard;
    if(dash) {
        $answer.val(dash);
    }
    $answer.select();
}
function save_template()
{
    var fn = $('#arg_filename').text();
    if (!fn || fn.match(/[^a-zA-Z_0-9]/)) {
        util.errorDialog("Please enter a proper file name first. Alphanumeric and underscore characters only.");
        return;
    }
    var theDash = theShittyGlobals.theGrid.getDashboard();
    if (theUser) {
        saveSettings(theUser, 'template.' + fn, theDash);
    }
    else {
        util.errorDialog('You must log in to save templates');
    }
    window.location.hash = '#?template=' + fn;
}

var isAutoRefresh = false;
var autoRefreshTimer = null;

function setAutoRefresh(ref) {
    console.log('setAutoRefresh interval=' + theShittyGlobals.theReloadInterval);
    if (autoRefreshTimer) {
        clearInterval(autoRefreshTimer);
        autoRefreshTimer = null;
    }
    isAutoRefresh = ref;
    if (isAutoRefresh) {
        $('.tbutton.auto_refresh').addClass('toggle');
        autoRefreshTimer = setInterval(_.partial(guiRefresh, theShittyGlobals), theShittyGlobals.theReloadInterval*1000);
    }
    else {
        $('.tbutton.auto_refresh').removeClass('toggle');
    }
}

function auto_refresh() {
    if (isAutoRefresh) {
        setAutoRefresh(0);
    }
    else {
        util.calcReloadInterval(theShittyGlobals);
        setAutoRefresh(true);
    }
}

//  Also look at load_hash(), the other way to get state in.
function openDashboard(scope, dashboard, json) {
    json = JSON.parse(json["dashboard." + dashboard]);
    $('input#arg_filename').val(dashboard);
    theShittyGlobals.theGrid.clear();
    if (!_.isUndefined(json.left_tab_visibility)) {
        if (json.left_tab_visibility) {
            if ($('#lefttab').hasClass('closed')) {
                theShittyGlobals.leftTabToggler();
            }
        }else{
            if (!$('#lefttab').hasClass('closed')) {
                theShittyGlobals.leftTabToggler();
            }
        }
    }
    if (!_.isUndefined(json.top_tab_visibility)) {
        if (json.top_tab_visibility) {
            if ($('#toptab').hasClass('closed')) {
                theShittyGlobals.topTabToggler();
            }
        }else{
            if (!$('#toptab').hasClass('closed')) {
                theShittyGlobals.topTabToggler();
            }
        }
    }
    if (json.size) {
        var $item = null;
        if(json.timeSlider == 'manual') {
            $item = null;
        } else if(typeof(json.sizeDropdown) == 'number') {
            $item = $('.tdropdown.chart_size .tmenuitem').eq(json.sizeDropdown);
        }
        graph_size(json.size.width, json.size.height, $item);
    }
    console.log(json);
    var graphs = json.graphs;
    for (var k in graphs) {
        var nu = theShittyGlobals.theGrid.newGraph();
        if (json.formats) {
            nu._format = bars_ix_to_fmt[json.formats[k]] || "errorBars";
        }
        else {
            nu._format = "errorBars";
        }
        var serii = graphs[k];
        for (var i in serii) {
            var ser = serii[i];
            nu.toggleSeries(ser);
        }
        if (json.collapseds) {
            var collapsed = json.collapseds[k];
            if (collapsed) {
                nu._collapse();
            }
        }

    }
    if (json.timeInterval) {
        theShittyGlobals.theOriginalDates.start = new Date(theShittyGlobals.theOriginalDates.stop.getTime() - json.timeInterval * 1000);
        util.restoreOriginalInterval(theShittyGlobals);
    }
    if(json.timeSlider == 'manual') {
        theTimeSlider.setManual(true);
    } else if(typeof(json.timeSlider) == 'number') {
        theTimeSlider.setIndex(json.timeSlider);
    }
    if (json.events) {
        theShittyGlobals.theGrid._loader.configureEvents(json.events);
    }else {
        theShittyGlobals.theGrid._loader.configureEvents({});
    }
    util.calcReloadInterval(theShittyGlobals);
    setAutoRefresh(json.autoRefresh);
    theShittyGlobals.theGrid.repaintAll();

    window.location.hash = '#?dashboard=' + dashboard;
    document.title = "Paradox - Dashboard - " + dashboard;
    theDashboards.setCurrentDashboard(dashboard);
}

function load_dashboard(args, contexts, cb) {
    if (!contexts || !contexts.length) {
        util.errorDialog('Error loading dashboard');
        cb();
        return;
    }
    var ctx = contexts.shift();
    console.log('load_dashboard ' + ctx + ' ' + JSON.stringify(args));
    loadSettings(ctx, 'dashboard.' + args.dashboard,
        function(json) {
            openDashboard(ctx, args.dashboard, json);
            cb();
        },
        function(e) {
            load_dashboard(args, contexts, cb);
        });
}


var bars_ix_to_fmt = {
    1: 'noBars',
    2: 'errorBars',
    3: 'customBars',
    4: 'stacked',
    5: 'area'
};

var bars_fmt_to_ix = {
    'noBars': 1,
    'errorBars': 2,
    'customBars': 3,
    'stacked': 4,
    'area': 5
};


function load_state(state, _ctx, cb) {
    //  clear windows
    theShittyGlobals.theGrid.clear();
    //  load counter sets
    var graphs = state.graphs.split(';');
    for (var k in graphs) {
        var vals = graphs[k].split(',');
        var graph_ix = vals[0];
        var fmt = bars_ix_to_fmt[vals[1]] || "errorBars";
        var nu = theShittyGlobals.theGrid.newGraph();
        nu._format = fmt;
        var serii = state[graph_ix].split(';');
        for (var i in serii) {
            var ser = serii[i];
            nu.toggleSeries(ser);
        }
    }
    //  set time range
    resetDateRange(new Date(parseInt(state['from'])*1000), new Date(parseInt(state['to'])*1000));
    //  I'm done!
    cb();
}

//  Also look at openDashboard(), the other way to get state in.
var load_hash = util.guard(function _load_hash(hash) {
    var keyvals = hash.split('&');
    var func = null;
    var args = {};
    for (var ki in keyvals) {
        var kv = keyvals[ki].split('=', 2);
        var key = kv[0];
        var value = kv[1];
        if (key == 'dashboard') {
            func = load_dashboard;
        }
        if (key == 'state') {
            func = load_state;
        }
        if (key == 'template') {
            func = load_template;
        }
        args[key] = unescape(value);
    }
    var contexts = ['global'];
    if (theUserName) {
        contexts.push(theUserName);
    }
    if (func) {
        var end = begin();
        func(args, contexts, end);
    }
});

function maybeParseCookie(str, cb) {
    console.log('maybeParseCookie', str);
    var cookies = str.split(';');

    for(var i = 0; i < cookies.length; i++) {
        var cookie = cookies[i].split('=');
        if(cookie[0].match(/^\s*login\s*$/)) {
            var login = decodeURIComponent(cookie[1]).split(':');
            doLogin(login[0], null, login[1], cb);
            return;
        }
    }

    $('#login').slideDown();
    cb();
}

function gotohash() {
    var hash = window.location.hash;
    if (hash && hash.substr(0, 2) == '#?') {
        console.log('loading window.location.hash ' + hash);
        load_hash(hash.substr(2));
    } else if (hash && hash.substr(0, 2) == '#@') {
        console.log('loading window.location.hash ' + hash);
        var end = begin();
        load_json(hash.substr(2));
        end();
    }
    theShittyGlobals.theGrid.repaintAll();
}

function packup_state_as_hash() {
    var state = {};
    var theDash = theShittyGlobals.theGrid.getDashboard();
    var graphs = [];
    for (var k in theDash.graphs) {
        var sers = theDash.graphs[k].join(';');
        state[k] = sers;
        var grix = '' + k + ',' + theDash.formats[k];
        graphs.push(grix);
    }
    state.graphs = graphs.join(';');
    var qstr = '#?';
    for (var k in state) {
        qstr += escape(k) + '=' + escape(state[k]) + '&';
    }
    qstr += 'from=' + escape(Math.floor(theShittyGlobals.theCurrentDates.start.getTime()/1000)) + '&';
    qstr += 'to=' + escape(Math.floor(theShittyGlobals.theCurrentDates.stop.getTime()/1000)) + '&';
    qstr += 'state=';
    document.location.hash = qstr;
}

function packup_state_as_json() {
    var state = {};
    var theDash = theShittyGlobals.theGrid.getDashboard();
    state.graphs = {};
    for (var k in theDash.graphs) {
        var sers = theDash.graphs[k];
        state.graphs[k] = {
            series: sers,
            format: theDash.formats[k],
            collapsed: theDash.collapseds[k]
        };
    }
    state.from = escape(Math.floor(theShittyGlobals.theCurrentDates.start.getTime()/1000));
    state.to = escape(Math.floor(theShittyGlobals.theCurrentDates.stop.getTime()/1000));
    state.events = this.loader.getEvents();
    state.size = theShittyGlobals.theGraphSize;
    state.timeSlider = theTimeSlider.isManual() ? 'manual' : theTimeSlider.getIndex();
    state.sizeDropdown = typeof(theSizeDropdownValue) == 'number' ? theSizeDropdownValue : 'manual';
    state.left_tab_visibility = theDash.left_tab_visibility;
    state.top_tab_visibility = theDash.top_tab_visibility;
    state.top_tab_extra_visibility = theDash.top_tab_extra_visibility;
    var qstr = '#@';
    qstr += encodeURIComponent(JSON.stringify(state));
    document.location.hash = qstr;
}

function load_json(stateHash) {
    //  clear windows
    theShittyGlobals.theGrid.clear();
    //  load counter sets
    var state = JSON.parse(decodeURIComponent(stateHash));
    if (!_.isUndefined(state.left_tab_visibility)) {
        if (state.left_tab_visibility) {
            if ($('#lefttab').hasClass('closed')) {
                theShittyGlobals.leftTabToggler();
            }
        }else{
            if (!$('#lefttab').hasClass('closed')) {
                theShittyGlobals.leftTabToggler();
            }
        }
    }

    if (!_.isUndefined(state.top_tab_visibility)) {
        if (state.top_tab_visibility) {
            if ($('#toptab').hasClass('closed')) {
                theShittyGlobals.topTabToggler();
            }
        }else{
            if (!$('#toptab').hasClass('closed')) {
                theShittyGlobals.topTabToggler();
            }
        }
    }

    if (!_.isUndefined(state.top_tab_extra_visibility)) {
        if (state.top_tab_extra_visibility) {
            if ($('#toptab_extra').hasClass('closed')) {
                theShittyGlobals.topTabExtraToggler();
            }
        }else{
            if (!$('#toptab_extra').hasClass('closed')) {
                theShittyGlobals.topTabExtraToggler();
            }
        }
    }


    if (state.size) {
        var $item = null;
        if(state.timeSlider == 'manual') {
            $item = null;
        } else if(typeof(state.sizeDropdown) == 'number') {
            $item = $('.tdropdown.chart_size .tmenuitem').eq(state.sizeDropdown);
        }
        graph_size(state.size.width, state.size.height, $item);
    }

    for (var k in state.graphs) {
        var graph = state.graphs[k];
        var graph_ix = k;
        var fmt = bars_ix_to_fmt[graph.format] || "errorBars";
        var nu = theShittyGlobals.theGrid.newGraph();
        nu._format = fmt;
        var serii = graph.series;
        for (var i in serii) {
            var ser = serii[i];
            nu.toggleSeries(ser);
        }
        var collapsed = graph.collapsed;
        if (collapsed) {
            nu._collapse();
        }
    }
    theShittyGlobals.theGrid._loader.configureEvents(state.events);
    //  set time range
    resetDateRange(new Date(parseInt(state['from'])*1000), new Date(parseInt(state['to'])*1000));
}
/*
 * work in progress. migrating to the use of backbone, underscorejs and jquery-cookie
 */

var CounterTreeModel = Backbone.Model.extend({
    initialize: function() {
        this.fetch();
    },

    defaults: {
        "pattern": "*",
        "matching_names": []
    },

    url: function() {
        return "/counters/" + this.get("pattern");
    },
});

function asCounterTree(counters) {
    function newNode(text, children, type) {
        return {
            'text':     _.isUndefined(text)     ? '#' : text,
            'children': _.isUndefined(children) ? {}  : children,
            'type':     _.isUndefined(type)     ? 2   : type
        }
    }

    // convert list of counters into a tree
    var parsed_counters = newNode();
    _.each(counters, function(counter, index) {
        var fields = counter.name.split(".");
        var node = parsed_counters;
        _.each(fields, function(field, index) {
            if (!node.children[field]) {
                node.children[field] = newNode(field);
            }
            node = node.children[field];
        })
        // store counter type in lowest descendant
        node.type = counter.type;
    })

    // collapse nodes that have only 1 child
    function maybeMoveUp(subtree, key, tree) {
        collapse(subtree);
        if (_.size(subtree.children) == 1) {
            var child_key  = _.keys(subtree.children)[0];
            var child_node = subtree.children[child_key];
            var new_key    = key + '.' + child_key;

            tree[new_key] = newNode(new_key, child_node.children, child_node.type)

            delete tree[key];
        }
    }

    function collapse(counters) {
        _.each(counters.children, maybeMoveUp);
    }

    collapse(parsed_counters);

    return parsed_counters;
}

function asCounterTree2(counters, pat) {
    function newNode(text, children, type) {
        return {
            'text':     _.isUndefined(text)     ? '#' : text,
            'children': _.isUndefined(children) ? {}  : children,
            'type':     _.isUndefined(type)     ? 2   : type
        }
    }

    // convert list of counters into a tree
    var parsed_counters = newNode();

    var traverse = function(prefix, parentN, counter, index) {
        if (! _.isUndefined(counter.c)) {
            var thisLevel = newNode(index)
            if (!parentN.children[index]) {
                parentN.children[index] = thisLevel;
            }
            _.each(counter.c, _.partial(traverse, prefix.concat([index]), thisLevel));

            if (_.size(thisLevel.children) == 0) {
                delete parentN.children[index];
            }
        }

        var ctr = prefix.concat([index]).join(".");
        if (ctr.search(pat) != -1) {
            var leaf = newNode(index, {}, counter.t)
            if(!parentN.children[index]) {
                parentN.children[index] = leaf;
            }
        }
    };

    _.each(counters, _.partial(traverse, [], parsed_counters));
    // collapse nodes that have only 1 child
    function maybeMoveUp(subtree, key, tree) {
        collapse(subtree);
        if (_.size(subtree.children) == 1) {
            var child_key  = _.keys(subtree.children)[0];
            var child_node = subtree.children[child_key];
            var new_key    = key + '.' + child_key;

            tree[new_key] = newNode(new_key, child_node.children, child_node.type)

            delete tree[key];
        }
    }

    function collapse(counters) {
        _.each(counters.children, maybeMoveUp);
    }

    collapse(parsed_counters);

    return parsed_counters;
}

function formatTreeLevel(tree) {
    var icons = {
        0 : 'istatd-gauge',
        1 : 'istatd-counter',
    };

    return _.map(tree.children, function (subtree, key, tree) {
        if (_.size(subtree.children) > 0) {
            return {text: key, children: true};
        }
        else {
            return {text: key, a_attr : {'class': icons[subtree.type]}};
        }
    });
}

var CounterTreeFilterView = Backbone.View.extend({
    initialize: function() {
        var div = $("<div id='counter_filter' />");
        this.icon = $("<div class='magnifying_glass' />");
        this.label = $("<div id='counter_filter_label'>Search</div>");
        this.input = $("<input type='text' id='counter_filter_text' value=''/>");
        this.clear = $("<div id='counter_filter_clear' />");

        div.append(this.label);
        div.append(this.input);
        div.append(this.icon);
        div.append(this.clear);
        this.$el.append(div);
    },

    events: {
        "click #counter_filter_label"  : "label_click",
        "focus #counter_filter_text"   : "input_focus",
        "blur #counter_filter_text"    : "input_blur",
        "keydown #counter_filter_text" : "input_keydown",
        "click #counter_filter_clear"  : "clear_filter",
    },

    label_click: function() {
        this.input.focus();
    },

    input_focus: function() {
        this.label.hide();
    },

    input_blur: function() {
        if (!this.input.val()) {
            this.label.show();
        }
    },

    input_keydown: function(ev) {
        if (ev.which == 13) {
            $('#counter_jstree').jstree('deselect_all');
            this.model.set("pattern", this.input.val());
        }
    },

    clear_filter: function(ev) {
        this.input.val("");
        this.label.show();
        this.model.set("pattern", "");
    }
});

var CounterTreeView = Backbone.View.extend({
    el: '#counters',

    initialize: function() {
        this.counterTreeModel = new CounterTreeModel();

        var tree_filter_view = new CounterTreeFilterView({el: this.$el, model: this.counterTreeModel});

        var scroll_area = $("<div class='scroll' />");
        var jstree_view = $("<div id='counter_jstree' />");

        this.$el.append(scroll_area.append(jstree_view));

        this.listenTo(this.counterTreeModel, 'change', this.render);

    },

    render: function() {
        var proto_pat = this.counterTreeModel.get("pattern");

        var pat;
        if (proto_pat == "*") {
            pat = /.*/;
        }
        else {
            if (proto_pat.indexOf("*") == -1) {
                /* auto wildcard bare words */
                proto_pat = ".*" + proto_pat + ".*";
            }
            else {
                /* convert wildcard characters to regex */
                var subpatterns = proto_pat.split("*");
                proto_pat = subpatterns.join(".*");
            }
            pat = new RegExp(proto_pat);
        }

        // filter counters client side
        var matching_names = this.counterTreeModel.get("matching_names");
        var counters = matching_names;

        var counter_tree = asCounterTree2(counters, pat);

        function findNodeInCounterTree(counter_tree, id, parents) {

            // return the node of our counter tree that is associated with the jstree
            // node that was just opened.
            //
            // id is the DOM id of the opened node.  parents is the list of DOM ids
            // from the opened node back up the tree to the root.  We don't need
            // the jstree '#' id for the root of the tree, so we discard it with
            // slice()

            var path = _.clone(parents).reverse().concat(id).slice(1);
            var node = counter_tree;
            _.each(path, function(id) {
                node = node.children[$('#counter_jstree').jstree('get_node', id).text];
            });
            return node;
        };

        function getCounterName(id, parents) {
            var path = _.clone(parents).reverse().concat(id).slice(1);
            return _.map(path, function(id) {
                return $('#counter_jstree').jstree('get_node', id).text;
            }).join('.');
        }

        function getCounterNameSimple(parents) {
            var path = _.clone(parents).reverse().slice(1);
            return _.map(path, function(id) {
                return $('#counter_jstree').jstree('get_node', id).text;
            }).join('.');
        }


        // function call order matters here.  you must refresh jstree before you destroy
        $('#counter_jstree').jstree('refresh').jstree('destroy');

        var menuItems = [
                {text: 'Add globbed', action: function(e, ctx) {
                        var node = $('#counter_jstree').jstree('get_node', ctx.context.id);
                        var counter = getCounterNameSimple(node.parents);
                        toggleGraphing(counter + '.*');
                    }
                },
                {text: 'Add and edit', action: function(e, ctx) {
                        var node = $('#counter_jstree').jstree('get_node', ctx.context.id);
                        var counter = getCounterName(node.id, node.parents);
                        var g = toggleGraphing(counter);
                        g.showManualQuery();
                    }
                }

            ];
        $('#counter_jstree')
            .on('select_node.jstree', function (e, data) {
                if ("class" in data.node.a_attr) {
                    var counter = getCounterName(data.node.id, data.node.parents);
                    toggleGraphing(counter);
                }
                else {
                    if ($('#counter_jstree').jstree('is_open', data.node)) {
                        $('#counter_jstree').jstree('close_node', data.node);
                    }
                    else {
                        $('#counter_jstree').jstree('open_node', data.node);
                    }
                }
             })
            .on('after_open.jstree', function (e, data) {
                context.attach('.jstree-leaf', menuItems);
            })
            .jstree({
                'core': {
                    'data': function(node, cb) {
                        if (node.id == '#') {
                            cb.call(this, formatTreeLevel(counter_tree));
                        }
                        else {
                            var subtree = findNodeInCounterTree(counter_tree, node.id, node.parents);
                            cb.call(this, formatTreeLevel(subtree));
                        }
                    }
                },
                'plugins' : [ "sort" ]
            });
        context.attach('.jstree-leaf', menuItems);
        theShittyGlobals.theGrid.repaintAll();
        return this;
    }

});

var UserModel = Backbone.Model.extend({
    toJSON: function(options) {
        var data = {};
        data["user." + this.get('username')] = JSON.stringify({password: this.get('password_hash')});
        return data;
    }
});

var UsersModel = Backbone.Collection.extend({
    model: UserModel,
    url: "/settings?s=users",
    parse: function(response, options) {
        return _.map(response, function(value, key) {
            return {
                username: key.split('.')[1],
                password_hash: JSON.parse(value)['password']
            }
        });
    }
});

var SettingsModel = Backbone.Model.extend({
    initialize: function() {
        var self = this;
        this.users = new UsersModel();
        this.users.fetch({
            reset: true,
            success: function() {
               self.trigger("reset");
            }
        });
    },

    getLoggedInUser: function() {
        var login_cookie = $.cookie('login');
        if (!_.isUndefined(login_cookie)) {
            var piece = login_cookie.split(':');
            var user = this.users.findWhere({username: piece[0]});
            if (!_.isUndefined(user)) {
                return user.get('username');
            }
        }
        return null;
    },

    isLoggedIn: function() {
        return !_.isNull(this.getLoggedInUser());
    },

    createUserAndLogin: function(username, password_hash) {
        this.users.create({
            username: username,
            password_hash: password_hash
        });
        this.login(username, password_hash, true);
        //Grrrr this causes istatd to actually save the user to disk
        saveSettings(username, '', '', function () {} );
    },

    /* OK, I know -- getting the hash from the server and checking it client
       side is 100% not secure from the server's point of view. That's not
       the point. The login is only here to prevent casual impersonation, and
       to provide a unique name to store preferences under so different users
       don't step on each other.
       Really, the only reason there's a password is because it's expected ;-)
     */
    login: function(username, password, hashed) {
        var is_hashed = hashed || false;
        var password_hash = is_hashed ? password : Sha256.hash(password + ' salt ' + username);
        var user = this.users.findWhere({username: username});
        var is_new_user = _.isUndefined(user);

        if (is_new_user) {
            var self = this;
            util.choiceDialog(
                "Could not load user info -- create new user " + username + "?",
                {'create': 'Yes', 'no': 'No'},
                function(opt) {
                    if (opt == 'create') {
                        self.createUserAndLogin(username, password_hash);
                    }
                }
            );
        }
        else {
            $.cookie('login', username + ':' + password_hash, {expires: 12, path: '/'});
            this.trigger('logged_in', user);
        }

    },

    logout: function() {
        $.removeCookie('login', {expires: 12, path: '/'});
        this.trigger('logged_out');
    },

});

var UserControlsView = Backbone.View.extend({
    el: "#user_controls",

    initialize: function() {
        this.model = new SettingsModel();

        this.$login  = $('<div id="login" />');
        this.$logout = $('<div id="logout" class="tdropdown username"></div>');

        this.$el.append(this.$login);
        this.$el.append(this.$logout);

        this.userLoginView  = new UserLoginView({el: this.$login, model: this.model});
        this.userLogoutView = new UserLogoutView({el: this.$logout, model: this.model});

        this.listenTo(this.model, 'reset', function() {
            if (_.isNull(this.model.getLoggedInUser())) {
                this.showViewsWhenNobodyIsLoggedIn();
            }
            else {
                this.showViewsWhenSomebodyIsLoggedIn();
            }
        });

        this.listenTo(this.model, 'logged_in', function() {
            this.showViewsWhenSomebodyIsLoggedIn();
        });

        this.listenTo(this.model, 'logged_out', function() {
            // redirect back myself, this will reset entire UI to logged out state.
            window.location.href = window.location.href.split('#')[0];
        });
    },

    showViewsWhenNobodyIsLoggedIn: function() {
        // nobody is logged in, so show login view
        var self = this;
        this.userLogoutView.slideUp(function() {
            self.userLoginView.slideDown();
        });

        // hmm, this gotohash() still needs to pull into backbone gracefully
        // the call to gotohash() here makes it possible to load bookmark urls
        // even if a users is not logged into istatd.
        gotohash();
    },

    showViewsWhenSomebodyIsLoggedIn: function() {
        // someone is logged in, so show logout view
        var self = this;
        this.userLoginView.slideUp(function() {
            self.userLogoutView.slideDown();
            // convert to backbone model and view
            theUserName = self.model.getLoggedInUser(); // global needed by old dashboards code.
            $('#save_dashboard').removeClass('disabled');
            theDashboards.reload();

            // hmm, this gotohash() still needs to pull into backbone gracefully
            gotohash();
        });
    },
});

var UserLoginView = Backbone.View.extend({
    initialize: function() {
        this.$username = $('<input type="text" id="loginname" value="Username">');
        this.$password = $('<input type="password" id="loginpassword">');
        var button     = $('<div class="tbutton" id="loginbutton">Login</div>');
        this.$el.append(this.$username);
        this.$el.append(this.$password);
        this.$el.append(button);
    },

    events: {
        "click #loginbutton": "login",
        "keydown #loginname": "input_keydown",
        "keydown #loginpassword": "input_keydown",
        "focus #loginname": "input_focus",
        "focus #loginpassword": "input_focus",
    },

    input_keydown: function(ev) {
        if (ev.which == 13) {
            $('#counter_jstree').jstree('deselect_all');
            if (ev.currentTarget.id == 'loginname') {
                this.$password.focus();
            } else {
                this.$password.blur();
                this.login();
            }
        }
    },

    input_focus: function(ev) {
        $(ev.currentTarget).val('');
    },

    login: function() {
        this.model.login(this.$username.val(), this.$password.val());
    },

    slideDown: function(callback) {
        this.$el.slideDown();
    },

    slideUp: function(callback) {
        this.$el.slideUp(callback);
    }
});


var istatdDropdownOptions = [ 'items', 'label' ];
var IstatdDropdownView = Backbone.View.extend({
    items: [],
    label: 'no-label',

    constructor: function(options) {
        Backbone.View.apply(this, arguments);
        _.extend(this, _.pick(options, istatdDropdownOptions));
    },

    initialize: function() {},

    renderLabel: function() {
        if (_.isUndefined(this.$label)) {
            this.$label = $('<div class="label"></div>');
            this.$el.append(this.$label);
        }

        this.$label.html(_.result(this, 'label'));
        return this;
    },

    renderDropdown: function() {
        if (_.isUndefined(this.$dropdown)) {
            this.$dropdown = $('<div class="reveal"></div>');
            console.log(this.$dropdown);
            _.each(this.items, function(item_definition) {
                var $entry = $('<div class="tmenuitem">' + item_definition.item + '</div>');
                if (item_definition.selected || false) {
                    $entry.addClass('current');
                }
                this.$dropdown.append($entry);
            }, this);
            this.$el.append(this.$dropdown);
        }

        return this;
    },

    render: function() {
        this
            .renderLabel()
            .renderDropdown()
        return this;
    },

    events: function() {
        var self = this;
        $('body').mouseup(function(ev) {
            self.close(self.$el);
        });

        return {
            'click': 'open',
            'click .reveal .tmenuitem': 'doAction'
        }
    },

    slideDown: function(callback) {
        this.render();
        this.$el.slideDown();
    },

    slideUp: function(callback) {
        this.$el.slideUp(callback);
    },

    open: function(ev) {
        ev.stopPropagation();
        $(ev.delegateTarget).toggleClass('open')
    },

    close: function($el) {
        $el.removeClass('open');
    },

    doAction: function(ev) {
        ev.stopPropagation();
        var item = $(ev.currentTarget).text();
        var item_definition = _.findWhere(this.items, {item: item});
        item_definition['action'](ev, this.model);
    },


});

var UserLogoutView = IstatdDropdownView.extend({
    label: function() { return "User: " + this.model.getLoggedInUser(); },

    items: [
        { item: 'Logout', action: function(ev, model) { model.logout(); } }
    ],


});

var ChartSizeView = IstatdDropdownView.extend({
    label: 'Chart Size',

    items: [
        { item: 'Small (300x120)',  action: function(ev) { graph_size(300, 120, $(ev.currentTarget)); } },
        { item: 'Medium (600x240)', action: function(ev) { graph_size(600, 240, $(ev.currentTarget)); }, selected: true },
        { item: 'Large (800x480)',  action: function(ev) { graph_size(800, 480, $(ev.currentTarget)); } },
        { item: 'Larger (1024x768)',  action: function(ev) { graph_size(1024, 768, $(ev.currentTarget)); } },
        { item: 'Modern (1600x1200)',  action: function(ev) { graph_size(1600, 1200, $(ev.currentTarget)); } },
        { item: 'Huge (2048x1536)',  action: function(ev) { graph_size(2048, 1536, $(ev.currentTarget)); } },
        { item: 'Massive (3200x2400)',  action: function(ev) { graph_size(3200, 2400, $(ev.currentTarget)); } },
        { item: 'Wide (1900x400)',  action: function(ev) { graph_size(1900, 400, $(ev.currentTarget)); } },
        { item: 'Custom...',        action: function(ev) { size_custom(); } },
    ],

    initialize: function(options) {
        this.render();
    },
});

var AdvancedToolsView = IstatdDropdownView.extend({
    label: 'Advanced Tools',
    items: [
        { item: 'Regex/Replace...',            action: function(ev) { regexp_change(); } },
        { item: 'Regex/Add...',                action: function(ev) { regexp_add(); } },
        { item: 'Configure Event Annotations', action: function(ev) { configure_events(); } },
        { item: 'Manual Date Range...',        action: function(ev) { select_daterange(); } },
        { item: 'Create Bookmark URL (JSON)',  action: function(ev) { packup_state_as_json(); } },
    ],
    initialize: function(options) {
        this.render();
    },

});

var setupKBShortcuts = util.guard(function _setup_KB_shortcuts() {
    $(document).keydown(function (e) {
        if(e.which == 83 && e.ctrlKey == true && e.shiftKey == true) { //ctrl+shift+s
            save_dashboard();
            return false;
        }else if(e.which == 69 && e.ctrlKey == true && e.shiftKey == true) { //ctrl+shift+s
            email_user();
            return false;
        }else if(e.which == 80 && e.ctrlKey == true && e.shiftKey == true) { //ctrl+shift+p
            email_png_user();
            return false;
        }else if(e.which == 76 && e.ctrlKey == true && e.shiftKey == true) { //ctrl+shift+l
            make_permalink_png();
            return false;
        }else if(e.which == 83 && e.ctrlKey == true) { //ctrl+s
            packup_state_as_json();
            return false;
        }
    }
    );
});

var on_ready = util.guard(function _on_ready() {
    var end = begin('on_ready');
    theTabs = new TabCollection('lefttab');
    theDashboards = new DashboardList('dashboards', theTabs.widget);
    theSplitter = new HSplitter('hsplit');
    loader = new Loader(theShittyGlobals.theCurrentDates);
    theShittyGlobals.theGrid = new GraphGrid('grid', loader);
    theTimeSlider = new TimeSlider('time_slider');

    counterTreeView = new CounterTreeView();
    userControls = new UserControlsView();
    chartSize = new ChartSizeView({el: $('.tdropdown.chart_size')});
    advanceTools = new AdvancedToolsView({el: $('.tdropdown.advanced_tools'), loader: loader});

    $('#hsplit').attr('unselectable', 'on');
    $('.tbutton').attr('unselectable', 'on');
    $('.tdropdown div').attr('unselectable', 'on');
    $('.tslider div').attr('unselectable', 'on');

    $('.tbutton.refresh').click(util.guard(function(ev) {
        if($(this).hasClass('disabled')) {
            return;
        }
        guiRefresh(theShittyGlobals);
    }));
    $('.tbutton.auto_refresh').click(util.guard(function(ev) {
        if($(this).hasClass('disabled')) {
            return;
        }
        auto_refresh();
    }));
    $('.tbutton#save_dashboard').click(util.guard(function(ev) {
        if($(this).hasClass('disabled')) {
            return;
        }
        save_dashboard();
    }));
    $('.tbutton.events').click(util.guard(function(ev) {
        if($(this).hasClass('disabled')) {
            return;
        }
        toggleEvents(theShittyGlobals);
    }));

    var leftTabToggler = toggle_lefttab_visibility(theShittyGlobals);
    var topTabToggler = toggle_toptab_visibility(theShittyGlobals);
    var topTabExtraToggler = toggle_toptab_extra_visibility(theShittyGlobals);

    theShittyGlobals.leftTabToggler = leftTabToggler;
    theShittyGlobals.topTabToggler = topTabToggler;
    theShittyGlobals.topTabExtraToggler = topTabExtraToggler;
    $('#hide_lefttab').click(leftTabToggler);
    $('#hide_toptab').click(topTabToggler);
    $('#hide_toptab_extra').click(topTabExtraToggler);

    setupKBShortcuts();


    $(window).resize(theShittyGlobals.theGrid.repaintAll.bind(theShittyGlobals.theGrid));
    theEventLoader = new EventLoader();

    window.onhashchange = gotohash;
    if(visibilitySupported) {
        document.addEventListener(visibilityChange, function() {
            if(document_hidden != document[hidden]) {
                if(document[hidden]) {
                    guiRefresh(theShittyGlobals);
                }
            }

            document_hidden = document[hidden];
        });
    }

    end();
});

    return on_ready;
});
