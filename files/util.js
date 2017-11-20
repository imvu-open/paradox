define('util', function() {
    /* Event handlers and other calls into this application
    from the surrounding DOM should be wrapped in guard().
    This lets us display error messages and track exceptions
    in an explicit way that improves user experience and
    makes the app easier to debug.

    Guard takes a function, and returns a function that does
    whatever the original function did, with appropriate
    wrapping for error/exception handling.
    */
    function guard(f) {
        return function() {
            try {
                return f.apply(this, arguments);
            }
            catch (e) {
                if (typeof(console) !== 'undefined') {
                    console.log(e.message);
                }

                var stack = e.stack;
                if(typeof(stack) === 'undefined') {
                    var callee = arguments.callee;
                    stack = [];
                    while(callee) {
                        var name = callee.toString().match(/function\s*([^(])*/)[1];
                        name = name || '<anonymous function>';
                        name += '(' + Array.prototype.join.call(callee.arguments, ', ') + ')';
                        stack.push(name);
                        callee = callee.caller;
                    }
                    stack = stack.join('\n');
                }

                var str = 'Error in ' + f.name + ':\n' + e.message + '\n\n' + stack;
                errorDialog(str);
            }
        }
    }

    function htmlescape(str) {
        str = str.replace(/\&/g, '&amp;').replace(/</g, '&lt;').replace(/\n/g, '<br>\n');
        return str;
    }

    function arrayKeys(a) {
    var ret = new Array();
    for (k in a) {
        ret.push(k);
    }
    ret.sort();
    return ret;
    }

    function errorDialog(str, container) {
        console.log('errorDialog ' + str);
        if (!container) {
            container = $('body');
        }
        else {
            container = $(container);
        }
        var $div = $('.dialog.error');
        var append = false;
        if (!$div || $div.length == 0) {
            $div = $("<div class='dialog error'><span class='left_closebox buttonbox'/><span class='closebox buttonbox'/><div class='scroll'><span class='text'/></div></div>");
        }
        else {
            append = true;
            $div.detach();
        }
        str = "<pre>" + htmlescape(str) + "</pre>";
        var $span = $('span.text', $div);
        if (append) {
            str = $span.html() + "<br/><br/>" + str;
        }
        $span.html(str);
        $div.prependTo(container);
        //  can't use guard() in error because of recursion
        $('span.closebox', $div).click(function() {
            $div.remove();
        });
        $('span.left_closebox', $div).click(function() {
            $div.remove();
        });
        $(document).keydown(function(e) {
            if (e.keyCode == 27) {
                $div.remove();
                $(document).unbind("keydown");
            }
        });
        nesting = 0;
        $container = $('div#progressbar').show();
        $progress = $('.progressfg', $container);
        $progress.animate({width: 0}, 500.0, function() {
            $container.css('visibility', 'hidden');
        });
    }

    function closeErrorDialog(container) {
        console.log('closeErrorDialog ');
        if (!container) {
            container = $('body');
        }
        else {
            container = $(container);
        }
        var $div = $('.dialog.error');
        $div.remove();
    }

    /* Actually show data from a little while ago, to avoid showing
    current, possibly not-yet-full, buckets. */
    function getAdjustedNow() {
        return new Date((new Date().getTime())-10*1000);
    }

    function empty(obj) {
        for (var k in obj) {
            return false;
        }
        return true;
    }

    function keys(obj) {
        ret = [];
        for (var k in obj) {
            ret.push(k);
        }
        return ret;
    }

    function choiceDialog(text, options, cb) {
        console.log('options: ' + text);
        $('.dialog.prompt').detach();
        var buttons = [];
        for (var k in options) {
            buttons.push("<div class='tbutton' name='" + k + "'>" + htmlescape(options[k]) + "</div>");
        }
        buttons = buttons.join("");
        var $div = $("<div class='dialog prompt'><span class='closebox buttonbox'></span><div class='text'></div>" +
            "<div class='buttonrow choices'>" + buttons + "</div></div>");
        str = htmlescape(text);
        $('div.text', $div).html(str);
        $div.prependTo($('body'));
        $('span.closebox', $div).click(function() {
            $div.remove();
        });
        var eHandler = guard(function(ev) {
            ev.stopPropagation();
            var choice = $(this).attr('name');
            console.log('choice: ' + choice);
            cb(choice);
            $div.remove();
        });
        $('.tbutton', $div).click(eHandler);
        return $div;
    }

    function calcReloadInterval(shittyGlobals) {
        var delta = (shittyGlobals.theCurrentDates.stop.getTime() - shittyGlobals.theCurrentDates.start.getTime())/1000;
        var ival = Math.ceil(delta / 500);
        var points = [10, 15, 20, 30, 60]
        for (var k in points) {
            if (points[k] >= ival) {
                shittyGlobals.theReloadInterval = points[k];
                return;
            }
        }
        shittyGlobals.theReloadInterval = 120;
    }

    var refresh = guard(function _refresh(shittyGlobals) {
        if (!_.isUndefined(shittyGlobals.pageIsHidden()) && shittyGlobals.pageIsHidden()) {
            return;
        }
        var date = getAdjustedNow();
        console.log('refresh; auto reload interval=' + shittyGlobals.theReloadInterval + ' date is ' + date);
        var delta = shittyGlobals.theCurrentDates.stop.getTime() - shittyGlobals.theCurrentDates.start.getTime();
        if ((shittyGlobals.theCurrentDates.stop - shittyGlobals.theCurrentDates.start) == (shittyGlobals.theOriginalDates.stop - shittyGlobals.theOriginalDates.start)) {
            shittyGlobals.theCurrentDates.start = new Date(date.getTime() - delta);
            shittyGlobals.theCurrentDates.stop = date;
        }
        if(!_.isUndefined(shittyGlobals.theGrid)) {
            shittyGlobals.theGrid.reloadAll();
        }
    });

    function zoomOutIntervals(shittyGlobals) {
        restoreOriginalInterval(shittyGlobals);
        calcReloadInterval(shittyGlobals);
    }


    function restoreOriginalInterval(shittyGlobals) {
        shittyGlobals.theCurrentDates.start = shittyGlobals.theOriginalDates.start;
        shittyGlobals.theCurrentDates.stop = shittyGlobals.theOriginalDates.stop;
    }

    function round(value, decimals) {
        return Number(Math.round(value+'e'+decimals)+'e-'+decimals);
    }

    function parseRGBColor(colorStr) {
        return /rgb[(](\d+)[,](\d+)[,](\d+)[)]/gi.exec(colorStr).slice(1);
    }

    function arrayToRGBColor(colorArr) {
        return "rgb(" + colorArr[0] + "," + colorArr[1] + "," + colorArr[2] + ")";
    }
    function promptDialogN(prmp, fields, submitCallback) {
        console.log('prompt: ' + prmp);

        $('.dialog.prompt').detach();
        var $div = $("<div class='dialog prompt'><span class='closebox buttonbox'></span><div class='text'></div>" +
            "<div class='inputs'></div><div class='tbutton' name='prompt_done'>OK</div></div>");
        str = htmlescape(prmp);
        $('div.text', $div).html(str);
        $inputs = $('div.inputs', $div);
        var fldNames = [];
        for (var k in fields) {
            fldNames.push(k);
            $input = $("<div class='input " + k + "'><label for='" + k + "'>" + fields[k] +
                "</label> <input type='text' class='answer " + k + "' name='" +
                k + "'/></div> ");
            $input.appendTo($inputs);
        }
        $div.prependTo($('body'));
        $('span.closebox', $div).click(function() {
            $div.remove();
        });
        $('.answer', $div).val('');
        var eHandler = guard(function(ev) {
            ev.stopPropagation();
            var kv = {};
            console.log('fldNames: ' + JSON.stringify(fldNames));
            for (var k in fldNames) {
                var fn = fldNames[k];
                kv[fn] = $('.answer.' + fn, $inputs).val();
            }
            if(submitCallback(kv) !== false) {
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


    return {
        'errorDialog' : errorDialog,
        'closeErrorDialog' : closeErrorDialog,
        'arrayKeys': arrayKeys,
        'htmlescape': htmlescape,
        'guard': guard,
        'getAdjustedNow': getAdjustedNow,
        'empty': empty,
        'keys': keys,
        'choiceDialog': choiceDialog,
        'calcReloadInterval': calcReloadInterval,
        'refresh': refresh,
        'round': round,
        'zoomOutIntervals': zoomOutIntervals,
        'parseRGBColor': parseRGBColor,
        'arrayToRGBColor': arrayToRGBColor,
        'restoreOriginalInterval': restoreOriginalInterval,
        'promptDialogN':promptDialogN,
        }
});
