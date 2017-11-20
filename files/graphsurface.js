define('graphsurface', ["widget", "util", "imvu-hairlines", "context"], function(Widget, util) {
//  GraphSurface turns a $div ($self) into a graph destination
//  parWig is the widget of the parent container
function GraphSurface($self, parWig, loader, shittyGlobals) {
    //  not a widget?
    if (!parWig.$obj) {
        util.errorDialog("GraphSurface parent is not a widget:\n" + objDump(parWig));
        return;
    }
    var self = this;
    $self.css({position: 'relative'});
    this.$self = $self;
    this._loader = loader;
    this._series = {};
    this._lastRenderData = {};
    this._dygraph = null;
    this._hairlines = null;
    this._format = 'noBars';
    this._extraOptsA = [];
    this._extraOpts = {};
    this._axisOpts = {};
    this._events = {};
    this._eventsConfig = {};
    this._collapseToggle = null;
    this._collapse = null;
    this._uncollapse = null;
    this._collapsed = false;
    this._showEvents = true;
    this._grid = _.has(shittyGlobals, "theGrid") ? shittyGlobals.theGrid : undefined;
    this._shittyGlobals = shittyGlobals;
    $('span.closebox', $self).click(util.guard(function() {
        self.close();
    }));
    $('span.zoomoutbox', $self).click(util.guard(function() {
        self._shittyGlobals.zoomOutIntervals(self._shittyGlobals);
    }));
    $('span.settingsbox', $self).click(util.guard(function() {
        self.showSettings();
    }));
    $('span.summarybox', $self).click(util.guard(function() {
        self.toggleSummary();
    }));
    $('span.querybox', $self).click(util.guard(function() {
        self.showManualQuery();
    }));
    new Widget(this, $self, parWig);

    var $div = $('.graphdiv', this.$self);
    var t = document.querySelector('#point-select-template');
    var clone = document.importNode(t.content, true);
    $div.parents('.graph').append(clone);

    if (!_.isUndefined(this._grid)) {
        this._grid.add(this);
    }
    var me = this;
    var menuItems = [
            {header: 'Graph Tools'},
            {text: 'Edit Query', action: function(e) {
                    self.showManualQuery();
                }
            },
            {text: 'Breakout', action: function(e) {
                    self.showBreakout();
                }
            },
            {text: 'Show StdDev', action: function(e) {
                    self._format = 'errorBars';
                    self.repaint();
                }
            },
            {text: 'Show Lines', action: function(e) {
                    self._format = 'noBars';
                    self.repaint();
                }
            },
            {text: 'Show Min/Max', action: function(e) {
                    self._format = 'customBars';
                    self.repaint();
                }
            },
            {text: 'Raw Data', action: function(e) {
                    self.rawData();
                }
            },
        ];
    if (!_.isUndefined(this._grid)) {
        menuItems.splice(1, 0, {text: 'Duplicate', action: function(e) {
                        var s = self.clone();
                        s.reload();
                    }
                }
            );
    }
    var collapse = function () {
        if (!self._collapsed) {
            $me = $(this);

            var height = $me.height();

            $me.animate({ height: "30px" }, {duration: 1000, queue: false});
            self._collapsed = true;
        }

    }.bind($div.parents('.graph').find('.legend'));
    var uncollapse = function() {
        if (self._collapsed) {
            $me = $(this);
            $me.animate({ height: $me.get(0).scrollHeight }, {duration: 1000, queue: false });
            self._collapsed = false;
        }
    }.bind($div.parents('.graph').find('.legend'));

    var collapseToggle = function() {
        if (self._collapsed) {
            self._uncollapse();
        }else{
            self._collapse();
        }
    };

    this._collapseToggle = collapseToggle;
    this._collapse = collapse;
    this._uncollapse = uncollapse;
    var legendMenuItems = [
            {header: 'Legend Tools'},
            {text: 'Toggle legend collapse', action: function(e) {
                    self._collapseToggle();
                }
            },
            {text: 'Legend collapse', action: function(e) {
                    self._collapse();
                }
            },
            {text: 'Legend uncollapse', action: function(e) {
                    self._uncollapse();
                }
            },
        ];
    context.attach($div.parents('.graph').find('.legend'), legendMenuItems);
    context.attach($self, menuItems);
}
GraphSurface.prototype.rawData = function GraphSurface_rawData() {
    window.open('data:application/json;' + (window.btoa ? 'base64,' + btoa(JSON.stringify(this._lastRenderData)) : JSON.stringify(this._lastRenderData)));
}

GraphSurface.prototype.clone = function GraphSurface_clone() {
    var s = this._grid.newGraph();
    s._series = _.clone(this._series);
    s._format = _.clone(this._format);
    s._showEvents = _.clone(this._showEvents);
    return s;
}
GraphSurface.prototype._destroyDygraph = function GraphSurface_destroyDygraph() {
    if(this._dygraph) {
        this._dygraph.destroy();
        this._dygraph = null;
    }
}
GraphSurface.prototype.tryRemoveSeries = util.guard(function GraphSurface_tryRemoveSeries(path) {
    if (this._series[path]) {
        // do not allow the last series to be deleted
        if (util.keys(this._series).length > 1) {
            delete this._series[path];
        } else {
            console.log("Cannot remove last graph");
        }
    } else {
        console.log("This path is not a single counter. It may be the result of N queries mapping to M counters. We cannot remove it");
    }
    this.reload();
})
GraphSurface.prototype.toggleSeries = util.guard(function GraphSurface_toggleSeries(path) {
    if (this._series[path]) {
        // do not allow the last series to be deleted
        if (util.keys(this._series).length > 1) {
            delete this._series[path];
        }
    } else {
        this._series[path] = function(s) { return s };
    }
    this.reload();
})
GraphSurface.prototype.click = util.guard(function GraphSurface_click(ev) {
    ev.stopPropagation();
    if (!_.isUndefined(this._grid)) {
        this._grid.select(this);
    }
})

GraphSurface.prototype.reload = function GraphSurface_reload() {
    var self = this;
    var seriesKeys = util.keys(this._series);
    this._loader.addToNextGet(seriesKeys, 500,
        this._shittyGlobals.theGraphMaxSamples ? this._shittyGlobals.theGraphMaxSamples : this._shittyGlobals.theGraphSize.width,
        function(data) {
            self.renderData(seriesKeys, data);
        }
    );
}

var combinator = function(accum, elem, field, combiner) {
    if (_.has(elem, field)) {
        if (_.has(accum, field)) {
            accum[field] = combiner(accum[field], elem[field]);
        } else {
            accum[field] = elem[field];
        }
    }
    return accum;
}
var orCombine = function (l,r) { return l || r; }
var maxCombine = function (l,r) { return Math.max(l, r); }
var minCombine = function (l,r) { return Math.min(l, r); }

GraphSurface.prototype.condenseExtraOpts = function GraphSurface_condenseExtraOpts(optsA) {
    var newOpts = {};
    newOpts = _.reduce(optsA, function(a, e) {

        a = combinator(a, e, "stackedGraph", orCombine);
        a = combinator(a, e, "stackedGraphNaNFill", orCombine);
        a = combinator(a, e, "constrainMin", minCombine);
        a = combinator(a, e, "constrainMax", maxCombine);
        a = combinator(a, e, "constrainMinY2", minCombine);
        a = combinator(a, e, "constrainMaxY2", maxCombine);
        a = combinator(a, e, "removeMinConstraint", orCombine);
        a = combinator(a, e, "removeMaxConstraint", orCombine);
        a = combinator(a, e, "removeMinConstraintY2", orCombine);
        a = combinator(a, e, "removeMaxConstraintY2", orCombine);

        var remains = _.omit(e, ['stackedGraph', 'stackGraphNaNFill', 'constrainMin', 'constrainMax', 'constraintMinY2', 'constraintMaxY2', 'removeMinConstraint', 'removeMaxConstraint', 'removeMinConstraintY2', 'removeMaxConstraintY2']);

        a = _.extend(a, remains);
        return a;
    }, {});
    return newOpts;
}

GraphSurface.prototype.renderData = function GraphSurface_renderData(seriesKeys, data) {
    if (seriesKeys.length == 0) {
        //  removed -- don't do more
        console.log('seriesKeys is empty -- not rendering');
        return;
    }
    this._lastRenderData = {start: data.start, stop:data.stop, interval:data.interval};
    this._extraOptsA = [];
    this._extraOpts = {};
    this._axisOpts = {};
    for (var k in seriesKeys) {
        var query = seriesKeys[k];
        var myResults = data.results[query];
        for (k in myResults.counters) {

            this._lastRenderData[k] = myResults.counters[k];
        }
        this._extraOptsA.push(myResults.options);
        _.extend(this._axisOpts, myResults.axis_options);
    }
    this._events = data.events;
    this._eventsConfig = this._loader.getEvents();


    this._extraOpts = this.condenseExtraOpts(this._extraOptsA);
    var self = this;
    //  asynchronize actual painting
    setTimeout(function() {
        if (_.isUndefined(self._shittyGlobals.pageIsHidden) || !self._shittyGlobals.pageIsHidden()) {
            self.repaint();
        }
    }, 1);
}
GraphSurface.prototype.repaint = util.guard(function GraphSurface_repaint() {
    var $div = $('.graphdiv', this.$self);
    $div.css({"margin-left": 0, "margin-top": 10, "background": "transparent"});
    $div.width(this._shittyGlobals.theGraphSize.width - 30);
    $div.height(this._shittyGlobals.theGraphSize.height - 60);


    this._eventsConfig = this._loader.getEvents();

    // about to do a bad thing with the plot array.  assuming data
    // arrays are the same length and has same start time and interval
    var data = this._lastRenderData;

    var plotTimes = [];
    var labels = ['time'];

    var interval = data.interval;
    var start = data.start;
    var stop = data.stop;
    var format = this._format || 'noBars';

    // initialize data array with Date objects
    var i = start;
    while ( i < stop ) {
        plotTimes.push([new Date(i*1000)]);
        i += interval;
    }
    if(plotTimes.length == 0) {
        return;
    }


    var minimum = Math.pow(2,100) - 1; // pick a really big minimum to start
    var maximum = -minimum;
    var minVal = minimum;
    var minimum2 = Math.pow(2,100) - 1; // pick a really big minimum to start
    var maximum2 = -minimum;
    var minVal2 = minimum;
    var gotdata = false;
    var pushfn = null;
    var ann_maxval = maximum;
    var ann_maxtime = null;
    var ann_minval = minimum;
    var ann_mintime = null;

    var range_slop = 0;
    var range_slop2 = 0;
    //  "errorBars" really means sdev
    //  "customBars" really means min/max
    //  "noBars" means no bars :-)
    if (format == 'noBars' || format == 'stacked' || format == 'area') {
        pushfn = function(plot, bucket, graphOpts) {
            if (!bucket) {
                if (_.has(graphOpts, "graphNulls") && graphOpts["graphNulls"] === true) {
                    plot.push(null);
                }else {
                    plot.push(NaN);
                }
            }
            else {
                plot.push(bucket.avg);
                if (!_.has(graphOpts, "axis") || graphOpts["axis"] !== "y2") {
                    minimum = Math.min(bucket.avg, minimum);
                    maximum = Math.max(bucket.avg, maximum);
                    minVal = Math.min(bucket.avg, minVal);
                }else {
                    minimum2 = Math.min(bucket.avg, minimum2);
                    maximum2 = Math.max(bucket.avg, maximum2);
                    minVal2 = Math.min(bucket.avg, minVal2);
                }
                gotdata = true;
            }
        };
    }
    else if (format == 'errorBars') {
        pushfn = function(plot, bucket, graphOpts) {
            if (!bucket) {
                if (_.has(graphOpts, "graphNulls") && graphOpts["graphNulls"] === true) {
                    plot.push([null, null]);
                }else {
                    plot.push([NaN, NaN]);
                }
            }
            else {
                plot.push([bucket.avg, bucket.sdev]);
                if (!_.has(graphOpts, "axis") || graphOpts["axis"] !== "y2") {
                    minimum = Math.min(bucket.avg-bucket.sdev, minimum);
                    maximum = Math.max(bucket.avg+bucket.sdev, maximum);
                    minVal = Math.min(bucket.avg, minVal);
                }else {
                    minimum2 = Math.min(bucket.avg-bucket.sdev, minimum2);
                    maximum2 = Math.max(bucket.avg+bucket.sdev, maximum2);
                    minVal2 = Math.min(bucket.avg, minVal2);
                }
                gotdata = true;
            }
        };
    }
    else {
        pushfn = function(plot, bucket, graphOpts) {
            if (!bucket) {
                if (_.has(graphOpts, "graphNulls") && graphOpts["graphNulls"] === true) {
                    plot.push([null, null, null]);
                }else {
                    plot.push([NaN, NaN, NaN]);
                }
            }
            else {
                plot.push([bucket.min, bucket.avg, bucket.max]);
                if (!_.has(graphOpts, "axis") || graphOpts["axis"] !== "y2") {
                    minimum = Math.min(bucket.min, minimum);
                    maximum = Math.max(bucket.max, maximum);
                    minVal = Math.min(bucket.min, minVal);
                }else {
                    minimum2 = Math.min(bucket.min, minimum2);
                    maximum2 = Math.max(bucket.max, maximum2);
                    minVal2 = Math.min(bucket.min, minVal2);
                }
                gotdata = true;
            }
        };
    }
    var annotations = [];
    var hairline = [];
    var perGraphOpts = {};
    jQuery.each(data, function(key,value) {
        var buckets = data[key]['data'];
        if (buckets) {
            var ann_min = Math.pow(2, 100);
            var ann_max = -ann_min;
            var ann_min_ts = null;
            var ann_max_ts = null;
            var bidx = 0;
            var graphOpts = null;
            if (data[key].graph_opts !== "") {
                if (typeof data[key].graph_opts === 'object') {
                    graphOpts = data[key].graph_opts;
                    perGraphOpts[key] = graphOpts;
                }
                else {
                    var unescaped = data[key].graph_opts.replace(/\\"/g, '"');
                    graphOpts = JSON.parse(unescaped);
                    perGraphOpts[key] = graphOpts;
                }
            }
            jQuery.each(plotTimes, function(i, plot) {
                // get next bucket of data to insert
                while ((bidx < buckets.length) && (buckets[bidx].time == 0)) {
                    bidx++;
                }

                if (bidx >= buckets.length) {
                    pushfn(plot, null, graphOpts);
                }
                else {
                    var bucket = buckets[bidx];
                    var btime = bucket.time*1000;
                    var timestamp = plot[0].getTime();
                    if (bucket.min < ann_min) {
                        ann_min = bucket.min;
                        ann_min_ts = timestamp;
                    }
                    if (bucket.max > ann_max) {
                        ann_max = bucket.max;
                        ann_max_ts = timestamp;
                    }

                    if (btime == timestamp) {
                        pushfn(plot, bucket, graphOpts);
                        bidx += 1;
                    }
                    else {
                        pushfn(plot, null, graphOpts);
                    }
                }
            });
            labels.push(data[key].name);
            if ((!_.isUndefined(perGraphOpts[key]) && !_.isUndefined(perGraphOpts[key].showAnnotations)) &&
                    perGraphOpts[key].showAnnotations === true) {
                var ann_min_short = util.round(ann_min, 5).toString();
                var ann_max_short = util.round(ann_max, 5).toString();
                annotations.push({
                    series: key,
                    x: ann_min_ts,
                    shortText: ann_min_short,
                    text: key + " min " + ann_min.toString(),
                    width: 8 * ann_min_short.length

                });
                annotations.push({
                    series: key,
                    x: ann_max_ts,
                    shortText: ann_max_short,
                    text: key + " max " + ann_max.toString(),
                    width: 8 * ann_max_short.length
                });
            }
            ann_minval = Math.min(ann_min, ann_minval);
            if (ann_minval == ann_min) {
                ann_mintime = ann_min_ts;
            }
            ann_maxval = Math.max(ann_max, ann_maxval);
            if (ann_maxval == ann_max) {
                ann_maxtime = ann_max_ts;
            }
        }
    });
    var that = this;
    if (this._showEvents) {
        _.map(this._eventsConfig, function (evConfig, n) {
            var keys = evConfig.keys;
            var allOurKeys = _.flatten(_.map(keys, function(k) {
                var [ak,fk] = k.split(":");
                if (!_.isUndefined(that._events[ak])) {
                    return that._events[ak];
                }else
                    return [];
                }));
            
            _.map(allOurKeys, function(x) {
                hairline.push({xval: x.created_at * 1000, interpolated:false, message: x.message, values: x, thickness: evConfig.thickness, color: evConfig.color});
            });
        });
    }
    // for stacked charts, the y axis must be based on max of the sum
    // of the y values for each x.
    if (format == 'stacked' || (_.has(this._extraOpts, "stackedGraph") && this._extraOpts["stackedGraph"] === true )) {
        var sums = Array(plotTimes.length);
        jQuery.each(data, function(key, series) {
            if (series instanceof Object && 'data' in series) {
                jQuery.each(series['data'], function(index, bucket) {
                    if (sums[index] == undefined) {
                        sums[index] = bucket.avg;
                    }
                    else {
                        sums[index] += bucket.avg;
                    }
                });
            }
        });
        maximum = -Math.pow(2,100) - 1; // pick a really big negative maximum to start
        jQuery.each(sums, function(key, value) {
            maximum = Math.max(value, maximum);
        });

    }

    // Doing Dygraph's job of calculating the data range properly,
    // so that error bars don't go outside the plotted data set.
    if (!gotdata) {
        // bah!  got no data!
        // make the empty graph show the range from 0-1 so at least we get some y axis labels
        minimum = 0;
        maximum = 1.1;
    }
    else {
        // need to add % above the ends of of range for easy selection of zoom ranges
        range_slop = (maximum - minimum) * 0.05;
        if (range_slop == 0.0) {
            range_slop = 0.5;
        }
        maximum = maximum + (range_slop * 2); // 10% extra on top
        minimum = minimum - range_slop;       // 5% extra on bottom!

        range_slop2 = (maximum2 - minimum2) * 0.05;
        if (range_slop2 == 0.0) {
            range_slop2 = 0.5;
        }
        maximum2 = maximum2 + (range_slop2 * 2); // 10% extra on top
        minimum2 = minimum2 - range_slop2;       // 5% extra on bottom!
    }

    // Does the sdev range go negative, but no value goes negative? Clamp to 0.
    if (minVal >= 0 && minimum < 0) {
        minimum = 0;
    }
    // 
    // Does the sdev range go negative, but no value goes negative? Clamp to 0.
    if (minVal2 >= 0 && minimum2 < 0) {
        minimum2 = 0;
    }

    if (_.has(this._extraOpts, 'constrainMin') && (!_.has(this._extraOpts, 'removeMinConstraint') || !this._extraOpts['removeMinConstraint'])) {
        minimum = this._extraOpts['constrainMin'];
    }
    if (_.has(this._extraOpts, 'constrainMax') && (!_.has(this._extraOpts, 'removeMaxConstraint') || !this._extraOpts['removeMaxConstraint'])) {
        maximum = this._extraOpts['constrainMax'];
    }

    if (_.has(this._extraOpts, 'constrainMinY2') && (!_.has(this._extraOpts, 'removeMinY2Constraint') || !this._extraOpts['removeMinY2Constraint'])) {
        minimum2 = this._extraOpts['constrainMinY2'];
    }
    if (_.has(this._extraOpts, 'constrainMaxY2') && (!_.has(this._extraOpts, 'removeMaxY2Constraint') || !this._extraOpts['removeMaxY2Constraint'])) {
        maximum2 = this._extraOpts['constrainMaxY2'];
    }


    var self = this;
    // If we have no interaction model yet, define one, by copying the default dygraph.
    // We need to do this to provide a few extra overrides in spots to dygraph behaviour.
    if(!this._shittyGlobals.theInteractionModel) {
        var model = {};
        this._shittyGlobals.theInteractionModel = model;

        var defaultModel = Dygraph.Interaction.defaultModel;
        for(k in defaultModel) {
            model[k] = defaultModel[k];
        }

        // The mouse up event tracks the domain of the graph,
        // so we can use that for a zoomCallback that will fire directly after.
        model.lastDomain = {};
        model.mouseup = util.guard(function(event, g, context) {
            model.lastDomain[g] = g.xAxisRange().slice(0);
            if (defaultModel.mouseup !== undefined) {
                defaultModel.mouseup(event, g, context);
            }
        });

        // Double click to restore zoom *AND* restore original time interval.
        model.dblclick = util.guard(function(event, g, context) {
            self._shittyGlobals.zoomOutIntervals(self._shittyGlobals);
            defaultModel.dblclick(event, g, context);
        });
    }

    if (!_.isUndefined(this._grid)) {
        this._grid.$self.find('canvas').css('opacity', 1.0);
    }

    this._destroyDygraph();
    var hairlines = this._hairlines = new Dygraph.Plugins.ImvuHairlines({
        divFiller: function(div, data) {
            var html = "<span>" + data.hairline.data.message + "</span><br>"
            
            _.map(data.hairline.data.values, function (el, k) {
                     html += "<span>" + k + " : " + el + "</span><br>";
            });
            $('.hairline-legend', div).html(html);
            $('.dygraph-hairline div', div).css("width", data.hairline.data.thickness);
            $('.dygraph-hairline div', div).css("background", data.hairline.data.color);
            $(div).draggable({drag: function (e,u) { return false; } });
            $('.dygraph-hairline').draggable({drag: function (e,u) { return false; } });
        }
    });
    var params = {
            'labelsDiv': $div.parents('.graph').find('.legend').get()[0],
            'legend': 'always',
            'labels': labels,
            'showLabelsOnHighlight': true,
            'zoomCallback': function(minX, maxX, yRanges) {
                // We need to use the interactionModel's stored domain to check if the X axis is changed.
                // Y zooming doesn't change the X axis, so this is reliable.
                var domain = self._shittyGlobals.theInteractionModel.lastDomain[g] || [NaN, NaN];

                // If we're zooming on the x, then we need to fetch finer resolution data.
                // Would have used g.isZoomed('x'), but that will always return false,
                // except for the newest Dygraph instance which gets the correct values.
                // This is a cheesy workaround in the meantime!
                if(minX != domain[0] || maxX != domain[1]) {
                    if (!_.isUndefined(self._grid)) {
                        self._grid.$self.find('canvas').css('opacity', 0.5);
                    }
                    self._shittyGlobals.theCurrentDates.start = new Date(minX);
                    self._shittyGlobals.theCurrentDates.stop = new Date(maxX);

                    console.log("Zoomed Range: " + self._shittyGlobals.theCurrentDates.start + " - " + self._shittyGlobals.theCurrentDates.stop);
                    util.calcReloadInterval(self._shittyGlobals);
                    util.refresh(self._shittyGlobals);
                }
            },
            'interactionModel': this._shittyGlobals.theInteractionModel,
            'labelsKMB': true,
            'axes': {
                'x' : {
                    'valueFormatter': function (x) {return ''},
                },
                'y' : {
                    'valueFormatter': function (y) {return ''},
                    'pixelsPerLabel': 17,
                    'valueRange': [minimum, maximum]

                }
            },
            'plugins': [
                    hairlines
                ]

        };
    _.extend(params, {series: perGraphOpts});
    _.extend(params, this._extraOpts);
    if (_.has(this._extraOpts, "y2")) { ///hackkkkkkkk
        params["axes"]["y2"] = {} ;
        params["axes"]["y2"]["valueRange"] = [minimum2, maximum2];
        params["axes"]["y2"]["pixelsPerLabel"] = 17;
        params["axes"]["y2"]["valueFormatter"] = function(y) {return ''};
        params["axes"]["y2"]["drawAxis"] = true;
        params["axes"]["y2"]["labelsKMB"] = true;
        params["axes"]["y2"]["independentTicks"] = true;
    }

    _.map(this._axisOpts, function (v, k) {
        var axisOpt = params["axes"][k];
        if (_.isUndefined(axisOpt)) {
            axisOpt = {};
        }

        _.extend(axisOpt, v);
        params["axes"][k] = axisOpt;
    });

    //Dygraph is so dumb
    if (_.has(params["axes"]["y"], "logscale")) {
        params["logscale"] = params["axes"]["y"]["logscale"];
    }

    params.highlightCallback = function(e, x, pts, row, seriesName) {
        for (var i = 0; i < pts.length; i++) {
            var selSeriesName = seriesName || labels[1];
            if (pts[i].name === selSeriesName) {
                var props = self._dygraph.getPropertiesForSeries(selSeriesName);
                var coords = [self._dygraph.toDomXCoord(pts[i].xval), self._dygraph.toDomYCoord(pts[i].yval, props.axis - 1)];
                var parentPos = $div.parents('.graph').offset();

                var legendHeight = $($div.parents('.graph')).find('.legend').height();
                $($div.parents('.graph')).find('.point-select').offset({top: (parentPos.top + coords[1] + legendHeight) , left: (parentPos.left + coords[0]) });
                $($div.parents('.graph')).find('.point-select').css('visibility', 'visible');
                $($div.parents('.graph')).find('.point-select').css({color: props.color, 'border-color': props.color});
                if (e.ctrlKey) {
                    $($div.parents('.graph')).find('.point-select .content').html(new Date(pts[i].xval) + ": " + util.round(pts[i].yval, 2));
                }else if (e.altKey) {
                    $($div.parents('.graph')).find('.point-select .content').html((pts[i].xval / 1000) + ": " + util.round(pts[i].yval, 2));
                }else if (e.shiftKey) {
                    $($div.parents('.graph')).find('.point-select .content').html(pts[i].name + ": " + util.round(pts[i].yval, 2));
                } else {
                    $($div.parents('.graph')).find('.point-select .content').html(util.round(pts[i].yval, 2));
                }
            }
        }
    };
    params.unhighlightCallback = function(e) {
        $($div.parents('.graph')).find('.point-select').css('visibility', 'hidden');
    };
    if (labels.length > 2) {
        params.highlightSeriesOpts = {
            highlightCircleSize: 5,
            strokeWidth: 3,
        };
    }

    if (format == 'errorBars') {
        params.errorBars = true;
    }
    else if (format == 'customBars') {
        params.customBars = true;
    }
    else if (format == 'stacked') {
        params.stackedGraph = true;
    }
    else if (format == 'area') {
        params.fillGraph = true;
    }

    var g = this._dygraph = new Dygraph(
        // containing div
        $div[0],
        plotTimes,
        params
    );
    g.setAnnotations(annotations);
    $('.summary', this.$self).html("<span class='nobreak'>Maximum: " + ann_maxval + " at " +
        (new Date(ann_maxtime)).toLocaleDateString() + "</span><span class='nobreak'> Minimum: " +
        ann_minval + " at " + (new Date(ann_mintime)).toLocaleDateString() + "</span>");

    hairlines.set(hairline);
});
GraphSurface.prototype.close = util.guard(function GraphSurface_close() {
    this._destroyDygraph();
    this._series = null;
    this.$self.remove();
    if (!_.isUndefined(this._grid)) {
        this._grid.remove(this);
    }
});
GraphSurface.prototype.getSeries = function GraphsSurface_getSeries() {
    var ret = [];
    for (var s in this._series) {
        ret.push(s);
    }
    return ret;
}
GraphSurface.prototype.showSettings = function GraphSurface_showSettings() {
    var self = this;
    util.choiceDialog(    "Choose display format for these graphs.",
    {
        'noBars': "Lines",
        'errorBars': "StdDev",
        'customBars': "Min/Max",
        'stacked' : "Stacked",
        'area' : "Area"
    },
    function(v) {
        console.log("Selected format: " + v);
        self._format = v;
        self.repaint();
    });
}
GraphSurface.prototype.showManualQuery = function GraphSurface_showManualQuery() {
    var self = this;
    queryDialog("Modify queries", self,
    function(newQueryBlock) {
        var graph = self;
        var strs = newQueryBlock.trim().split('\n');
        var oldCtrs = util.keys(graph._series).slice(0);
        var newDiff = _.difference(strs, oldCtrs);
        for (var str in newDiff) {
            graph.toggleSeries(newDiff[str]);
        }
        var diff = _.difference(oldCtrs, strs);
        for (var str in diff) {
            graph.toggleSeries(diff[str]);
        }
    });
}

GraphSurface.prototype.showBreakout = function GraphSurface_showBreakout() {
    var self = this;
    singleFieldDialog("Breakout",
    function(regex) {
        var data = _.omit(self._lastRenderData, ['start', 'stop', 'interval']);
        var groups = _.groupBy(_.map(data, function(v,k) { return k; }), function(e) { return e.match(regex)[1]; });
        _.map(groups, function(vv,gk) {
            var graph = self.clone();
            graph._series = {};
            _.map(_.clone(self._series), function (v, k) { graph.toggleSeries(k + ' | include "' + gk + '"'); } );
            graph.reload();
        } );

    });
}
GraphSurface.prototype.toggleSummary = function GraphSurface_toggleSummary() {
    $('.summary', this.$self).toggleClass('visible');
}

var queryDialog = function(prmp, graph, cb) {
    var queries = _.map(graph._series, function(val, key) {
        return key;
    }).join("\n");
    console.log('prompt: ' + prmp);

    $('.dialog.prompt').detach();
    var $div = $("<div class='dialog prompt'><span class='closebox buttonbox'></span><div class='text'></div>" +
        "<textarea cols='60' rows='5' class='answer' style='width: 800px; height: 400px'/><div class='tbutton help' name='help'>?</div><div class='tbutton ok' name='prompt_done'>OK</div></div>");
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
    $('.tbutton.ok', $div).click(eHandler);
    $('.tbutton.help', $div).click(util.guard(function(ev) {
        $('.dialog.prompt2').detach();
        var $div2 = $("<div class='dialog prompt2 functions-list'><span class='closebox buttonbox'></span><div class='scroll'><div class='text' style='font-size: 70%'></div></div></div>");
        $div2.prependTo($div);

        $('span.closebox', $div2).click(function() {
            $div2.remove();
        });

        $('div.text', $div2).on('mouseover', '.func-arg', function (e) {
            console.log("enter");
            console.log($(this));
            $(this).addClass('fake-hover');
            $(this).parents('.func-arg').removeClass('fake-hover');
            e.stopPropagation();
        });

        $('div.text', $div2).on('mouseout', '.func-arg', function (e) {
            console.log("leave");
            console.log($(this));
            $(this).removeClass('fake-hover');
            e.stopPropagation();
        });

        var xhr = new XMLHttpRequest();
        xhr.onreadystatechange = function() {
            if (xhr.readyState == 4) {
                var data = JSON.parse(xhr.responseText);
                var html = _.map(data, function(val, key) {
                    var t = document.querySelector('#function-help-template');
                    var clone = document.importNode(t.content, true);
                    $(clone.querySelector('.function-help span.name')).html(key);
                    $(clone.querySelector('.function-help span.description')).html(val.description);
                    $(clone.querySelector('.function-help span.type')).html(makeTypeHtml(val.type));
                    if (_.has(val, 'aliases')) {
                        $(clone.querySelector('.function-help span.aliases')).html(val.aliases);
                    }
                    return clone;
                });
                $('div.text', $div2).append(html);

                var menuItems = [
                        {header: 'Query Tools'},
                        {text: 'Info', action: function(e) {
                                var selection = window.getSelection();
                                var container = selection.focusNode.parentElement;
                                console.log(selection);
                                console.log(selection.toString());
                                console.log(container);
                                var selectedText = _.map($(selection.focusNode).closest('.func-arg'), function (v) { return $(v); } )[0][0].textContent;
                                console.log(selectedText);
                                var mkDiv = function() {
                                    $('.dialog.prompt3').detach();
                                    var $div3 = $("<div class='dialog prompt3 functions-info'><span class='closebox buttonbox'></span><div class='scroll'><div class='text' style='font-size: 70%'></div></div></div>");
                                    $div3.prependTo($div2);

                                    $('span.closebox', $div3).click(function() {
                                        $div3.remove();
                                    });
                                    return $div3;
                                };
                                requestFunctionHelp(mkDiv, selection);
                            }
                        },
                    ];
                context.attach($('div.text', $div2), menuItems);
            }
        }
        xhr.open('GET', '/help', true);
        xhr.send();

    }));
    return $div;
};

var makeTypeHtml = function(val) {
    var dispatch = { 'text': makeTypeHtmlText,
                     'tag': makeTypeHtmlTag,
                     'func': makeTypeHtmlFunc,
                     'fn': makeTypeHtmlFn,
    };
    return dispatch[val.t](val, dispatch);
};

var spanWrap = function(t) {
    return "<span>" + t + "<span>";
};

var makeTypeHtmlText = function(val, dispatch) {
    return "<div class='func-arg'>" + spanWrap(val.rep) + "</div>";
};
var makeTypeHtmlTag = function(val, dispatch) {
    var leftHtml = dispatch[val.left.t](val.left, dispatch);
    var rightHtml = dispatch[val.right.t](val.right, dispatch);

    return "<div class='func-arg'>" + spanWrap(val.outerL) + leftHtml + spanWrap(val.sep) + rightHtml + spanWrap(val.outerR) + "</div>";
};
var makeTypeHtmlFunc = function(val, dispatch) {
    var leftHtml = dispatch[val.left.t](val.left, dispatch);
    var rightHtml = dispatch[val.right.t](val.right, dispatch);

    return "<div class='func-arg'>" + spanWrap(val.leftOuterL) + leftHtml + spanWrap(val.leftOuterR) + spanWrap(val.sep) + rightHtml + "</div>";

};
var makeTypeHtmlFn = function(val, dispatch) {
    var leftHtml = dispatch[val.left.t](val.left, dispatch);
    var rightHtml = dispatch[val.right.t](val.right, dispatch);

    return "<div class='func-arg'>" + leftHtml + spanWrap(val.sep) + rightHtml + "</div>";
};

var requestType = function($div, selection) {
    var xhr = new XMLHttpRequest();
    xhr.onreadystatechange = function() {
        if (xhr.readyState == 4) {
            var data = JSON.parse(xhr.responseText);
            var html = _.map(data, function(val) {
                var t = document.querySelector('#function-type-template');
                var clone = document.importNode(t.content, true);
                $(clone.querySelector('.function-type span.name')).html(val.name);
                $(clone.querySelector('.function-type span.type')).html(makeTypeHtml(val.type));
                $(clone.querySelector('.function-type span.args-needed')).html(val.argsNeeded);
                return clone;
            });
            $('div.text', $div).append(html);
        }
    }
    xhr.open('POST', '/function_help/type', true);
    var selectedText = _.map($(selection.focusNode).closest('.func-arg'), function (v) { return $(v); } )[0][0].textContent;
    xhr.send(selectedText);
}

var requestName = function($div, selection) {
//    var xhr = new XMLHttpRequest();
//    xhr.onreadystatechange = function() {
//        if (xhr.readyState == 4) {
//            var data = JSON.parse(xhr.responseText);
//            var html = _.map(data, function(val) {
//                var t = document.querySelector('#function-type-template');
//                var clone = document.importNode(t.content, true);
//                $(clone.querySelector('.function-type span.name')).html(val.name);
//                $(clone.querySelector('.function-type span.type')).html(val.type);
//                $(clone.querySelector('.function-type span.args-needed')).html(val.argsNeeded);
//                return clone;
//            });
//            $('div.text', $div3).append(html);
//        }
//    }
//    xhr.open('POST', '/function_help/type', true);
//    xhr.send(selection.toString());
}

var requestFunctionHelp = function(mkDiv, selection) {
    var dispatch = { 'type': requestType,
                     'name': requestName};
    var onDispatch = $(selection.focusNode).closest('.field-help').find('span')[0].className;
    if (_.has(dispatch, onDispatch) ) {
        dispatch[onDispatch](mkDiv(), selection);
    }
};


var singleFieldDialog = function(prmp, cb) {
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
    $('.answer', $div).val('.+host[.](.+)$');
    var eHandler = util.guard(function(ev) {
        ev.stopPropagation();
        if(cb($('.answer', $div).val()) !== false) {
            $div.remove();
        }
    });
    $('.tbutton', $div).click(eHandler);
    return $div;
};

return GraphSurface;
});
