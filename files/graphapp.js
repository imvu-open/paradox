requirejs.config({
    "baseUrl": "files",
    "paths": {
      "graphapp": "graphapp",
      "jquery": "jquery-2.1.1",
      "jquery-ui": "jquery-ui-1.11.4/jquery-ui",
      "istatd-legend": "istatd-legend",
      "dygraph": "dygraph-combined",
      "hairlines": "extras/hairlines",
      "imvu-hairlines": "extras/imvu-hairlines",
      "super-annotations": "extras/super-annotations",
      "underscore": "underscore-min",
      "backbone": "backbone-min",
      "context": "context",
      //"graph": "graph",
      "domReady": "domReady",
      "loader": "loader",
      "widget": "widget",
      "graphsurface": "graphsurface",
    },
    shim: {
        "dygraph": {
            exports: 'Dygraph,DygraphCanvasRenderer,DygraphLayout,DygraphOptions'
        },
        "hairlines": {
            deps : ["dygraph"],
        },
        "imvu-hairlines": {
            deps : ["dygraph"],
        },
        "super-annotations": {
            deps : ["dygraph"],
        },
    }
});

// Load the main app module to start the app
//requirejs(["graph"]);
require([
    "jquery",
    "jquery-ui",
    "underscore",
    "backbone",
    "istatd-legend",
    "dygraph",
    "imvu-hairlines",
    "super-annotations",
    "context",
], function() {
    require(["domReady", "loader", "util", "widget", "graphsurface"], function(domReady, Loader, util, Widget, GraphSurface) {
        domReady(function() {

            console.log(Dygraph.PLUGINS);
            Dygraph.PLUGINS = Dygraph.PLUGINS.slice(1);
            Dygraph.PLUGINS.unshift(ISTATD_Legend);
            console.log(Dygraph.PLUGINS);
            // Remove the '?' at the start of the string and split out each assignment
            var qArgs = _.chain( location.search.slice(1).split('&') )
                // Split each array item into [key, value]
                // ignore empty string if search is empty
                .map(function(item) { if (item) return item.split('='); })
                // Remove undefined in the case the search is empty
                .compact()
                // Turn [key, value] arrays into object parameters
                .object()
                // Return the value of the chain operation
                .value();

            var tmpDate = util.getAdjustedNow();
            var dates = { 'start': new Date(tmpDate.getTime() - 3600*1000), 'stop': tmpDate };
            if (!_.isUndefined(qArgs.start) && !_.isUndefined(qArgs.stop)) {
                dates.start = new Date(qArgs.start);
                dates.stop = new Date(qArgs.stop);
            }else if (!_.isUndefined(qArgs.offset)) {
                dates.start = new Date(dates.stop.getTime() - qArgs.offset*1000);
            }
            loader = new Loader(dates);

            var widget = new Widget(this, $('#' + 'grid'), null);
            var theShittyGlobals = {
                    'theInteractionModel' : null,
                    'theGraphSize' : { width: 600, height: 240 },
                    'theGraphMaxSamples' : null,
                    'theOriginalDates' : { 'start': dates.start, 'stop': dates.stop },
                    'theCurrentDates' : { 'start': dates.start, 'stop': dates.stop },
                    'zoomOutIntervals' : util.zoomOutIntervals,
                };
            var $ret = $("<div class='graph'><span title='Show/Hide summary' class='summarybox buttonbox'/><span title='Settings for display' class='settingsbox buttonbox'/><span title='Settings for manual query' class='querybox buttonbox'/><span title='Restore default zoom' class='zoomoutbox buttonbox'/><span title='Close' class='closebox buttonbox'/><div class='legend'/><div class='graphdiv'></div><div class='summary'></div></div>");
            $ret.width(theShittyGlobals.theGraphSize.width);
            this.$self.append($ret);
            var width = $(window).width();
            var height = $(window).height() - 5;
            this.$self.css({position: 'absolute', width: width, left: 0});
            this.$self.height(height);
            var surface = new GraphSurface($ret, widget, loader, theShittyGlobals);

            var decodedQuery = decodeURIComponent(qArgs.query);
            var strs = decodedQuery.trim().split('\n');
            _.each(strs, function(q) {
                surface.toggleSeries(q);
            });
            surface.reload();
        });
    });
});

