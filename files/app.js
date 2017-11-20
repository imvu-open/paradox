requirejs.config({
    "baseUrl": "files",
    "paths": {
      "app": "app",
      "jquery": "jquery-2.1.1",
      "jquery-ui": "jquery-ui-1.11.4/jquery-ui",
      "jquery-cookie": "jquery-cookie-1.4.1",
      "sha256": "sha256",
      "istatd-legend": "istatd-legend",
      "dygraph": "dygraph-combined",
      "hairlines": "extras/hairlines",
      "imvu-hairlines": "extras/imvu-hairlines",
      "super-annotations": "extras/super-annotations",
      "underscore": "underscore-min",
      "backbone": "backbone-min",
      "jstree": "jstree.min",
      "graph": "graph",
      "domReady": "domReady",
      "loader": "loader",
      "widget": "widget",
      "graphsurface": "graphsurface",
      "context": "context",
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
    "sha256",
    "dygraph",
    "imvu-hairlines",
    "super-annotations",
    "jstree",
    "context",
], function() {
    require(["domReady", "graph", "context"], function(domReady, on_ready) {
        domReady(on_ready);
    });
});

