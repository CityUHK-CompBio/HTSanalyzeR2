HTMLWidgets.widget(global = {
    name: "forceGraph",
    type: "output",
    store: {currentKey: ""},

    getElementState: function(el) {
        var elId = el.id;
        if (!(elId in global.store)) {
            global.store[elId] = {elId: elId};
        }
        return global.store[elId];
    },

    getCurrentConfig: function(state, x) {
        if(x) { state.currentKey = JSON.stringify(x).hashCode().toString(); }

        if(state.currentKey == "") {
            return;
        }

        if(!(state.currentKey in state)) {
            state[state.currentKey] = {
                colors: {
                    red: [158, 22, 23],
                    blue: [0, 106, 156],
                    grey: [200,200,200],
                    white: [255,255,255],
                    edge: [46, 219, 86]
                }
            };
        }

        return state[state.currentKey];
    },

    initialize: function(el, width, height) {
        // console.log("====================   initialize   ========================");
        el.style.height = "90vh";
        // var state = global.getElementState(el);
    },

    resize: function(el, width, height) {
        // console.log("====================   resize   ========================");
        // var state = global.getElementState(el);
    },

    renderValue: function(el, x, simulation) {
        // console.log("====================   renderValue   ========================");
        // console.log(el);
        console.log(x);

        var state = global.getElementState(el);
        state.container = el;

        if (x.update) {
            global.update(state, x);
        } else {
            global.construct(state, x);
        }
    },

    construct: function(state, x) {
        console.log("======================   construct   ========================");
        // console.log(state);

        var current = global.getCurrentConfig(state)
        if ("sigma" in current) {
            s = current.sigma;
            s.kill();
        }

        var current = global.getCurrentConfig(state, x)
        var container = state.container;
        container.innerHTML = '';

        var colors = current.colors;

        var g = { nodes: [], edges: [] };

        if ("graph" in current) {
            g = current.graph;
        } else {
            var i, s;

            N = x.nodes.id.length;
            E = x.links.source.length;

            for(i =0; i < N; i++) {
                g.nodes.push({
                    id: x.nodes.id[i],
                    label : x.nodes.label[i],
                    x: Math.cos(2 * i * Math.PI / N ),
                    y: Math.sin(2 * i * Math.PI / N + Math.PI),
                    size: 0 + x.nodes.size[i],
                    scheme: x.nodes.scheme[i]
                });
            }

            for(i = 0; i < E; i++) {
                g.edges.push({
                    id: 'e' + i,
                    source: x.links.source[i],
                    target: x.links.target[i],
                    size: x.links.weight[i] / 2,
                });
            }

            // Color
            for(i =0; i < N; i++) {
                var c = colors.grey;



                if(x.options.type == "GSCA") {
                    if(x.nodes.scheme[i] == 'Pos') {
                        c = _interpolateColor(colors.red, colors.white, x.nodes.color[i] / 0.05);
                    } else if (x.nodes.scheme[i] == 'Neg') {
                        c = _interpolateColor(colors.blue, colors.white, x.nodes.color[i] / 0.05);
                    }
                } else if (x.options.type == "NWA") {
                    if(x.nodes.scheme[i] == 'Pos') {
                        c = _interpolateColor(colors.red, colors.blue, x.nodes.color[i] / 9);
                    } else if (x.nodes.scheme[i] == 'Neg') {
                        c = _interpolateColor(colors.blue, colors.white, x.nodes.color[i] / 9);
                    }
                }

                g.nodes[i].color = r2rgba(c, 0.9);
            }

        }

        s = new sigma({
          graph: g,
          renderer: {
            container: container,
            type: 'canvas',
          },
          settings: {
            clone: false,
            skipErrors: true,
            // maxNodeSize: 30,
            // maxEdgeSize: 8,
            // minEdgeSize: 1,
            maxNodeSize: 10,
            maxEdgeSize: 2,
            minEdgeSize: 1,

            edgeColor: 'default',
            defaultEdgeColor: r2rgba([46, 219, 86], 0.2),

            nodeOuterBorderColor: 'default',
            nodeOuterBorderSize: 1,
            defaultNodeOuterBorderColor: '#eee',

            enableEdgeHovering: false,
            borderSize: 2,
            outerBorderSize: 3,
            nodeHaloColor: 'rgba(236, 81, 72, 0.1)',
            nodeHaloSize: 30,
          }
        });

        var config = {  
            linLogMode:false,
            strongGravityMode:false,
            outboundAttractionDistribution: false,
            adjustSizes:false,
            barnesHutOptimize: false,
            
            gravity:30,
            barnesHutTheta:0.1,
            edgeWeightInfluence:0,
            startingIterations: 1,
            iterationsPerRender: 1,
            slowDown: 200,

            autoStop:true,
            avgDistanceThreshold:1e-6,
            // maxIterations:100000,
            easing:'quadraticInOut'
        };
        sigma.layouts.startForceLink(s, config);

        // setTimeout(function(){
        //     sigma.layouts.configForceLink(s, {slowDown: 500}) 
        // }, 2000);


        // LASSO
        var activeState = sigma.plugins.activeState(s);
        var keyboard = sigma.plugins.keyboard(s, s.renderers[0]);

        // Initialize the Select plugin:
        var select = sigma.plugins.select(s, activeState);
        select.bindKeyboard(keyboard);

        // Initialize the dragNodes plugin:
        var dragListener = sigma.plugins.dragNodes(s, s.renderers[0], activeState);

        // Initialize the lasso plugin:
        var lasso = new sigma.plugins.lasso(s, s.renderers[0], {
          'strokeStyle': 'rgb(236, 81, 72)',
          'lineWidth': 2,
          'fillWhileDrawing': true,
          'fillStyle': 'rgba(236, 81, 72, 0.2)',
          'cursor': 'crosshair'
        });

        select.bindLasso(lasso);

        // halo on active nodes:
        function renderHalo() {
          s.renderers[0].halo({
            nodes: activeState.nodes()
          });
        }

        s.renderers[0].bind('render', function(e) {
          renderHalo();
        });

        //"spacebar" + "s" keys pressed binding for the lasso tool
        keyboard.bind('32+83', function() {
          if (lasso.isActive) {
            lasso.deactivate();
          } else {
            lasso.activate();
          }
        });

        // Listen for selectedNodes event
        lasso.bind('selectedNodes', function (event) {
          setTimeout(function() {
            lasso.deactivate();
            s.refresh({ skipIdexation: true });
          }, 0);
        });

        current.sigma = s;
        current.graph = g;
        current.data = x;
        current.scheme = x.options.nodeScheme;
        current.type = x.options.type;

        global.generateControllers(state);
        configureSettingPanel(state);
    },

    update: function(state, u) {
        var current = global.getCurrentConfig(state)

        var i, s;
        var colors = current.colors

        g = current.graph;
        s = current.sigma;
        x = current.data;
        type = current.type;
        

        for (i = 0; i < g.nodes.length; i++) {
            var c = colors.grey;

            if (type == "GSCA") {
                tick = x.options.seriesData[u.process_map - 1];
                g.nodes[i].scheme = x.nodes["scheme." + tick][i];
                if(g.nodes[i].scheme == 'Pos') {
                    c = _interpolateColor(colors.red, colors.white, x.nodes["color." + tick][i] / 0.02);
                } else if (g.nodes[i].scheme == 'Neg') {
                    c = _interpolateColor(colors.blue, colors.white, x.nodes["color." + tick][i] / 0.02);
                }
            } else if(type == "NWA"){
                tick = x.options.seriesData[u.process_net - 1];
                g.nodes[i].scheme = x.nodes["scheme." + tick][i];
                if(g.nodes[i].scheme == 'Pos') {
                    c = _interpolateColor(colors.red, colors.white, x.nodes["color." + tick][i] / 9);
                } else if (g.nodes[i].scheme == 'Neg') {
                    c = _interpolateColor(colors.blue, colors.white, x.nodes["color." + tick][i] / 9);
                }
            }

            g.nodes[i].color = r2rgba(c, 0.9);
        }

        if (!sigma.layouts.isForceLinkRunning()) {
        //     sigma.layouts.startForceLink(s, config);
            s.refresh();    
        }
        
    },

    generateControllers: function(state) {
        console.log("===============================generate Controllers===============================")
        console.log(state);
        var current = global.getCurrentConfig(state);
        console.log(current);
        state.controller = {};

        var s = current.sigma;
        var g = current.graph;
        var x = current.data;

        function reset() {
            sigma.layouts.stopForceLink();

            for (i = 0; i < g.nodes.length; i++) {
                g.nodes[i].x = Math.cos(2 * i * Math.PI / N );
                g.nodes[i].y = Math.sin(2 * i * Math.PI / N + Math.PI);
            }

            s.refresh();
        }

        function refresh() {
            sigma.layouts.configForceLink(s, {slowDown: current.layout.slowDown});
            if(!sigma.layouts.isForceLinkRunning()) {
                sigma.layouts.startForceLink(s);
            }
            s.refresh();
        }


        // Layout
        current.layout = {}
        current.layout.linLogMode = false;
        current.layout.strongGravityMode = false
        current.layout.outboundAttractionDistribution = false
        current.layout.adjustSizes = false
        current.layout.barnesHutOptimize = false
        current.layout.gravity = 30
        current.layout.barnesHutTheta = 0.1
        current.layout.edgeWeightInfluence = 0
        current.layout.slowDown = 200

        state.controller.linLogMode = function(val) {
            current.layout.linLogMode = val;
            sigma.layouts.configForceLink(s, {linLogMode:val});
            refresh();
        }

        state.controller.strongGravityMode = function(val) {
            current.layout.strongGravityMode = val;
            sigma.layouts.configForceLink(s, {strongGravityMode:val});
            refresh();
        }

        state.controller.outboundAttractionDistribution = function(val) {
            current.layout.outboundAttractionDistribution = val;
            sigma.layouts.configForceLink(s, {outboundAttractionDistribution:val});
            refresh();
        }

        state.controller.adjustSizes = function(val) {
            current.layout.adjustSizes = val;
            sigma.layouts.configForceLink(s, {adjustSizes:val});
            refresh();
        }

        state.controller.barnesHutOptimize = function(val) {
            current.layout.barnesHutOptimize = val;
            sigma.layouts.configForceLink(s, {barnesHutOptimize:val});
            refresh();
        }

        state.controller.gravity = function(val) {
            current.layout.gravity = val;
            sigma.layouts.configForceLink(s, {gravity:val});
            refresh();
        }

        state.controller.barnesHutTheta = function(val) {
            current.layout.barnesHutTheta = val;
            sigma.layouts.configForceLink(s, {barnesHutTheta:val});
            refresh();
        }

        state.controller.edgeWeightInfluence = function(val) {
            current.layout.edgeWeightInfluence = val;
            sigma.layouts.configForceLink(s, {edgeWeightInfluence:val});
            refresh();
        }

        state.controller.slowDown = function(val) {
            current.layout.slowDown = val;
            sigma.layouts.configForceLink(s, {slowDown:val});
            refresh();
        }

        // Label
        current.label = {}
        current.label.opacity = 1;
        current.label.color = "#000000"
        current.label.scale = 1
        state.controller.labelOption = function(val) {
            s.settings("drawLabels", val != 'none');
            if(val != 'none') {
                for (i = 0; i < g.nodes.length; i++) {
                    g.nodes[i].label = x.nodes["label_" + val][i];
                }
            }
            s.refresh();
        }
        state.controller.labelColor = function(val) {
            current.label["color"] = val;
            s.settings("defaultLabelColor", r2rgba(h2r(val), current.label.opacity));
            s.refresh();
        }
        state.controller.labelOpacity = function(val) {
            current.label["opacity"] = val;
            s.settings("defaultLabelColor", r2rgba(h2r(current.label.color), val));
            s.refresh();
        }
        state.controller.labelScale = function(val) {
            current.label["scale"] = val;
            s.settings("defaultLabelSize", 14 * val);
            s.refresh();
        }

        // Node
        current.node = {}
        current.node.scale = 1
        current.node.color = "#000000"
        current.node.opacity = 1;

        state.controller.nodeScale = function(val) {
            console.log(val);
            current.node["scale"] = val;
            for (i = 0; i < g.nodes.length; i++) {
                g.nodes[i].size = x.nodes.size[i] * val;
            }
            s.settings("maxNodeSize", 30 * val);
            s.refresh();
        }

        state.controller.nodeBorderColor = function(val) {
            console.log(val);
            current.node["color"] = val;
            s.settings("defaultNodeBorderColor", r2rgba(h2r(val), current.node.opacity));
            s.refresh();
        }
        state.controller.nodeBorderOpacity = function(val) {
            console.log(val);
            current.node["opacity"] = val;
            s.settings("defaultNodeBorderColor", r2rgba(h2r(current.node.color), val));
            s.refresh();
        }
        state.controller.nodeBorderWidth = function(val) {
            console.log(val);
            current.node["scale"] = val;
            s.settings("nodeBorderSize", 2 * val);
            s.refresh();
        }
        

        // Edge
        current.edge = {}
        current.edge.scale = 1
        current.edge.color = "#000000"
        current.edge.opacity = 1;

        state.controller.edgeScale = function(val) {
            console.log(val);
            current.edge["scale"] = val;
            for (i = 0; i < g.edges.length; i++) {
                g.edges[i].size = x.links.weight[i] * val / 2;
            }
            s.settings("maxEdgeSize", 8 * val);
            s.refresh();
        }

        state.controller.edgeColor = function(val) {
            console.log(val);
            current.edge["color"] = val;
            s.settings("defaultEdgeColor", r2rgba(h2r(val), current.edge.opacity));
            s.refresh();
        }
        state.controller.edgeOpacity = function(val) {
            console.log(val);
            current.edge["opacity"] = val;
            s.settings("defaultEdgeColor", r2rgba(h2r(current.edge.color), val));
            s.refresh();
        }
        
        // Color Scheme
        // current.scheme = "dual" or "linear"
        state.controller.scheme = function(schemeId, domain, range) {
            if (schemeId.startsWith(current.scheme)) {
                var interpolate = function(val) {
                    var f = (val - domain[0]) / (domain[1] - domain[0]);
                    return _interpolateColor(h2r(range[0]), h2r(range[1]), f);
                }
                if (schemeId.startsWith("dual")) {
                    var sch = schemeId.replace("dual", "");
                    for (i = 0; i < g.nodes.length; i++) {
                        if(g.nodes[i].scheme == sch) {
                            g.nodes[i].color = r2rgba(interpolate(x.nodes.color[i]), 0.9);
                        }
                    }
                }
            }
            s.refresh();
        }

        // Buttons
        state.controller.pause = function() {
            sigma.layouts.stopForceLink();
        }

        state.controller.saveSVG = function() {
            console.log("savesvg");
            s.toSVG({download: true, labels:true, filename: 'network.svg', size: 2000});
        }

        state.controller.refresh = function() {
            sigma.layouts.startForceLink(s);
        }

    }
});
