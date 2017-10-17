HTMLWidgets.widget(global = {
    name: "forceGraph",
    type: "output",
    store: {},

    getElementState: function(el) {
        var elId = el.id;
        if (!(elId in global.store)) {
            global.store[elId] = {elId: elId, currentKey: "default"};
        }
        return global.store[elId];
    },

    getCurrentConfig: function(state, x) {
        if(x) { state.currentKey = JSON.stringify(x).hashCode().toString(); }

        if(!(state.currentKey in state)) {
            state[state.currentKey] = {
                settings: {
                    minNodeSize: 3,
                    minEdgeSize: 1,
                    // maxNodeSize: 40,
                    // maxEdgeSize: 8,
                    maxNodeSize: 10,
                    maxEdgeSize: 2
                },
                layout: {
                    linLogMode: false,
                    strongGravityMode: false,
                    outboundAttractionDistribution: false,
                    adjustSizes: false,
                    barnesHutOptimize: false,
                    gravity: 30,
                    barnesHutTheta: 0.1,
                    edgeWeightInfluence: 0,
                    slowDown: 200
                },
                label: {
                    opacity: 1,
                    color: "#000000",
                    scale: 1,
                },
                node: {
                    scale: 1,
                    opacity: 0.9,
                    borderColor: "#E6E6E6",
                    borderOpacity: 0.7,
                    borderWidth: 1,
                    NANodeColor: "#EDEDED",
                    NANodeOpacity: 0.9,
                },
                edge : {
                    scale: 1,
                    color: "#C6C6C6",
                    opacity: 0.4,
                },
                scheme: {
                    dual: {
                        Pos: {
                            domain:[0, 1],
                            range:["#9E1617", "#FFFFFF"]
                        },
                        Neg: {
                            domain:[0, 1],
                            range:["#006A9C", "#FFFFFF"]
                        }
                    }
                }
            };
        }

        return state[state.currentKey];
    },

    initialize: function(el, width, height) {
        // console.log("====================   initialize   ========================");
        el.style.height = "90vh";
        var state = global.getElementState(el);
        state.container = el;
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
        // state.container = el;

        if (x.update) {
            global.update(state, x);
        } else {
            global.construct(state, x);
        }
    },

    construct: function(state, x) {
        // console.log("======================   construct   ========================");
        // console.log(state);

        var present = global.getCurrentConfig(state);
        if ("sigma" in present) {
            present.sigma.kill();
            present.sigma = null;
        }
        state.container.innerHTML = '';

        current = global.getCurrentConfig(state, x);
        global.mergeConfig(current, x);


        var g = null;
        if ("graph" in current) {
            g = current.graph;
        } else {
            g = { nodes: [], edges: [] };
            N = x.nodes.id.length;
            E = x.links.source.length;

            for(var i =0; i < N; i++) {
                g.nodes.push({
                    id: x.nodes.id[i],
                    label : x.nodes.label[i],
                    x: Math.cos(2 * i * Math.PI / N ),
                    y: Math.sin(2 * i * Math.PI / N + Math.PI),
                    size: 0 + x.nodes.size[i],
                    scheme: x.nodes.scheme[i]
                });
            }

            for(var i = 0; i < E; i++) {
                g.edges.push({
                    id: 'e' + i,
                    source: x.links.source[i],
                    target: x.links.target[i],
                    size: x.links.weight[i]
                });
            }

            // Color
            for(i =0; i < N; i++) {
                if(x.nodes.scheme[i] != null) {
                    var palette = current.scheme.dual[x.nodes.scheme[i]];
                    c = _iterpolatePalette(palette, x.nodes.color[i]);
                    g.nodes[i].color = h2rgba(c, current.node.opacity);
                } else {
                    g.nodes[i].color = h2rgba(current.node.NANodeColor, current.node.NANodeOpacity);
                }
            }
        }

        var s = new sigma({
          graph: g,
          renderer: {
            container: state.container,
            type: 'canvas',
          },
          settings: {
            clone: false,
            skipErrors: true,

            minNodeSize: current.settings.minNodeSize,
            minEdgeSize: current.settings.minEdgeSize,
            maxNodeSize: current.settings.maxNodeSize,
            maxEdgeSize: current.settings.maxEdgeSize,

            edgeColor: 'default',
            defaultEdgeColor: h2rgba(current.edge.color, current.edge.opacity),

            nodeBorderColor: 'default',
            nodeBorderSize: current.node.borderWidth,
            defaultNodeBorderColor: h2rgba(current.node.borderColor, current.node.borderOpacity),

            defaultLabelSize: 14 * current.label.scale,
            defaultLabelColor: h2rgba(current.label.color, current.label.opacity),

            enableEdgeHovering: false,
            borderSize: 2,
            outerBorderSize: 3,
            nodeHaloColor: 'rgba(236, 81, 72, 0.1)',
            nodeHaloSize: 30,
          }
        });

        var forceConfig = {
            linLogMode: current.layout.linLogMode,
            strongGravityMode:current.layout.strongGravityMode,
            outboundAttractionDistribution: current.layout.outboundAttractionDistribution,
            adjustSizes:current.layout.adjustSizes,
            barnesHutOptimize: current.layout.barnesHutOptimize,

            gravity:current.layout.gravity,
            barnesHutTheta:current.layout.barnesHutTheta,
            edgeWeightInfluence:current.layout.edgeWeightInfluence,
            slowDown: current.layout.slowDown,
            startingIterations: 1,
            iterationsPerRender: 1,

            autoStop:true,
            avgDistanceThreshold:1e-6,
            // maxIterations:100000,
            easing:'quadraticInOut'
        };
        sigma.layouts.startForceLink(s, forceConfig);

        // Initialize the activeState plugin:
        var activeState = sigma.plugins.activeState(s);
        var keyboard = sigma.plugins.keyboard(s, s.renderers[0]);
        // Initialize the select plugin:
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

        current.graph = g;
        current.sigma = s;
        current.data = x;
        current.type = x.options.type;
        global.generateControllers(state, current);

        configureSettingPanel(state, current);
    },

    update: function(state, u) {
        // console.log("====================   Update    ========================");
        var current = global.getCurrentConfig(state)

        var g = current.graph;
        var s = current.sigma;
        var x = current.data;
        var type = current.type;

        for(i =0; i < g.nodes.length; i++) {
            var tick = x.options.seriesData[u.process - 1];
            g.nodes[i].theme = x.nodes["scheme." + tick][i];

            if(g.nodes[i].theme != null) {
                var palette = current.scheme.dual[g.nodes[i].theme];
                c = _iterpolatePalette(palette, x.nodes.color[i]);
                g.nodes[i].color = h2rgba(c, current.node.opacity);
            } else {
                g.nodes[i].color = h2rgba(current.node.NANodeColor, current.node.NANodeOpacity);
            }
        }

        if (!sigma.layouts.isForceLinkRunning()) {
            s.refresh();    
        }
    },

    mergeConfig: function(config, x) {
        if ("Pos" in x.options.colorDomain) {
            config.scheme.dual.Pos.domain = x.options.colorDomain.Pos;      
        }
        if ("Neg" in x.options.colorDomain) {
            config.scheme.dual.Neg.domain = x.options.colorDomain.Neg;      
        }
        if(x.options.type == "GSCA") {
            config.settings.maxNodeSize = 40;
            config.settings.maxEdgeSize = 8;
        } else if (x.options.type == "GSCA") {
            config.settings.maxNodeSize = 10;
            config.settings.maxEdgeSize = 2;
        }
    },

    generateControllers: function(state, current) {
        // console.log("============================ generate Controllers ============================")

        var s = current.sigma;
        var g = current.graph;
        var x = current.data;
        current.controllers = {};

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
        current.controllers.linLogMode = function(val) {
            current.layout.linLogMode = val;
            sigma.layouts.configForceLink(s, {linLogMode:val});
            refresh();
        }

        current.controllers.strongGravityMode = function(val) {
            current.layout.strongGravityMode = val;
            sigma.layouts.configForceLink(s, {strongGravityMode:val});
            refresh();
        }

        current.controllers.outboundAttractionDistribution = function(val) {
            current.layout.outboundAttractionDistribution = val;
            sigma.layouts.configForceLink(s, {outboundAttractionDistribution:val});
            refresh();
        }

        current.controllers.adjustSizes = function(val) {
            current.layout.adjustSizes = val;
            sigma.layouts.configForceLink(s, {adjustSizes:val});
            refresh();
        }

        current.controllers.barnesHutOptimize = function(val) {
            current.layout.barnesHutOptimize = val;
            sigma.layouts.configForceLink(s, {barnesHutOptimize:val});
            refresh();
        }

        current.controllers.gravity = function(val) {
            current.layout.gravity = val;
            sigma.layouts.configForceLink(s, {gravity:val});
            refresh();
        }

        current.controllers.barnesHutTheta = function(val) {
            current.layout.barnesHutTheta = val;
            sigma.layouts.configForceLink(s, {barnesHutTheta:val});
            refresh();
        }

        current.controllers.edgeWeightInfluence = function(val) {
            current.layout.edgeWeightInfluence = val;
            sigma.layouts.configForceLink(s, {edgeWeightInfluence:val});
            refresh();
        }

        current.controllers.slowDown = function(val) {
            current.layout.slowDown = val;
            sigma.layouts.configForceLink(s, {slowDown:val});
            refresh();
        }

        // Label
        current.controllers.labelOption = function(val) {
            s.settings("drawLabels", val != 'none');
            if(val != 'none') {
                for (i = 0; i < g.nodes.length; i++) {
                    g.nodes[i].label = x.nodes["label_" + val][i];
                }
            }
            s.refresh();
        }
        current.controllers.labelColor = function(val) {
            current.label["color"] = val;
            s.settings("defaultLabelColor", r2rgba(h2r(val), current.label.opacity));
            s.refresh();
        }
        current.controllers.labelOpacity = function(val) {
            current.label["opacity"] = val;
            s.settings("defaultLabelColor", r2rgba(h2r(current.label.color), val));
            s.refresh();
        }
        current.controllers.labelScale = function(val) {
            current.label["scale"] = val;
            s.settings("defaultLabelSize", 14 * val);
            s.refresh();
        }

        // Node
        current.controllers.nodeScale = function(val) {
            current.node["scale"] = val;
            for (i = 0; i < g.nodes.length; i++) {
                g.nodes[i].size = x.nodes.size[i] * val;
            }
            s.settings("maxNodeSize", 30 * val);
            s.refresh();
        }
        current.controllers.nodeOpacity = function(val) {
            current.node["opacity"] = val;
            for (i = 0; i < g.nodes.length; i++) {
                if(g.nodes[i].scheme != null) {
                    var palette = current.scheme.dual[g.nodes[i].scheme];
                    c = _iterpolatePalette(palette, x.nodes.color[i]);
                    g.nodes[i].color = h2rgba(c, current.node.opacity);
                }
            }
            s.refresh();
        }
        current.controllers.nodeBorderColor = function(val) {
            current.node["borderColor"] = val;
            s.settings("defaultNodeBorderColor", r2rgba(h2r(val), current.node.borderOpacity));
            s.refresh();
        }
        current.controllers.nodeBorderOpacity = function(val) {
            current.node["borderOpacity"] = val;
            s.settings("defaultNodeBorderColor", r2rgba(h2r(current.node.borderColor), val));
            s.refresh();
        }
        current.controllers.nodeBorderWidth = function(val) {
            current.node["borderWidth"] = val;
            s.settings("nodeBorderSize", val);
            s.refresh();
        }
        
        // Edge
        current.controllers.edgeScale = function(val) {
            current.edge["scale"] = val;
            for (i = 0; i < g.edges.length; i++) {
                g.edges[i].size = x.links.weight[i] * val;
            }
            s.settings("maxEdgeSize", 8 * val);
            s.refresh();
        }

        current.controllers.edgeColor = function(val) {
            current.edge["color"] = val;
            s.settings("defaultEdgeColor", r2rgba(h2r(val), current.edge.opacity));
            s.refresh();
        }
        current.controllers.edgeOpacity = function(val) {
            current.edge["opacity"] = val;
            s.settings("defaultEdgeColor", r2rgba(h2r(current.edge.color), val));
            s.refresh();
        }

        // Color Scheme
        current.controllers.scheme = function(schemeId, domain, range) {
            if (schemeId.startsWith("dual")) {
                var sch = schemeId.replace("dual", "");
                current.scheme.dual[sch].domain = domain;
                current.scheme.dual[sch].range = range;

                for (i = 0; i < g.nodes.length; i++) {
                    if(g.nodes[i].scheme == sch) {
                        var palette = current.scheme.dual[sch];
                        c = _iterpolatePalette(palette, x.nodes.color[i]);
                        g.nodes[i].color = h2rgba(c, current.node.opacity);
                    }
                }
            }
            s.refresh();
        }

        // Buttons
        current.controllers.pause = function() {
            console.log(this);
            sigma.layouts.stopForceLink();
        }

        current.controllers.refresh = function() {
            sigma.layouts.startForceLink(s);
        }

        current.controllers.saveSVG = function() {
            cur = global.getCurrentConfig(state)
            cur.sigma.toSVG({download: true, labels:true, filename: 'network.svg', size: 2000});
        }
    }
});
