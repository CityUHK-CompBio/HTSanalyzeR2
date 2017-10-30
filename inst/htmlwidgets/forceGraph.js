HTMLWidgets.widget(global = {
    name: "forceGraph",
    type: "output",
    store: {currentTab: null},

    getElementState: function(el) {
        var elId = el.id;
        if (!(elId in global.store)) {
            global.store[elId] = {elId: elId, container:el, currentKey: "default"};
        }

        return global.store[elId];
    },

    setConfig: function(state, x) {
        state.currentKey = JSON.stringify(x).hashCode().toString();
        if(!(state.currentKey in state)) {
             state[state.currentKey] = {
                settings: {
                    minNodeSize: 3,
                    minEdgeSize: 1,
                    // maxNodeSize: 40,
                    // maxEdgeSize: 8,
                    maxNodeSize: 10,
                    maxEdgeSize: 3
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
                    text: "id",
                    scale: 1,
                    opacity: 1,
                    color: "#000000",
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

    getConfig: function(state, x) {
        var key = state.currentKey;
        if(x) { key = JSON.stringify(x).hashCode().toString(); }

        if(!(key in state)) {
            return null;
        }
        return state[key];
    },

    mergeConfig: function(config, x) {
        if(x.options.type == "GSCA") {
            config.settings.maxNodeSize = 20;
            config.settings.minNodeSize = 3;
            config.settings.maxEdgeSize = 8;
        } else if (x.options.type == "NWA") {
            config.settings.maxNodeSize = 10;
            config.settings.maxEdgeSize = 2;
        }

        var options = x.options;
        var configurableKeys = ["settings", "layout", "label", "node", "edge"]; // "scheme"
        for(var ki in configurableKeys) {
            var key = configurableKeys[ki];
            if(key in options) {
                for (var sk in options[key]) {
                    if(options[key][sk] != null) {
                        config[key][sk] = options[key][sk];
                    }
                }
            }
        }

        // TODO: Use uneven scalers
        // if("scheme" in options) {}
        if ("Pos" in x.options.colorDomain) {
            config.scheme.dual.Pos.domain = x.options.colorDomain.Pos;
        }
        if ("Neg" in x.options.colorDomain) {
            config.scheme.dual.Neg.domain = x.options.colorDomain.Neg;
        }
    },

    currentSituation: function() {
        var situation = {};
        if(global.store.currentTab != null) {
            situation.el = global.store.currentTab;
            situation.state = global.getElementState(situation.el);
            situation.config = global.getConfig(situation.state);
        }
        return situation;
    },

    initialize: function(el, width, height) {
        console.log("====================   initialize   ========================");
        el.style.height = "85vh";
        global.store.currentTab = el;
        var initState = global.getElementState(el);
        initState.container = el;
        
        // TODO: rmarkdown.
        registerForceGraph(global);

        global.initHandlers();
    },

    resize: function(el, width, height) {
        console.log("====================   resize   ========================");
    },

    renderValue: function(el, x) {
        console.log("====================   renderValue   ========================");
        console.log(x);
        var state = global.getElementState(el);

        if (x.update) {
            global.update(state, x);
        } else {
            global.construct(state, x);
        }
    },

    switchTab: function(el) {
        console.log("====================   switchTab   ========================");
        if(global.store.currentTab != el) {
            global.store.currentTab = el;
            var state = global.getElementState(el);
            if(state.hasOwnProperty("supervisor")) {
                var sv = state.supervisor;
                sigma.layouts.killForceLink();
                sigma.layouts.startForceLink(sv.sigInst, sv.config);
                sv.sigInst.refresh();
            }
        }
    },

    construct: function(state, x) {
        console.log("======================   construct   ========================");
        // console.log(state);

        if(!state.hasOwnProperty("supervisor")) {
            var s = new sigma({
              graph: { nodes: [], edges: [] },
              renderer: {
                container: state.container,
                type: 'canvas',
              }
            });

            sigma.layouts.killForceLink();
            sigma.layouts.startForceLink(s);
            state.supervisor = sigma.layouts.stopForceLink();
        }

        config = global.setConfig(state, x);
        if(!config.hasOwnProperty("metadata")) {
            global.mergeConfig(config, x);
            var meta = {};
            
            var g = { nodes: [], edges: [] };
            N = x.nodes.id.length;
            E = x.links.source.length;

            for(var i =0; i < N; i++) {
                g.nodes.push({
                    id: x.nodes.id[i],
                    label : x.nodes.label[i],
                    x: Math.cos(2 * i * Math.PI / N ),
                    y: Math.sin(2 * i * Math.PI / N + Math.PI),
                    size: x.nodes.size[i] * config.node.scale,
                    scheme: x.nodes.scheme[i]
                });
            }
            for(i =0; i < N; i++) {
                // color
                if(x.nodes.scheme[i] != null) {
                    var palette = config.scheme.dual[x.nodes.scheme[i]];
                    c = _iterpolatePalette(palette, x.nodes.color[i]);
                    g.nodes[i].color = h2rgba(c, config.node.opacity);
                } else {
                    g.nodes[i].color = h2rgba(config.node.NANodeColor, config.node.NANodeOpacity);
                }
                // label
                if(x.nodes["label_" + config.label.text] != null) {
                    g.nodes[i].label = x.nodes["label_" + config.label.text][i];
                }
            }
            for(var i = 0; i < E; i++) {
                g.edges.push({
                    id: 'e' + i,
                    source: x.links.source[i],
                    target: x.links.target[i],
                    size: x.links.weight[i] * config.edge.scale
                });
            }

            var sigmaSettings = {
                clone: false,
                skipErrors: true,

                minNodeSize: config.settings.minNodeSize,
                minEdgeSize: config.settings.minEdgeSize,
                maxNodeSize: config.settings.maxNodeSize * config.node.scale,
                maxEdgeSize: config.settings.maxEdgeSize,

                
                edgeColor: 'default',
                defaultEdgeColor: h2rgba(config.edge.color, config.edge.opacity),

                nodeBorderColor: 'default',
                nodeBorderSize: config.node.borderWidth,
                defaultNodeBorderColor: h2rgba(config.node.borderColor, config.node.borderOpacity),

                drawLabels: config.label.text != "none",
                defaultLabelSize: 14 * config.label.scale,
                defaultLabelColor: h2rgba(config.label.color, config.label.opacity),
                labelThreshold: 0,

                enableEdgeHovering: false,
                borderSize: 2,
                outerBorderSize: 3,
                nodeHaloColor: 'rgba(236, 81, 72, 0.1)',
                nodeHaloSize: 30
            };

            var forceConfig = {
                linLogMode: config.layout.linLogMode,
                strongGravityMode:config.layout.strongGravityMode,
                outboundAttractionDistribution: config.layout.outboundAttractionDistribution,
                adjustSizes:config.layout.adjustSizes,
                barnesHutOptimize: config.layout.barnesHutOptimize,

                gravity:config.layout.gravity,
                barnesHutTheta:config.layout.barnesHutTheta,
                edgeWeightInfluence:config.layout.edgeWeightInfluence,
                slowDown: config.layout.slowDown,
                startingIterations: 1,
                iterationsPerRender: 1,

                autoStop:true,
                avgDistanceThreshold:1e-6,
                // maxIterations:100000,
                easing:'quadraticInOut'
            };

            meta.data = x;
            meta.graph = g;
            meta.sigmaSettings = sigmaSettings;
            meta.forceConfig = forceConfig;

            config.metadata = meta;
        }

        var sv = state.supervisor;
        var meta = config.metadata;

        sv.graph.clear();
        sv.graph.read(meta.graph);
        sv.config = meta.forceConfig;
        sv.sigInst.settings(meta.sigmaSettings);
        sigma.layouts.killForceLink();
        sigma.layouts.startForceLink(sv.sigInst, sv.config);
        sv.sigInst.refresh();


        // // Initialize the activeState plugin:
        // var activeState = sigma.plugins.activeState(s);
        // var keyboard = sigma.plugins.keyboard(s, s.renderers[0]);
        // // Initialize the select plugin:
        // var select = sigma.plugins.select(s, activeState);
        // select.bindKeyboard(keyboard);
        // // Initialize the dragNodes plugin:
        // var dragListener = sigma.plugins.dragNodes(s, s.renderers[0], activeState);

        // // Initialize the lasso plugin:
        // var lasso = new sigma.plugins.lasso(s, s.renderers[0], {
        //   'strokeStyle': 'rgb(236, 81, 72)',
        //   'lineWidth': 2,
        //   'fillWhileDrawing': true,
        //   'fillStyle': 'rgba(236, 81, 72, 0.2)',
        //   'cursor': 'crosshair'
        // });
        // select.bindLasso(lasso);

        // // halo on active nodes:
        // function renderHalo() {
        //   s.renderers[0].halo({
        //     nodes: activeState.nodes()
        //   });
        // }

        // s.renderers[0].bind('render', function(e) {
        //   renderHalo();
        // });

        // //"spacebar" + "s" keys pressed binding for the lasso tool
        // keyboard.bind('32+83', function() {
        //   if (lasso.isActive) {
        //     lasso.deactivate();
        //   } else {
        //     lasso.activate();
        //   }
        // });

        // // Listen for selectedNodes event
        // lasso.bind('selectedNodes', function (event) {
        //   setTimeout(function() {
        //     lasso.deactivate();
        //     s.refresh({ skipIdexation: true });
        //   }, 0);
        // });


   //      // refreshSettingPanel(state);
   //      // if (!("controllers" in state)) {
   //      //     global.generateControllers(state);
   //      //     configureSettingPanel(state);
   //      // }

        refreshSettingPanel(state, config);

        configureSettingHandlers(global.store.handlers);

    },

    update: function(state, u) {
        console.log("====================   Update    ========================");
        console.log(u);
        var config = global.getConfig(state);
        var sv = state.supervisor;
        var meta = config.metadata;

        var x = meta.data;
        var g = meta.graph;
        for(var i = 0; i < g.nodes.length; i++) {
            var tick = x.options.seriesData[u.process - 1];
            g.nodes[i].theme = x.nodes["scheme." + tick][i];

            if(g.nodes[i].theme != null) {
                var palette = config.scheme.dual[g.nodes[i].theme];
                c = _iterpolatePalette(palette, x.nodes.color[i]);
                g.nodes[i].color = h2rgba(c, config.node.opacity);
            } else {
                g.nodes[i].color = h2rgba(config.node.NANodeColor, config.node.NANodeOpacity);
            }
        }

        if (!sigma.layouts.isForceLinkRunning()) {
            sv.sigInst.refresh();
        }
    },

    initHandlers: function() {
        console.log("==================== Init Handlers ========================");
        
        if(global.store.hasOwnProperty("handlers")) {
            return;
        }
        global.store.handlers = {};
        var handlers = global.store.handlers;
        
        // Layout
        createLayoutRefreshFunc = function(key) {
            return function(val) {
                console.log(key + ": " + val);
                var cur = global.currentSituation();
                cur.config.layout[key] = val;
                cur.config.metadata.forceConfig[key] = val;
                sigma.layouts.startForceLink(cur.state.supervisor.sigInst, cur.state.supervisor.config);
            }
        }

        handlers.linLogMode = createLayoutRefreshFunc("linLogMode");
        handlers.strongGravityMode = createLayoutRefreshFunc("strongGravityMode");
        handlers.outboundAttractionDistribution = createLayoutRefreshFunc("outboundAttractionDistribution");
        handlers.adjustSizes = createLayoutRefreshFunc("adjustSizes");
        handlers.barnesHutOptimize = createLayoutRefreshFunc("barnesHutOptimize");

        handlers.gravity = createLayoutRefreshFunc("gravity");
        handlers.barnesHutTheta = createLayoutRefreshFunc("barnesHutTheta");
        handlers.edgeWeightInfluence = createLayoutRefreshFunc("edgeWeightInfluence");
        handlers.slowDown = createLayoutRefreshFunc("slowDown");

        // Label
        handlers.labelOption = function(val) {
            console.log("labelOption" + ": " + val);
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            cur.config.label.text = val;
            if(val != 'none') {
                for(var i = 0; i < meta.graph.nodes.length; i++) {
                    if(meta.data.nodes["label_" + val] != null) {
                        meta.graph.nodes[i].label = meta.data.nodes["label_" + val][i];
                    }
                }
            }
            sv.sigInst.settings("drawLabels", val != 'none')
            sv.sigInst.refresh();
        }
        handlers.labelScale = function(val) {
            console.log("labelScale" + ": " + val);
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;

            cur.config.label.scale = val;
            sv.sigInst.settings("defaultLabelSize", 14 * val);
            sv.sigInst.refresh();
        }
        handlers.labelColor = function(val) {
            console.log("labelColor" + ": " + val);
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;

            cur.config.label.color = val;
            sv.sigInst.settings("defaultLabelColor", hex2rgba(val));
            sv.sigInst.refresh();
        }

        // Node
        handlers.nodeScale = function(val) {
            console.log("nodeScale" + ": " + val);
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            cur.config.node.scale = val;
            for(var i = 0; i < meta.graph.nodes.length; i++) {
                meta.graph.nodes[i].size = meta.data.nodes.size[i] * val;
            }
            sv.sigInst.settings("maxNodeSize", cur.config.settings.maxNodeSize * val);
            sv.sigInst.refresh();
        }
        handlers.nodeOpacity = function(val) {
            console.log("nodeOpacity" + ": " + val);
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            cur.config.node.opacity = val;
            for(var i = 0; i < meta.graph.nodes.length; i++) {
                if(meta.graph.nodes[i].scheme != null) {
                    var palette = cur.config.scheme.dual[meta.graph.nodes[i].scheme];
                    c = _iterpolatePalette(palette, meta.data.nodes.color[i]);
                    meta.graph.nodes[i].color = h2rgba(c, val)
                }
            }
            sv.sigInst.refresh();
        }
        handlers.nodeBorderWidth = function(val) {
            console.log("nodeBorderWidth" + ": " + val);
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;

            cur.config.node.borderWidth = parseFloat(val);
            sv.sigInst.settings("nodeBorderSize", parseFloat(val));
            sv.sigInst.refresh();
        }
        handlers.nodeBorderColor = function(val) {
            console.log("nodeBorderColor" + ": " + val);
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;

            cur.config.node.borderColor = val;
            sv.sigInst.settings("defaultNodeBorderColor", hex2rgba(val));
            sv.sigInst.refresh();
        }

        // Edge
        handlers.edgeScale = function(val) {
            console.log("edgeScale" + ": " + val);
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            cur.config.edge.scale = val;
            for(var i = 0; i < meta.graph.edges.length; i++) {
                meta.graph.edges[i].size = meta.data.links.weight[i] * val;
             }
            sv.sigInst.settings("maxEdgeSize", 8 * val);
            sv.sigInst.refresh();
        }
        handlers.edgeColor = function(val) {
            console.log("edgeColor" + ": " + val);
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;

            cur.config.edge.color = val;
            sv.sigInst.settings("defaultEdgeColor", hex2rgba(val));
            sv.sigInst.refresh();
        }

        // // Color Scheme
        // state.controllers.scheme = function(schemeId, domain, range) {

        //     var cur = getConfig(state);
        //     if (schemeId.startsWith("dual")) {
        //         var sch = schemeId.replace("dual", "");
        //         cur.current.scheme.dual[sch].domain = domain;
        //         cur.current.scheme.dual[sch].range = range;

        //         for (i = 0; i < cur.g.nodes.length; i++) {
        //             if(cur.g.nodes[i].scheme == sch) {
        //                 var palette = cur.current.scheme.dual[sch];
        //                 c = _iterpolatePalette(palette, cur.x.nodes.color[i]);
        //                 cur.g.nodes[i].color = h2rgba(c, cur.current.node.opacity);
        //             }
        //         }
        //     }
        //     cur.s.refresh();
        // }

        // // Buttons
        // state.controllers.pause = function() {
        //     sigma.layouts.stopForceLink();
        // }

        // state.controllers.refresh = function() {
        //     var cur = getConfig(state);
        //     sigma.layouts.startForceLink(cur.s);
        // }

        // state.controllers.saveSVG = function() {
        //     var cur = getConfig(state);
        //     cur = global.getConfig(state)
        //     cur.sigma.toSVG({download: true, labels:true, filename: 'network.svg', size: 2000});
        // }
    }
});
