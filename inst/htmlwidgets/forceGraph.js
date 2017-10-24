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

        var g = null;
        if (!("graph" in current)) {
            global.mergeConfig(current, x);

            g = { nodes: [], edges: [] };
            N = x.nodes.id.length;
            E = x.links.source.length;

            for(var i =0; i < N; i++) {
                g.nodes.push({
                    id: x.nodes.id[i],
                    label : x.nodes.label[i],
                    x: Math.cos(2 * i * Math.PI / N ),
                    y: Math.sin(2 * i * Math.PI / N + Math.PI),
                    size: x.nodes.size[i] * current.node.scale,
                    scheme: x.nodes.scheme[i]
                });
            }

            for(var i = 0; i < E; i++) {
                g.edges.push({
                    id: 'e' + i,
                    source: x.links.source[i],
                    target: x.links.target[i],
                    size: x.links.weight[i] * current.edge.scale
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
        } else {
            g = current.graph;
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
            maxNodeSize: current.settings.maxNodeSize * current.node.scale,
            maxEdgeSize: current.settings.maxEdgeSize,

            edgeColor: 'default',
            defaultEdgeColor: h2rgba(current.edge.color, current.edge.opacity),

            nodeBorderColor: 'default',
            nodeBorderSize: current.node.borderWidth,
            defaultNodeBorderColor: h2rgba(current.node.borderColor, current.node.borderOpacity),

            defaultLabelSize: 14 * current.label.scale,
            defaultLabelColor: h2rgba(current.label.color, current.label.opacity),
            labelThreshold: 0,

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

        refreshSettingPanel(state);
        if (!("controllers" in state)) {
            global.generateControllers(state);
            configureSettingPanel(state);
        }
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

    generateControllers: function(state) {
        // console.log("============================ generate Controllers ============================")

        // var s = cur.current.sigma;
        // var g = cur.current.graph;
        // var x = cur.current.data;
        // cur.current.controllers = {};

        if ("controllers" in state) {
            return;
        }
        state.controllers = {};

        function getConfig(state) {
            var config = global.getCurrentConfig(state);
            return {
                s: config.sigma,
                g: config.graph,
                x: config.data,
                current: config
            }
        }

        function reset() {
            var cur = getConfig(state);
            sigma.layouts.stopForceLink();
            for (i = 0; i < cur.g.nodes.length; i++) {
                cur.g.nodes[i].x = Math.cos(2 * i * Math.PI / N );
                cur.g.nodes[i].y = Math.sin(2 * i * Math.PI / N + Math.PI);
            }
            cur.s.refresh();
        }

        function refresh() {
            var cur = getConfig(state);
            sigma.layouts.configForceLink(cur.s, {slowDown: cur.current.layout.slowDown});
            if(!sigma.layouts.isForceLinkRunning()) {
                sigma.layouts.startForceLink(cur.s);
            }
            cur.s.refresh();
        }

        // Layout
        state.controllers.linLogMode = function(val) {
            var cur = getConfig(state);
            cur.current.layout.linLogMode = val;
            sigma.layouts.configForceLink(cur.s, {linLogMode:val});
            refresh();
        }

        state.controllers.strongGravityMode = function(val) {
            var cur = getConfig(state);
            cur.current.layout.strongGravityMode = val;
            sigma.layouts.configForceLink(cur.s, {strongGravityMode:val});
            refresh();
        }

        state.controllers.outboundAttractionDistribution = function(val) {
            var cur = getConfig(state);
            cur.current.layout.outboundAttractionDistribution = val;
            sigma.layouts.configForceLink(cur.s, {outboundAttractionDistribution:val});
            refresh();
        }

        state.controllers.adjustSizes = function(val) {
            var cur = getConfig(state);
            cur.current.layout.adjustSizes = val;
            sigma.layouts.configForceLink(cur.s, {adjustSizes:val});
            refresh();
        }

        state.controllers.barnesHutOptimize = function(val) {
            var cur = getConfig(state);
            cur.current.layout.barnesHutOptimize = val;
            sigma.layouts.configForceLink(cur.s, {barnesHutOptimize:val});
            refresh();
        }

        state.controllers.gravity = function(val) {
            var cur = getConfig(state);
            cur.current.layout.gravity = val;
            sigma.layouts.configForceLink(cur.s, {gravity:val});
            refresh();
        }

        state.controllers.barnesHutTheta = function(val) {
            var cur = getConfig(state);
            cur.current.layout.barnesHutTheta = val;
            sigma.layouts.configForceLink(cur.s, {barnesHutTheta:val});
            refresh();
        }

        state.controllers.edgeWeightInfluence = function(val) {
            var cur = getConfig(state);
            cur.current.layout.edgeWeightInfluence = val;
            sigma.layouts.configForceLink(cur.s, {edgeWeightInfluence:val});
            refresh();
        }

        state.controllers.slowDown = function(val) {
            var cur = getConfig(state);
            cur.current.layout.slowDown = val;
            sigma.layouts.configForceLink(cur.s, {slowDown:val});
            refresh();
        }

        // Label
        state.controllers.labelOption = function(val) {
            var cur = getConfig(state);
            cur.s.settings("drawLabels", val != 'none');
            if(val != 'none') {
                for (i = 0; i < cur.g.nodes.length; i++) {
                    cur.g.nodes[i].label = cur.x.nodes["label_" + val][i];
                }
            }
            cur.s.refresh();
        }
        state.controllers.labelColor = function(val) {
            var cur = getConfig(state);
            cur.current.label["color"] = val;
            cur.s.settings("defaultLabelColor", r2rgba(h2r(val), cur.current.label.opacity));
            cur.s.refresh();
        }
        state.controllers.labelOpacity = function(val) {
            var cur = getConfig(state);
            cur.current.label["opacity"] = val;
            cur.s.settings("defaultLabelColor", r2rgba(h2r(cur.current.label.color), val));
            cur.s.refresh();
        }
        state.controllers.labelScale = function(val) {
            var cur = getConfig(state);
            cur.current.label["scale"] = val;
            cur.s.settings("defaultLabelSize", 14 * val);
            cur.s.refresh();
        }

        // Node
        state.controllers.nodeScale = function(val) {
            var cur = getConfig(state);
            cur.current.node["scale"] = val;
            for (i = 0; i < cur.g.nodes.length; i++) {
                cur.g.nodes[i].size = cur.x.nodes.size[i] * val;
            }
            cur.s.settings("maxNodeSize", cur.current.settings.maxNodeSize * val);
            cur.s.refresh();
        }
        state.controllers.nodeOpacity = function(val) {
            var cur = getConfig(state);
            cur.current.node["opacity"] = val;
            for (i = 0; i < cur.g.nodes.length; i++) {
                if(cur.g.nodes[i].scheme != null) {
                    var palette = cur.current.scheme.dual[cur.g.nodes[i].scheme];
                    c = _iterpolatePalette(palette, cur.x.nodes.color[i]);
                    cur.g.nodes[i].color = h2rgba(c, cur.current.node.opacity);
                }
            }
            cur.s.refresh();
        }
        state.controllers.nodeBorderColor = function(val) {
            var cur = getConfig(state);
            cur.current.node["borderColor"] = val;
            cur.s.settings("defaultNodeBorderColor", r2rgba(h2r(val), cur.current.node.borderOpacity));
            cur.s.refresh();
        }
        state.controllers.nodeBorderOpacity = function(val) {
            var cur = getConfig(state);
            cur.current.node["borderOpacity"] = val;
            cur.s.settings("defaultNodeBorderColor", r2rgba(h2r(cur.current.node.borderColor), val));
            cur.s.refresh();
        }
        state.controllers.nodeBorderWidth = function(val) {
            var cur = getConfig(state);
            cur.current.node["borderWidth"] = val;
            cur.s.settings("nodeBorderSize", val);
            cur.s.refresh();
        }

        // Edge
        state.controllers.edgeScale = function(val) {
            var cur = getConfig(state);
            cur.current.edge["scale"] = val;
            for (i = 0; i < cur.g.edges.length; i++) {
                cur.g.edges[i].size = cur.x.links.weight[i] * val;
            }
            cur.s.settings("maxEdgeSize", 8 * val);
            cur.s.refresh();
        }

        state.controllers.edgeColor = function(val) {
            var cur = getConfig(state);
            cur.current.edge["color"] = val;
            cur.s.settings("defaultEdgeColor", r2rgba(h2r(val), cur.current.edge.opacity));
            cur.s.refresh();
        }
        state.controllers.edgeOpacity = function(val) {
            var cur = getConfig(state);
            cur.current.edge["opacity"] = val;
            cur.s.settings("defaultEdgeColor", r2rgba(h2r(cur.current.edge.color), val));
            cur.s.refresh();
        }

        // Color Scheme
        state.controllers.scheme = function(schemeId, domain, range) {

            var cur = getConfig(state);
            if (schemeId.startsWith("dual")) {
                var sch = schemeId.replace("dual", "");
                cur.current.scheme.dual[sch].domain = domain;
                cur.current.scheme.dual[sch].range = range;

                for (i = 0; i < cur.g.nodes.length; i++) {
                    if(cur.g.nodes[i].scheme == sch) {
                        var palette = cur.current.scheme.dual[sch];
                        c = _iterpolatePalette(palette, cur.x.nodes.color[i]);
                        cur.g.nodes[i].color = h2rgba(c, cur.current.node.opacity);
                    }
                }
            }
            cur.s.refresh();
        }

        // Buttons
        state.controllers.pause = function() {
            sigma.layouts.stopForceLink();
        }

        state.controllers.refresh = function() {
            var cur = getConfig(state);
            sigma.layouts.startForceLink(cur.s);
        }

        state.controllers.saveSVG = function() {
            var cur = getConfig(state);
            cur = global.getCurrentConfig(state)
            cur.sigma.toSVG({download: true, labels:true, filename: 'network.svg', size: 2000});
        }
    }
});
