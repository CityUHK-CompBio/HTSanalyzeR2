HTMLWidgets.widget(fg = {
    name: "forceGraph",
    type: "output",
    store: {currentTab: null},

    getElementState: function(el) {
        var elId = el.id;
        if (!(elId in fg.store)) {
            fg.store[elId] = {elId: elId, container:el, currentKey: "default"};
        }

        return fg.store[elId];
    },

    setConfig: function(state, x) {
        state.currentKey = JSON.stringify(x).hashCode().toString();
        if(!(state.currentKey in state)) {
             state[state.currentKey] = {
                settings: {
                    minNodeSize: 3,
                    minEdgeSize: 1,
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
                    color: "#000000FF"
                },
                node: {
                    scale: 1,
                    opacity: 0.9,
                    borderColor: "#E6E6E6B2",
                    borderWidth: 1,
                    NANodeColor: "#EDEDEDE5"
                },
                edge : {
                    scale: 1,
                    color: "#C6C6C666"
                },
                scheme: {
                    dual: {
                        pos: {
                            enabled: false,
                            domain: [0, 1],
                            range: ["#9E1617", "#FFF3F3"]
                        },
                        neg: {
                            enabled: false,
                            domain: [0, 1],
                            range: ["#006A9C", "#F3F3FF"]
                        }
                    }
                },
                info: {
                    graphType: "",
                    legendTitle: ""
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
        config.info.graphType = x.options.type; //HyperGeo GSEA NWA
        config.info.legendTitle = "legendTitle" in x.options ? x.options.legendTitle : "";
        if(config.info.graphType == "GSEA" || config.graphType == "HyperGeo") {
            config.settings.maxNodeSize = 20;
            config.settings.minNodeSize = 3;
            config.settings.maxEdgeSize = 8;
        } else if (config.info.graphType == "NWA") {
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
        if ("pos" in x.options.colorDomain) {
            config.scheme.dual.pos.enabled = true;
            config.scheme.dual.pos.domain = $.extend(true, {}, x.options.colorDomain.pos);
        }
        if ("neg" in x.options.colorDomain) {
            config.scheme.dual.neg.enabled = true;
            config.scheme.dual.neg.domain = $.extend(true, {}, x.options.colorDomain.neg);
        }
        if (config.info.graphType == "NWA" && "pos" in x.options.colorDomain) {
            config.scheme.dual.pos.domain[0] = x.options.colorDomain.pos[1];
            config.scheme.dual.pos.domain[1] = x.options.colorDomain.pos[0];
        }
        if (config.info.graphType == "GSEA") {
            if (!("pos" in x.options.colorDomain)) {
                config.scheme.dual.pos.domain[1] = 1e-8
            }
            if (!("neg" in x.options.colorDomain)) {
                config.scheme.dual.neg.domain[1] = 1e-8
            }
        }
    },

    currentSituation: function() {
        var situation = {};
        if(fg.store.currentTab != null) {
            situation.el = fg.store.currentTab;
            situation.state = fg.getElementState(situation.el);
            situation.config = fg.getConfig(situation.state);
        }
        return situation;
    },

    initialize: function(el, width, height) {
        // console.log("====================   initialize   ========================");
        el.style.height = ($("#settingBar").length > 0) ? "85vh" : "97vh";
        fg.store.currentTab = el;
        var initState = fg.getElementState(el);
        initState.container = el;

        registerForceGraph(fg);
        fg.initHandlers();
        configureSettingHandlers(fg.store.handlers);
    },

    resize: function(el, width, height) {
        // console.log("====================   resize   ========================");
        var state = fg.getElementState(el);
        var config = fg.getConfig(state);

        fg.refreshLegend(state, config);
    },

    renderValue: function(el, x) {
        // console.log("====================   renderValue   ========================");
        // console.log(x);
        var state = fg.getElementState(el);

        if (x.update) {
            fg.update(state, x);
        } else {
            fg.construct(state, x);
        }
    },

    update: function(state, u) {
        // console.log("====================   Update    ========================");
        // console.log(u);
        var config = fg.getConfig(state);
        var sv = state.supervisor;
        var meta = config.metadata;

        var x = meta.data;
        var g = meta.graph;
        for(var i = 0; i < g.nodes.length; i++) {
            var tick = x.options.seriesData[u.process - 1];
            g.nodes[i].theme = x.nodes["scheme." + tick][i];

            if(g.nodes[i].theme != null) {
                var palette = config.scheme.dual[g.nodes[i].theme];
                g.nodes[i].color = _iterpolatePalette(palette, x.nodes["color." + tick][i], config.node.opacity);
            } else {
                g.nodes[i].color = hex2rgba(config.node.NANodeColor);
            }
        }

        if (!sigma.layouts.isForceLinkRunning()) {
            sv.sigInst.refresh();
        }
    },

    construct: function(state, x) {
        // console.log("====================   Construct   ========================");
        // console.log(x);

        var config = fg.setConfig(state, x);
        fg.initSupervisor(state);
        fg.initMetadata(config, x);
        fg.build(state, config);
    },

    initSupervisor: function(state) {
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

            fg.initPlugins(state);
        }
    },

    initMetadata: function(config, x) {
        if(!config.hasOwnProperty("metadata")) {
            fg.mergeConfig(config, x);

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
                    g.nodes[i].color = _iterpolatePalette(palette, x.nodes.color[i], config.node.opacity);
                } else {
                    g.nodes[i].color = hex2rgba(config.node.NANodeColor);
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
                maxEdgeSize: config.settings.maxEdgeSize * config.edge.scale,


                edgeColor: 'default',
                defaultEdgeColor: hex2rgba(config.edge.color),

                nodeBorderColor: 'default',
                nodeBorderSize: config.node.borderWidth,
                defaultNodeBorderColor: hex2rgba(config.node.borderColor),

                drawLabels: config.label.text != "none",
                defaultLabelSize: 14 * config.label.scale,
                defaultLabelColor: hex2rgba(config.label.color),
                labelThreshold: 0,

                enableEdgeHovering: false,
                borderSize: 2,
                outerBorderSize: 3,
                nodeHaloColor: 'rgba(236, 81, 72, 0.2)',
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

            var meta = {
                data: x,
                graph: g,
                sigmaSettings: sigmaSettings,
                forceConfig: forceConfig
            };
            config.metadata = meta;
        }
    },

    initHandlers: function() {
        // console.log("==================== Init Handlers ========================");

        if(fg.store.hasOwnProperty("handlers")) {
            return;
        }
        fg.store.handlers = {};
        var handlers = fg.store.handlers;

        // Layout
        createLayoutRefreshFunc = function(key) {
            return function(val) {
                // console.log(key + ": " + val);
                var cur = fg.currentSituation();
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
            // console.log("labelOption" + ": " + val);
            var cur = fg.currentSituation();
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
            // console.log("labelScale" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;

            cur.config.label.scale = val;
            sv.sigInst.settings("defaultLabelSize", 14 * val);
            sv.sigInst.refresh();
        }
        handlers.labelColor = function(val) {
            // console.log("labelColor" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;

            cur.config.label.color = val;
            sv.sigInst.settings("defaultLabelColor", hex2rgba(val));
            sv.sigInst.refresh();
        }

        // Node
        handlers.nodeScale = function(val) {
            // console.log("nodeScale" + ": " + val);
            var cur = fg.currentSituation();
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
            // console.log("nodeOpacity" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            cur.config.node.opacity = val;
            for(var i = 0; i < meta.graph.nodes.length; i++) {
                if(meta.graph.nodes[i].scheme != null) {
                    var palette = cur.config.scheme.dual[meta.graph.nodes[i].scheme];
                    meta.graph.nodes[i].color = _iterpolatePalette(palette, meta.data.nodes.color[i], cur.config.node.opacity);
                }
            }
            sv.sigInst.refresh();
        }
        handlers.nodeBorderWidth = function(val) {
            // console.log("nodeBorderWidth" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;

            cur.config.node.borderWidth = parseFloat(val);
            sv.sigInst.settings("nodeBorderSize", parseFloat(val));
            sv.sigInst.refresh();
        }
        handlers.nodeBorderColor = function(val) {
            // console.log("nodeBorderColor" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;

            cur.config.node.borderColor = val;
            sv.sigInst.settings("defaultNodeBorderColor", hex2rgba(val));
            sv.sigInst.refresh();
        }

        // Edge
        handlers.edgeScale = function(val) {
            // console.log("edgeScale" + ": " + val);
            var cur = fg.currentSituation();
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
            // console.log("edgeColor" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;

            cur.config.edge.color = val;
            sv.sigInst.settings("defaultEdgeColor", hex2rgba(val));
            sv.sigInst.refresh();
        }

        // Color Scheme
        handlers.scheme = function(schemeId, subScheme, type, idx, value) {
            // console.log("scheme" + ": " + schemeId + " " + subScheme + " " + type + " " + value);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            if (schemeId == "dual") {
                cur.config.scheme.dual[subScheme][type][idx] = value;

                for(var i = 0; i < meta.graph.nodes.length; i++) {
                    var palette = cur.config.scheme.dual[subScheme];
                    if(meta.graph.nodes[i].scheme == subScheme) {
                        meta.graph.nodes[i].color = _iterpolatePalette(palette, meta.data.nodes.color[i], cur.config.node.opacity);
                    }
                }
                sv.sigInst.refresh();
                fg.refreshLegend(cur.state, cur.config);
            }
        }

        // Custom Buttons
        handlers.pause = function() {
            // console.log("pause");
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;

            sigma.layouts.stopForceLink();
        }

        handlers.refresh = function() {
            // console.log("refresh");
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;

            sigma.layouts.startForceLink(sv.sigInst);
        }

        // download function from sigma sigma.exporters.svg
        function download(string, filename) {
            if (typeof safari !== 'undefined') {
                var msg = "File download does not work in Safari. Please use a modern web browser such as Firefox, Chrome, or Internet Explorer 11.";
                alert(msg);
                throw new Error(msg);
            }

            // Blob
            var blob = new Blob( [string], {type: 'image/svg+xml;charset=utf-8'} );
            objectUrl = window.URL.createObjectURL(blob);

            if (navigator.msSaveBlob) { // IE11+ : (has Blob, but not a[download])
                navigator.msSaveBlob(blob, filename);
            } else if (navigator.msSaveOrOpenBlob) { // IE10+ : (has Blob, but not a[download])
                navigator.msSaveOrOpenBlob(blob, filename);
            } else {
                // A-download
                var anchor = document.createElement('a');
                anchor.setAttribute('href', objectUrl);
                anchor.setAttribute('download', filename);

                // Firefox requires the link to be added to the DOM before it can be clicked.
                document.body.appendChild(anchor);
                anchor.click();
                document.body.removeChild(anchor);
            }

            setTimeout(function() { // Firefox needs a timeout
                window.URL.revokeObjectURL(objectUrl);
            }, 0);
        }

        function appendLegend(svgString, situation) {
            var g = fg.drawLegend(situation.state, situation.config);
            return svgString.replace("</svg>", g.outerHTML + "</svg>");
        }

        handlers.saveSVG = function() {
            // console.log("saveSVG");
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;
            var elem = $(cur.state.container);

            // sv.sigInst.toSVG({download: true, labels:true, filename: 'network.svg', width: elem.width(), height: elem.height()})
            var svgStr = sv.sigInst.toSVG({labels:true, width: elem.width(), height: elem.height()});
            download(appendLegend(svgStr, cur), "network.svg");
        }
    },

    initPlugins: function(state) {
        var s = state.supervisor.sigInst;

        // Initialize the activeState plugin:
        var activeState = sigma.plugins.activeState(s);
        // Initialize the dragNodes plugin:
        var dragListener = sigma.plugins.dragNodes(s, s.renderers[0], activeState);
        // Initialize the select plugin:
        var select = sigma.plugins.select(s, activeState);

        // Halo on active nodes:
        s.renderers[0].bind('render', function(e) {
          s.renderers[0].halo({
            nodes: activeState.nodes()
          });
        });

        var keyboard = sigma.plugins.keyboard(s, s.renderers[0]);
        select.bindKeyboard(keyboard);
    },

    killPlugins: function(state) {
        sigma.plugins.killActiveState();
        sigma.plugins.killKeyboard(state.supervisor.sigInst);
        sigma.plugins.killSelect(state.supervisor.sigInst);
        sigma.plugins.killDragNodes(state.supervisor.sigInst);
    },

    switchTab: function(el) {
        // console.log("====================   switchTab   ========================");
        if(fg.store.currentTab != el) {
            // Current
            var state = fg.getElementState(fg.store.currentTab);
            $("svg", state.container).remove();
            fg.killPlugins(state);

            // Target
            fg.store.currentTab = el;
        }

        var state = fg.getElementState(el);
        if(state.hasOwnProperty("supervisor")) {
            var config = fg.getConfig(state);
            fg.build(state, config);
            fg.initPlugins(state);
        }
    },

    build: function(state, config) {
        // console.log("======================   Build   ========================");
        var sv = state.supervisor;
        var meta = config.metadata;

        sv.graph.clear();
        sv.graph.read(meta.graph);
        sv.config = meta.forceConfig;
        sv.sigInst.settings(meta.sigmaSettings);
        sigma.layouts.killForceLink();
        sigma.layouts.startForceLink(sv.sigInst, sv.config);
        sv.sigInst.refresh();

        fg.refreshLegend(state, config);

        if($("#settingBar").length > 0) {
            refreshSettingPanel(state, config);
        }
    },

    refreshLegend: function(state, config) {
        // console.log("====================   Draw Legend   =====================");
        // console.log(state);
        // console.log(config);

        var container = state.container;

        // Create or Select the SVG layer.
        if(container.getElementsByTagName("svg").length == 0) {
            var svgElem = document.createElementNS("http://www.w3.org/2000/svg", "svg");
            svgElem.style.width = "100%";
            svgElem.style.height = "100%";
            container.appendChild(svgElem);
        }
        var svg = container.getElementsByTagName("svg")[0];
        svg.innerHTML = '';

        var gLegend = fg.drawLegend(state, config);
        svg.appendChild(gLegend);
    },

    drawLegend: function(state, config) {
        function nice(value) {
            var sciNot = value.toExponential(1).split('e');
            var a = Math.round(sciNot[0] * 2 + Math.sign(sciNot[0]) * 0.45) * 0.5;
            var n = sciNot[1];
            return (a + 'e' + n) * 1;
            return value;
        }
        function appendChildren(el, children) {
            for (var i in children)
                el.appendChild(children[i]);
            return el;
        }
        function makeSVG(tag, attrs, children) {
            var el= document.createElementNS('http://www.w3.org/2000/svg', tag);
            for (var k in attrs)
                el.setAttribute(k, attrs[k]);
            appendChildren(el, children);
            return el;
        }
        function translate(x, y) {
            return 'translate('+ x +', ' + y + ')';
        }
        function makeText(cont, x, y) {
            var txt = makeSVG('text', {transform:translate(x, y), 'font-size':'13','font-family':'arial','fill':'rgba(0,0,0,0.8)','alignment-baseline':'hanging'} )
            txt.append(cont);
            return txt;
        }

        var pals = $.extend(true, {}, config.scheme.dual);
        var schs = [];

        // config.info.graphType: HyperGeo GSEA NWA
        if (config.info.graphType == 'GSEA') {
            schs = ["pos", "neg"];
        } else {
            if (pals.pos.enabled) schs.push("pos");
            if (pals.neg.enabled) schs.push("neg");
        }

        // nice domain
        for(var idx in schs) {
            pals[schs[idx]].domain[0] = nice(pals[schs[idx]].domain[0]);
            pals[schs[idx]].domain[1] = nice(pals[schs[idx]].domain[1]);
        }
        if (config.info.graphType == 'GSEA') {
            var maxDom = Math.max(pals.pos.domain[1], pals.neg.domain[1]);
            pals.pos.domain[1] = maxDom;
            pals.neg.domain[1] = maxDom;
        }

        // draw legend
        var dim = [$(state.container).width(), $(state.container).height()];
        var edgeLen = 18;
        var ticks = 11;
        var legWidth = 60;
        var legHeight = edgeLen * ticks;

        var g = makeSVG('g', {transform: translate(40, dim[1] - legHeight - 40)});

        if (schs.length == 1) {
            var pal = pals[schs[0]];
            for(var i = 0; i < ticks; i++) {
                var color = _iterpolateColor(pal.range, i / (ticks - 1), "hex");
                var rect = makeSVG('rect', {transform:translate(10, edgeLen * i), width:edgeLen, height:edgeLen, style:"fill:"+color});
                appendChildren(g, [rect]);
            }

            var txt0 = makeText(pal.domain[0], edgeLen + 15, 5);
            var txt1 = makeText(pal.domain[1], edgeLen + 15, (ticks - 1) * edgeLen + 5);
            appendChildren(g, [txt0, txt1]);
        } else {
            var i, sepTicks = 5;
            for(i = 0; i < sepTicks; i++) {
                var color = _iterpolateColor(pals.pos.range, i / (sepTicks - 1), "hex");
                g.appendChild(makeSVG('rect', {transform:translate(10, edgeLen * i), width:edgeLen, height:edgeLen, style:"fill:"+color}));
            }
            g.appendChild(makeSVG('rect', {transform:translate(10, edgeLen * i++), width:edgeLen, height:edgeLen, style:"fill:#FDFDFD"}));
            for(; i <= 2 * sepTicks; i++) {
                var color = _iterpolateColor(pals.neg.range, ( 2*sepTicks - i) / (sepTicks - 1), "hex");
                g.appendChild(makeSVG('rect', {transform:translate(10, edgeLen * i), width:edgeLen, height:edgeLen, style:"fill:"+color}));
            }

            var txts = [pals.pos.domain[0], pals.pos.domain[1], 1, pals.neg.domain[1], pals.neg.domain[0]];
            var transY = [5, 4*edgeLen + 5, 5*edgeLen + 5, 6*edgeLen + 5, 10 * edgeLen + 5];
            txts[2] = (config.info.graphType == 'NWA') ? 0 : 1;

            for(var i = 0; i < txts.length; i++) {
                g.appendChild(makeText(txts[i], edgeLen + 15, transY[i]));
            }
        }

        // Legend title
        var title = makeSVG('text', {transform:translate(edgeLen / 2 + 10, edgeLen * ticks + 5), 'font-size':'13','font-family':'arial','fill':'rgba(0,0,0,0.8)','alignment-baseline':'hanging', 'text-anchor':'middle'});
        title.append(config.info.legendTitle);
        // var txt1 = makeSVG('tspan', {x:0, dy:"1.2em"}); txt1.append("Adjusted");
        // var txt2 = makeSVG('tspan', {x:0, dy:"1.2em"}); txt2.append("p-values");
        // appendChildren(title, [txt1, txt2])
        appendChildren(g, [title]);

        return g;
    }

});


