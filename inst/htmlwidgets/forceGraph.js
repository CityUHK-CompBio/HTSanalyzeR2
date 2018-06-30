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
                    gravity: 10,
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
        var options = x.options;

        config.info.graphType = options.type; //HyperGeo GSEA NWA
        config.info.legendTitle = "legendTitle" in options ? options.legendTitle : "";
        config.info.colorScaler = options.colorScaler;
        config.info.upperBound = "nPermutations" in options ? Math.log10(options.nPermutations) : 1024;
        if(config.info.graphType == "GSEA" || config.graphType == "HyperGeo") {
            config.settings.maxNodeSize = 20;
            config.settings.minNodeSize = 3;
            config.settings.maxEdgeSize = 8;
        } else if (config.info.graphType == "NWA") {
            config.settings.maxNodeSize = 10;
            config.settings.maxEdgeSize = 2;
        }

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

        if ("pos" in options.colorDomain) {
            config.scheme.dual.pos.enabled = true;
            config.scheme.dual.pos.domain = $.extend(true, {}, options.colorDomain.pos);
        }
        if ("neg" in options.colorDomain) {
            config.scheme.dual.neg.enabled = true;
            config.scheme.dual.neg.domain = $.extend(true, {}, options.colorDomain.neg);
        }

        if (config.info.graphType == "HyperGeo") {
            config.scheme.dual.pos.domain = [1e-6, 1];
        } else if (config.info.graphType == "GSEA") {
            config.scheme.dual.pos.domain[0] = parseFloat("1e-" + config.info.upperBound);
            config.scheme.dual.neg.domain[0] = parseFloat("1e-" + config.info.upperBound);
        } else if (config.info.graphType == "NWA" && "pos" in options.colorDomain) {
            config.scheme.dual.pos.domain[0] = options.colorDomain.pos[1];
            config.scheme.dual.pos.domain[1] = options.colorDomain.pos[0];
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
                g.nodes[i].color = meta.interpolator(palette, x.nodes["color." + tick][i], config.node.opacity);
            } else {
                g.nodes[i].color = hex2rgba(config.node.NANodeColor);
            }
        }

        sv.sigInst.refresh({skipIndexation: true});

        // if (!sigma.layouts.isForceLinkRunning()) {
        //     sv.sigInst.refresh({skipIndexation: true});
        // }
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
            var interpolator = config.info.colorScaler == "log10" ? log10Interpolator : linearInterpolator;
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
                    g.nodes[i].color = interpolator(palette, x.nodes.color[i], config.node.opacity);
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
                interpolator: interpolator,
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
            meta.sigmaSettings.drawLabels = val != 'none';
            sv.sigInst.settings("drawLabels", meta.sigmaSettings.drawLabels);
            sv.sigInst.refresh({skipIndexation: true});
        }
        handlers.labelScale = function(val) {
            // console.log("labelScale" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            cur.config.label.scale = val;
            meta.sigmaSettings.defaultLabelSize = 14 * val;
            sv.sigInst.settings("defaultLabelSize", meta.sigmaSettings.defaultLabelSize);
            sv.sigInst.refresh({skipIndexation: true});
        }
        handlers.labelColor = function(val) {
            // console.log("labelColor" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            cur.config.label.color = val;
            meta.sigmaSettings.defaultLabelColor = hex2rgba(val);
            sv.sigInst.settings("defaultLabelColor", meta.sigmaSettings.defaultLabelColor);
            sv.sigInst.refresh({skipIndexation: true});
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
            meta.sigmaSettings.maxNodeSize = cur.config.settings.maxNodeSize * val;
            sv.sigInst.settings("maxNodeSize", meta.sigmaSettings.maxNodeSize);
            sv.sigInst.refresh({skipIndexation: true});
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
                    meta.graph.nodes[i].color = meta.interpolator(palette, meta.data.nodes.color[i], cur.config.node.opacity);
                }
            }
            sv.sigInst.refresh({skipIndexation: true});
        }
        handlers.nodeBorderWidth = function(val) {
            // console.log("nodeBorderWidth" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            cur.config.node.borderWidth = parseFloat(val);
            meta.sigmaSettings.nodeBorderSize = parseFloat(val);
            sv.sigInst.settings("nodeBorderSize", meta.sigmaSettings.nodeBorderSize);
            sv.sigInst.refresh({skipIndexation: true});
        }
        handlers.nodeBorderColor = function(val) {
            // console.log("nodeBorderColor" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            cur.config.node.borderColor = val;
            meta.sigmaSettings.defaultNodeBorderColor = hex2rgba(val);
            sv.sigInst.settings("defaultNodeBorderColor", meta.sigmaSettings.defaultNodeBorderColor);
            sv.sigInst.refresh({skipIndexation: true});
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
            meta.sigmaSettings.maxEdgeSize = cur.config.settings.maxEdgeSize * val;
            sv.sigInst.settings("maxEdgeSize", meta.sigmaSettings.maxEdgeSize);
            sv.sigInst.refresh({skipIndexation: true});
        }
        handlers.edgeColor = function(val) {
            // console.log("edgeColor" + ": " + val);
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            cur.config.edge.color = val;
            meta.sigmaSettings.defaultEdgeColor = hex2rgba(val);
            sv.sigInst.settings("defaultEdgeColor", meta.sigmaSettings.defaultEdgeColor);
            sv.sigInst.refresh({skipIndexation: true});
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
                        meta.graph.nodes[i].color = meta.interpolator(palette, meta.data.nodes.color[i], cur.config.node.opacity);
                    }
                }
                sv.sigInst.refresh({skipIndexation: true});
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
        function downloadSVG(rawSVG, filename) {
            if (typeof safari !== 'undefined') {
                var msg = "File download does not work in Safari. Please use a modern web browser such as Firefox, Chrome, or Internet Explorer 11.";
                alert(msg);
                throw new Error(msg);
            }

            // Blob
            var blob = new Blob( [rawSVG], {type: 'image/svg+xml;charset=utf-8'} );
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

        function downloadPNG(rawSVG, filename) {
            if (typeof safari !== 'undefined') {
                var msg = "File download does not work in Safari. Please use a modern web browser such as Firefox, Chrome, or Internet Explorer 11.";
                alert(msg);
                throw new Error(msg);
            }

            var blob = new Blob([rawSVG], {type:"image/svg+xml;charset=utf-8"});
            var objectUrl = window.URL.createObjectURL(blob);

            var cur = fg.currentSituation();
            var elem = $(cur.state.container);
            var canvas = document.createElement("canvas");
            canvas.setAttribute("width", elem.width());
            canvas.setAttribute("height", elem.height());

            var img = new Image;
            img.src = objectUrl;
            img.onload = function () {
                canvas.getContext("2d").drawImage(this, 0, 0);
                window.URL.revokeObjectURL(objectUrl);

                var anchor = document.createElement('a');
                anchor.setAttribute('href', canvas.toDataURL());
                anchor.setAttribute('download', filename);

                // Firefox requires the link to be added to the DOM before it can be clicked.
                document.body.appendChild(anchor);
                anchor.click();
                document.body.removeChild(anchor);
            };

            setTimeout(function() { // Firefox needs a timeout
                window.URL.revokeObjectURL(objectUrl);
            }, 0);
        }

        function fixStrokeBug(svgString) {
            // Extract style sheet
            var reStyle = /<style>[\S\s^<]*<\/style>/;
            var found = svgString.match(reStyle);
            if(found.length == 0) {
                return svgString;
            }
            var strStyle = found[0];
            var strStyleModified = strStyle;
            // Append stroke
            var reNodeStyle = /\.sigma-node{[^}]*}/g;
            found = strStyleModified.match(reNodeStyle);
            if(found.length > 0) {
                var sheet = found[0];
                var cur = fg.currentSituation();
                var ruleColor = "stroke: " + hex2rgba(cur.config.node.borderColor) + ";";
                var ruleWidth = "stroke-width: " + cur.config.node.borderWidth + ";";
                var appendedSheet = sheet.replace("}", ";" + ruleColor + ruleWidth + "}");
                strStyleModified = strStyleModified.replace(sheet, appendedSheet);
            }
            // Remove rgba
            var reRgbaFill = /fill:\srgba\(([\d]{1,3}),([\d]{1,3}),([\d]{1,3}),([01]?\.?[\d]*)\)/g;
            strStyleModified = strStyleModified.replace(reRgbaFill, "fill: rgb($1,$2,$3);fill-opacity: $4;");
            var reRgbaStroke = /stroke:\srgba\(([\d]{1,3}),([\d]{1,3}),([\d]{1,3}),([01]?\.?[\d]*)\)/g;
            strStyleModified = strStyleModified.replace(reRgbaStroke, "stroke: rgb($1,$2,$3);stroke-opacity: $4;");
            strStyleModified = strStyleModified.replace(/;;/g, ";");

            return svgString.replace(strStyle, strStyleModified);
        }

        function appendLegend(svgString, situation) {
            var g = fg.drawLegend(situation.state, situation.config);
            return svgString.replace("</svg>", g.outerHTML + "</svg>");
        }

        handlers.saveImg = function() {
            // console.log("saveSVG");
            var cur = fg.currentSituation();
            var sv = cur.state.supervisor;
            var elem = $(cur.state.container);

            // sv.sigInst.toSVG({download: true, labels:true, filename: 'network.svg', width: elem.width(), height: elem.height()})
            var svgStr = sv.sigInst.toSVG({labels:true, width: elem.width(), height: elem.height()});
            var rawSVG = appendLegend(fixStrokeBug(svgStr), cur);
            downloadSVG(rawSVG, "network.svg");
            // downloadPNG(rawSVG, "network.png");
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

            state = fg.getElementState(el);
            if(state.hasOwnProperty("supervisor")) {
                var config = fg.getConfig(state);
                fg.build(state, config);
                fg.initPlugins(state);
            }
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
        var fa = sigma.layouts.startForceLink(sv.sigInst, sv.config);
        sv.sigInst.refresh();

        fa.bind('start stop', function(event) { switchBtnIcon(event.type) });
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
        var ticks = config.info.graphType == 'HyperGeo' ? 7 : 11;
        var dimension = [$(state.container).width(), $(state.container).height()];
        var colors = Array(ticks).fill("#FDFDFD");
        var labels = Array(ticks).fill("");

        var schemes = []
        if (config.scheme.dual.pos.enabled) schemes.push("pos");
        if (config.scheme.dual.neg.enabled) schemes.push("neg");

        if (schemes.length == 1 && config.info.graphType != 'GSEA') {
            var pal = config.scheme.dual[schemes[0]];
            for(var i = 0; i < ticks; i++) {
                colors[i] = _interpolateColor(pal.range, i / (ticks - 1), "hex");
            }
        } else {
            for (var i = 0; i < 5; i++) {
                colors[i] = _interpolateColor(config.scheme.dual.pos.range, i / 4, "hex");
            }
            for (i++; i < ticks; i++) {
                colors[i] = _interpolateColor(config.scheme.dual.neg.range, (ticks - i - 1) / 4, "hex");
            }
        }

        // fix digits and filter zero
        var nice = function(vals, filterZero) {
            var digits = 1;
            if(vals[0].toFixed(digits) === vals[1].toFixed(digits)) digits++;
            if(vals[0].toFixed(digits) === vals[1].toFixed(digits)) digits++;
            var res = [vals[0].toFixed(digits), vals[1].toFixed(digits)];
            if(filterZero && parseFloat(res[0]) == 0) res[0] = "";
            if(filterZero && parseFloat(res[1]) == 0) res[1] = "";
            return res;
        }

        if (config.info.graphType == 'HyperGeo') {
            for (var i = 0; i < ticks; i++) {
                labels[i] = ticks - 1 - i;
            }
        } else if (config.info.graphType == 'GSEA') {
            ceiledBound = Math.ceil(config.info.upperBound);
            labels[0] = labels[10] = ceiledBound;
            labels[5] = 0;
        } else if (config.info.graphType == 'NWA') {
            if (schemes.length == 1) {
                var palLabels = nice(pal.domain, false)
                labels[0] = palLabels[0];
                labels[10] = palLabels[1];
            } else {
                var posLabels = nice(config.scheme.dual.pos.domain, true)
                labels[0] = posLabels[0]
                labels[4] = posLabels[1]
                labels[5] = "0";
                var negLabels = nice(config.scheme.dual.neg.domain, true)
                labels[6] = negLabels[1];
                labels[10] = negLabels[0];
            }
        }

        var legend = fg.legendFactory(colors, labels, config.info.legendTitle, dimension);
        return legend;
    },

    legendFactory: function(colors, labels, title, dimension) {
        function makeSVG(tag, attrs, children) {
            var el= document.createElementNS('http://www.w3.org/2000/svg', tag);
            for (var k in attrs)
                el.setAttribute(k, attrs[k]);
            appendChildren(el, children);
            return el;
        }
        function appendChildren(el, children) {
            for (var i in children)
                el.appendChild(children[i]);
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

        var edgeWidth = 18;
        var ticks = colors.length;
        var legWidth = 60;
        var legHeight = edgeWidth * ticks;

        var g = makeSVG('g', {transform: translate(60, dimension[1] - legHeight - 40)});

        for(var i = 0; i < ticks; i++) {
            var rect = makeSVG('rect', {transform:translate(10, edgeWidth * i), width:edgeWidth, height:edgeWidth, style:"fill:"+colors[i]});
            var text = makeText(labels[i], edgeWidth + 15, edgeWidth * i + 4);
            appendChildren(g, [rect, text]);
        }

        // Legend title
        var gTitle = makeSVG('text', {transform:translate(edgeWidth / 2 + 10, edgeWidth * ticks + 5), 'font-size':'13','font-family':'arial','fill':'rgba(0,0,0,0.8)','alignment-baseline':'hanging', 'text-anchor':'middle'});
        gTitle.append(title);
        appendChildren(g, [gTitle]);
        return g;
    }

});

