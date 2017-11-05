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
        configureSettingHandlers(global.store.handlers);
    },

    resize: function(el, width, height) {
        console.log("====================   resize   ========================");
        var state = global.getElementState(el);
        var config = global.getConfig(state);
        
		global.refreshLegend(state, config);
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
        console.log("====================   Construct   ========================");
        console.log(x);

        var config = global.setConfig(state, x);
        global.initSupervisor(state);
        global.initMetadata(config, x);
        global.build(state, config);
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

            global.initPlugins(state);
        }
    },

    initMetadata: function(config, x) {
        if(!config.hasOwnProperty("metadata")) {
            global.mergeConfig(config, x);
            
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
                    meta.graph.nodes[i].color = _iterpolatePalette(palette, meta.data.nodes.color[i], cur.config.node.opacity);
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

        // Color Scheme
        handlers.scheme = function(schemeId, domain, range) {
            console.log("scheme" + ": " + schemeId);
            // console.log(domain);
            // console.log(range);
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;
            var meta = cur.config.metadata;

            if (schemeId.startsWith("dual")) {
                var sch = schemeId.replace("dual", "");
                cur.config.scheme.dual[sch].domain = domain;
                cur.config.scheme.dual[sch].range = range;

                for(var i = 0; i < meta.graph.nodes.length; i++) {
                    var palette = cur.config.scheme.dual[sch];
                    if(meta.graph.nodes[i].scheme == sch) {
                        meta.graph.nodes[i].color = _iterpolatePalette(palette, meta.data.nodes.color[i], cur.config.node.opacity);
                    }
                }
                sv.sigInst.refresh();
                global.refreshLegend(cur.state, cur.config);
            }
        }

        // Custom Buttons
        handlers.pause = function() {
            console.log("pause");
            var cur = global.currentSituation();
            var sv = cur.state.supervisor;

            sigma.layouts.stopForceLink();
        }

        handlers.refresh = function() {
            console.log("refresh");
            var cur = global.currentSituation();
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
            var g = global.drawLegend(situation.state, situation.config);
            return svgString.replace("</svg>", g.outerHTML + "</svg>");
        }

        handlers.saveSVG = function() {
            console.log("saveSVG");
            var cur = global.currentSituation();
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

        // Initialize the lasso plugin:
        var lasso = new sigma.plugins.lasso(s, s.renderers[0], {
          'strokeStyle': 'rgb(236, 81, 72)',
          'lineWidth': 2,
          'fillWhileDrawing': true,
          'fillStyle': 'rgba(236, 81, 72, 0.2)',
          'cursor': 'crosshair'
        });

        // // "spacebar" + "s" keys pressed binding for the lasso tool
        // keyboard.bind('32+83', function() {
        //   if (lasso.isActive) {
        //     lasso.deactivate();
        //   } else {
        //     lasso.activate();
        //   }
        // });
        
        // select.bindLasso(lasso);

        // // Listen for selectedNodes event
        // lasso.bind('selectedNodes', function (event) {
        //   setTimeout(function() {
        //     lasso.deactivate();
        //     s.refresh({ skipIdexation: true });
        //   }, 0);
        // });

        state.plugins = { 
            activeState: activeState, 
            dragListener: dragListener, 
            select: select, 
            lasso: lasso, 
            keyboard: keyboard
        }
    },

    killPlugins: function(state) {
        state.plugins.lasso.clear();
        sigma.plugins.killActiveState();
        sigma.plugins.killKeyboard(state.supervisor.sigInst);
        sigma.plugins.killSelect(state.supervisor.sigInst);
        sigma.plugins.killDragNodes(state.supervisor.sigInst);

        state.plugins = null;
    },

    switchTab: function(el) {
        console.log("====================   switchTab   ========================");
        if(global.store.currentTab != el) {
            // Current
            var state = global.getElementState(global.store.currentTab);
            global.killPlugins(state);

            // Target
            global.store.currentTab = el;
            var state = global.getElementState(el);
            if(state.hasOwnProperty("supervisor")) {
                var config = global.getConfig(state);
                global.build(state, config);
                global.initPlugins(state);
            }
        }
    },

    build: function(state, config) {
        console.log("======================   Build   ========================");
        var sv = state.supervisor;
        var meta = config.metadata;

        sv.graph.clear();
        sv.graph.read(meta.graph);
        sv.config = meta.forceConfig;
        sv.sigInst.settings(meta.sigmaSettings);
        sigma.layouts.killForceLink();
        sigma.layouts.startForceLink(sv.sigInst, sv.config);
        sv.sigInst.refresh();

        global.refreshLegend(state, config);
        refreshSettingPanel(state, config);
    },

    refreshLegend: function(state, config) {
        console.log("====================   Draw Legend   =====================");
        console.log(state);
        console.log(config);

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

        var gLegend = global.drawLegend(state, config);
        svg.appendChild(gLegend);
    },

    drawLegend: function(state, config) {
        var baseId = state.container.id;

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

        var palette = config.scheme.dual;
        var dim = [$(state.container).width(), $(state.container).height()];
        var legHeight = 100;
        var legWidth = 13;

        var posGrad = makeSVG("linearGradient", {id:baseId+'PosGrad', x1:'0%', y1:'0%', x2:'0%', y2:'100%'});
    		posGrad.appendChild(makeSVG('stop', {offset:'0%', 'stop-color':palette.Pos.range[0], 'stop-opacity':'1'}));
    		posGrad.appendChild(makeSVG('stop', {offset:'100%', 'stop-color':palette.Pos.range[1], 'stop-opacity':'1'}));
        var negGrad = makeSVG("linearGradient", {id:baseId+'NegGrad', x1:'0%', y1:'100%', x2:'0%', y2:'0%'});
    		negGrad.appendChild(makeSVG('stop', {offset:'0%', 'stop-color':palette.Neg.range[0], 'stop-opacity':'1'}));
    		negGrad.appendChild(makeSVG('stop', {offset:'100%', 'stop-color':palette.Neg.range[1], 'stop-opacity':'1'}));
    	var defs = makeSVG("defs", null, [posGrad, negGrad]);

        var rect1 = makeSVG('rect', {x1:'0', y1:'0', width:legWidth, height:legHeight, fill:'url(#'+ baseId +'PosGrad)', transform:translate(0, 0)});
        var rect2 = makeSVG('rect', {x1:'0', y1:'0', width:legWidth, height:legHeight, fill:'url(#'+ baseId +'NegGrad)', transform:translate(0, legHeight)});

        var g = makeSVG('g', {transform: translate(45, dim[1] - 2 * legHeight - 20)}, [defs, rect1, rect2]);

        var textPos = makeSVG('text', {'font-size':'14','font-family':'arial','fill':palette.Pos.range[0],'transform':translate(-30,2),'alignment-baseline':'hanging'});
        	textPos.append("Pos");
        var textPosTick1 = makeSVG('text', {'font-size':'14','font-family':'arial','fill':'rgba(0,0,0,1)','transform':translate(legWidth+2,2),'alignment-baseline':'hanging'});
        	textPosTick1.append(palette.Pos.domain[0]);
        var textPosTick2 = makeSVG('text', {'font-size':'14','font-family':'arial','fill':'rgba(0,0,0,1)','transform':translate(legWidth+2,legHeight-2),'alignment-baseline':'baseline'});
        	textPosTick2.append(palette.Pos.domain[1]);
        appendChildren(g, [textPos, textPosTick1, textPosTick2]);

        var textNeg = makeSVG('text', {'font-size':'14','font-family':'arial','fill':palette.Neg.range[0],'transform':translate(-30,legHeight*2-2),'alignment-baseline':'baseline'});
        	textNeg.append("Neg");
        var textNegTick1 = makeSVG('text', {'font-size':'14','font-family':'arial','fill':'rgba(0,0,0,1)','transform':translate(legWidth+2,legHeight*2-2),'alignment-baseline':'baseline'});
        	textNegTick1.append(palette.Neg.domain[0]);
        var textNegTick2 = makeSVG('text', {'font-size':'14','font-family':'arial','fill':'rgba(0,0,0,1)','transform':translate(legWidth+2,legHeight+2),'alignment-baseline':'hanging'});
        	textNegTick2.append(palette.Neg.domain[1]);
        appendChildren(g, [textNeg, textNegTick1, textNegTick2]);

        return g;
    }

});


