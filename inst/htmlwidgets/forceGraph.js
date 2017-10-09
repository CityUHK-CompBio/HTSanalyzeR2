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
            // Default configuration
            state[state.currentKey] = {};
            // state[state.currentKey] = $.extend({}, global.defaultConfig);
            // var config = state[state.currentKey];
            // // Load X
            // var options = x.options;
            // var keys = ["distance", "seriesData",
            //     "title","titleSize","legendTitle",
            //     "edgeScale","edgeColor","edgeOpacity",
            //     "label","labelColor","labelOpacity","labelScale",
            //     "nodeScale","nodeScheme","nodeShape","nodeBorderColor","nodeBorderWidth","nodeBorderOpacity"]

            // for(var i in keys) {
            //     var key = keys[i]
            //     if(options.hasOwnProperty(key)) {
            //         config[key] = options[key];
            //     }
            // }

            // if(options.hasOwnProperty("colorDomain")) {
            //     var dom = options["colorDomain"];
            //     for (var scheme in config.palettes) {
            //         config.palettes[scheme].domain = dom.slice();
            //     }
            //     config.palettes.linear3.domain.splice(1, 0, (dom[0] + dom[1])/2);
            // }

            // config.labelFont = 14 * config.labelScale * state.ratio + "px Arial";

            // global.generateColorScalers(config);
            // global.generateRGBAColors(config);
        }

        return state[state.currentKey];
    },

    initialize: function(el, width, height) {
        // console.log("====================   initialize   ========================");
        el.style.height = "90vh";
        var state = global.getElementState(el);
        // var canvas = $(el).append($("<canvas />", {id: "graph"}));
        // $.extend(state, { canvas: canvas });
    },

    resize: function(el, width, height) {
        // console.log("====================   resize   ========================");

        // var state = global.getElementState(el);
        // var canvas = state.canvas;
        // var padding = state.padding;

        // state.ratio = window.devicePixelRatio || 1;
        // var size = {width: el.offsetWidth * state.ratio, height: el.offsetHeight * state.ratio};
        // size.boundary = [padding[0], size.width - padding[1], size.height - padding[2], padding[3]];
        // $.extend(state, size);

        // canvas.attr('width', state.width).attr('height', state.height);
        // canvas.style('transform', "scale(" + 1.0 / state.ratio + ")").style('transform-origin', "left top 0px");

        // simulation.force("center", d3.forceCenter(state.width / 2, state.height / 2));
        // simulation.alphaTarget(0).restart();
    },

    renderValue: function(el, x, simulation) {
        // console.log("====================   renderValue   ========================");
        // console.log(el);
        console.log(x);
        // console.log("=============================================================");

        var state = global.getElementState(el);

        if (x.update) {
            global.update(state, x);
        } else {
            global.construct(state, x);
        }
    },


    construct: function(state, x) {
        console.log("======================   construct   ========================");
        // console.log(state);
        // console.log(config);
        // console.log("=============================================================");

        // var canvas = state.canvas;

        var current = global.getCurrentConfig(state)
        if ("sigma" in current) {
            s = current.sigma;

            s.kill();
        }

        var current = global.getCurrentConfig(state, x)

        // var canvas = $("canvas#graph");
        var container = document.getElementById("map_output");
        container.innerHTML = '';

        var g = {
              nodes: [],
              edges: []
            };

        if ("graph" in current) {
            g = current.graph;
        } else {
            var i, s, o, cs = [];
            cs.push({
              color: 'rgba(200,200,200,0.9)'
            });

            cs.push({
              color: 'rgba(158, 22, 23,0.9)'
            });

            cs.push({
              color: 'rgba(0,106,156,0.9)'
            });

            cs.push({
              color: 'rgba(208,215,217, 0.2)'
            });

            N = x.nodes.id.length
            E = x.links.source.length

            // x.nodes.label = x.nodes["label_term"];

            for(i =0; i < N; i++) {
                g.nodes.push({
                    id: x.nodes.id[i],
                    label : x.nodes.label[i],
                    x: Math.cos(2 * i * Math.PI / N ),
                    y: Math.sin(2 * i * Math.PI / N + Math.PI),
                    // x : Math.cos(2 * Math.random() * N * Math.PI / N),
                    // y : Math.cos(2 * Math.random() * N * Math.PI / N),
                    size: 0 + x.nodes.size[i],
                    scheme: x.nodes.scheme[i],
                    color: cs[0].color
              });
            }

            for(i =0; i < g.nodes.length; i++) {
                if(g.nodes[i].scheme == 'Pos') {
                    g.nodes[i].color = cs[1].color;
                } else if (g.nodes[i].scheme == 'Neg') {
                    g.nodes[i].color = cs[2].color;
                }
            }

            for(i = 0; i < E; i++) {
                g.edges.push({
                    id: 'e' + i,
                    source: x.links.source[i],
                    target: x.links.target[i],
                    size: x.links.weight[i] / 1.5,
                    color: cs[3].color
                });
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
            maxNodeSize: 30,
            maxEdgeSize: 8,
            minEdgeSize: 1,

            enableEdgeHovering: false,
            borderSize: 2,
            outerBorderSize: 3,
            defaultNodeBorderColor: '#fff',
            defaultNodeOuterBorderColor: 'rgb(236, 81, 72)',
            nodeHaloColor: 'rgba(236, 81, 72, 0.1)',
            nodeHaloSize: 30,
          }
        });

        // var config = {
        //     autoArea: true,
        //     area: 1,
        //     gravity: 200,
        //     speed: 1,
        //     iterations: 1000,
        //     easing: 'quadraticInOut',
        //     duration: 8000,
        // }

        // s.refresh();
        // sigma.layouts.fruchtermanReingold.start(s, config);


        var config = {  
          linLogMode:true,
          outboundAttractionDistribution: false,
          strongGravityMode:false,
          gravity:4,
          barnesHutTheta:3,
            edgeWeightInfluence:0,
            adjustSizes:false,
            barnesHutOptimize: false,
            startingIterations: 1,
            iterationsPerRender: 1,
            slowDown: 50,
            autoStop:true,
            avgDistanceThreshold:1e-8,
            // maxIterations:200000,
            // nodeSiblingsScale: 1.5,
            easing:'quadraticInOut'
        }

        sigma.layouts.startForceLink(s, config);

        // LASSO
        // Instanciate the ActiveState plugin:
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
    },

    update: function(state, x) {
        var current = global.getCurrentConfig(state)

        var i, s, o, cs = [];
        cs.push({
          color: 'rgba(200, 200, 200, 0.9)'
        });

        cs.push({
          color: 'rgba(158, 22, 23,0.9)'
        });

        cs.push({
          color: 'rgba(0,106,156,0.9)'
        });

        g = current.graph;
        s = current.sigma;
        data = current.data;
        tick = data.options.seriesData[x.process_map - 1];

        for (i = 0; i < g.nodes.length; i++) {
            g.nodes[i].scheme = data.nodes["scheme." + tick][i];
            if(g.nodes[i].scheme == 'Pos') {
                g.nodes[i].color = cs[1].color;
            } else if (g.nodes[i].scheme == 'Neg') {
                g.nodes[i].color = cs[2].color;
            } else {
                g.nodes[i].color = cs[0].color;
            }
        }

        if (!sigma.layouts.fruchtermanReingold.isRunning(s)) {
            // sigma.layouts.fruchtermanReingold.start(s, config);
            s.refresh();
        }
        
    },

});
