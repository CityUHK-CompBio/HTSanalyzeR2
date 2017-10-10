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
        // var container = document.getElementById("map_output");
        // var container = document.getElementsByClassName("forceGraph")[0];
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

            // x.nodes.label = x.nodes["label_term"];

            for(i =0; i < N; i++) {
                var c = colors.grey;
                if(x.nodes.scheme[i] == 'Pos') {
                    c = _interpolateColor(colors.red, colors.white, x.nodes.color[i] / 0.05);
                } else if (x.nodes.scheme[i] == 'Neg') {
                    c = _interpolateColor(colors.blue, colors.white, x.nodes.color[i] / 0.05);
                }

                g.nodes.push({
                    id: x.nodes.id[i],
                    label : x.nodes.label[i],
                    x: Math.cos(2 * i * Math.PI / N ),
                    y: Math.sin(2 * i * Math.PI / N + Math.PI),
                    // x : Math.cos(2 * Math.random() * N * Math.PI / N),
                    // y : Math.cos(2 * Math.random() * N * Math.PI / N),
                    size: 0 + x.nodes.size[i],
                    scheme: x.nodes.scheme[i],
                    color: r2rgba(c, 0.9),
              });

            }

            for(i = 0; i < E; i++) {
                g.edges.push({
                    id: 'e' + i,
                    source: x.links.source[i],
                    target: x.links.target[i],
                    size: x.links.weight[i] / 2,
                    color: r2rgba(colors.edge, 0.2)
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

            nodeOuterBorderColor: 'default',
            nodeOuterBorderSize: 1,
            defaultNodeOuterBorderColor: '#eee',

            enableEdgeHovering: false,
            borderSize: 2,
            outerBorderSize: 3,
            // defaultNodeBorderColor: '#fff',
            // defaultNodeOuterBorderColor: 'rgb(236, 81, 72)',
            nodeHaloColor: 'rgba(236, 81, 72, 0.1)',
            nodeHaloSize: 30,
          }
        });

        var config = {  
            linLogMode:true,
            outboundAttractionDistribution: false,
            strongGravityMode:false,
            gravity:4,
            barnesHutTheta:5,
            edgeWeightInfluence:0,
            adjustSizes:false,
            barnesHutOptimize: false,
            startingIterations: 1,
            iterationsPerRender: 1,
            slowDown: 50,
            autoStop:true,
            avgDistanceThreshold:1e-7,
            // maxIterations:100000,
            easing:'quadraticInOut'
        };
        sigma.layouts.startForceLink(s, config);

        setTimeout(function(){
            sigma.layouts.configForceLink(s, {slowDown: 100}) 
        }, 2000);


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
    },

    update: function(state, x) {
        var current = global.getCurrentConfig(state)

        var i, s;
        var colors = current.colors

        g = current.graph;
        s = current.sigma;
        data = current.data;
        tick = data.options.seriesData[x.process_map - 1];

        for (i = 0; i < g.nodes.length; i++) {
            g.nodes[i].scheme = data.nodes["scheme." + tick][i];
            if(g.nodes[i].scheme == 'Pos') {
                c = _interpolateColor(colors.red, colors.white, data.nodes["color." + tick][i] / 0.02);
            } else if (g.nodes[i].scheme == 'Neg') {
                c = _interpolateColor(colors.blue, colors.white, data.nodes["color." + tick][i] / 0.02);
            } else {
                c = colors.grey;
            }

            g.nodes[i].color = r2rgba(c, 0.9);
        }

        if (!sigma.layouts.isForceLinkRunning()) {
        //     sigma.layouts.startForceLink(s, config);
            s.refresh();    
        }
        
    },

});




















// // Config for fruchtermanReingold
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
// if (!sigma.layouts.fruchtermanReingold.isRunning(s)) {
//     s.refresh();
// }


// // Config for layoutForce
// var config = {  
//  linLogMode:true,
//  outboundAttractionDistribution: false,
//  strongGravityMode:false,
//  gravity:4,
//  barnesHutTheta:3,
//  edgeWeightInfluence:0,
//  adjustSizes:false,
//  barnesHutOptimize: false,
//  startingIterations: 1,
//  iterationsPerRender: 1,
//  slowDown: 50,
//  autoStop:true,
//  avgDistanceThreshold:1e-8,
//  // maxIterations:200000,
//  // nodeSiblingsScale: 1.5,
//  easing:'quadraticInOut'
// }

// sigma.layouts.startForceLink(s, config);