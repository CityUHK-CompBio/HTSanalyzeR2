HTMLWidgets.widget(globalObj = {
    name: "forceGraph",
    type: "output",

    store: {},
    // rawdata: null, pause: false, mode: "all",
    // calcFunc: null, colorFunc: null, drawLegendFunc: null,

    initialize: function(el, width, height) {
        d3.select(el).append("svg")
            .attr("width", width)
            .attr("height", height);

        var simulation = d3.forceSimulation()
            .force("link", d3.forceLink())
            .force("charge", d3.forceManyBody())
            .force("collide", d3.forceCollide())
            .force("center", d3.forceCenter())
            .force("forceX", d3.forceX())
            .force("forceY", d3.forceY());

        simulation.force("center")
            .x(width / 2)
            .y(height / 2);

        return simulation
    },

    resize: function(el, width, height, simulation) {
        d3.select(el).select("svg")
            .attr("width", width)
            .attr("height", height);

        d3.select(el).select(".legend")
            .attr("transform", "translate(" + (parseInt(width) - 80) + ", 50)");

        d3.select(el).select(".title")
            .attr("transform", "translate(" + (parseInt(width) / 2) + ", 30)");

        simulation
            .force("center", d3.forceCenter(width / 2, height / 2));
    },

    renderValue: function(el, x, simulation) {
        if (x.update) {
            globalObj.update(el, x, simulation)
        } else {
            globalObj.construct(el, x, simulation)
        }
    },

    getStore: function() {
        id = d3.select('.tab-pane.active').attr('id');
        if(!(id in globalObj.store)) {
            globalObj.store[id] = { pause: false, mode: "all" }
        }
        return globalObj.store[id];
    },

    getActivePanel: function() {
        return d3.select(".tab-pane.active");
    },

    construct: function(el, x, simulation) {
        // x.nodes.scale = Array(x.nodes.size.length).fill(1);
        x.nodes.scale = Array.apply(null, Array(x.nodes.size.length)).map(Number.prototype.valueOf, 1);
        x.nodes.color_scheme = Array.apply(null, Array(x.nodes.size.length)).map(function() {return "default"});

        globalStore = globalObj.getStore();
        globalStore.rawdata = x;

        var options = x.options;
        var nodes = HTMLWidgets.dataframeToD3(x.nodes);
        var links = HTMLWidgets.dataframeToD3(x.links);

        var width = parseInt(el.offsetWidth);
        var height = parseInt(el.offsetHeight);

        var svg = d3.select(el).select("svg");
        svg.selectAll("*").remove();

        var view = svg.append("g").attr("class", "view");
        var zoomHandler = d3.zoom()
            .scaleExtent([1 / 2, 8])
            .on("zoom", function() { view.attr("transform", d3.event.transform); });

        svg.call(zoomHandler).on("dblclick.zoom", null);

        var color = d3.scaleLinear()
            .domain(options.colorDomain)
            .range(["#4575b4", "#ffffbf", "#a50026"])
            .interpolate(d3.interpolateHcl);
        var colorScheme1 = d3.scaleLinear()
            .domain(options.colorDomain)
            .range(["#d8b365", "#f5f5f5", "#5ab4ac"])
            .interpolate(d3.interpolateHcl);

        globalStore.colorFunc = {default : color, scheme1: colorScheme1};

        var link = view.append("g")
            .attr("class", "links")
            .selectAll("line")
            .data(links)
            .enter().append("line")
            .attr("class", "link")
            .attr("stroke", "grey")
            .attr("opacity", "0.6")
            .attr("stroke-width", function(d) {
                return d.weight;
            });

        var node = view.append("g")
            .attr("class", "nodes")
            .selectAll("rect")
            .data(nodes)
            .enter().append("g")
            .attr("class", "node")
            .on("mouseover", mouseover)
            .on("mouseout", mouseout)
            .call(d3.drag()
                .on("start", dragstarted)
                .on("drag", dragged)
                .on("end", dragended));


        var calc = {
            x: function(d) { return -d.size * d.scale },
            y: function(d) { return -d.size * d.scale },
            width: function(d) { return d.size * d.scale * 2 },
            height: function(d) { return d.size * d.scale * 2 },
            fill: function(d) { return color(d.color) },
            dx: function(d) {return d.size * d.scale + 2 },
            font: function(d) {return (d.size * d.scale / 2 + 7) + "px serif" },

            xL: function(d) { return -(d.size * d.scale + 8) },
            yL: function(d) { return -(d.size * d.scale + 8) },
            widthL: function(d) { return 2 * (d.size * d.scale + 8) },
            heightL: function(d) { return 2 * (d.size * d.scale + 8) },
            dxL: function(d) { return d.size * d.scale + 10 },
            fontL: function(d) { return (d.size * d.scale / 2 + 11) + "px serif"}
        };
        globalStore.calcFunc = calc;

        node.append("rect")
            .on("click", clicked)
            .attr("rx", 1000)
            .attr("ry", 1000)
            .attr("x", calc.x)
            .attr("y", calc.y)
            .attr("width", calc.width)
            .attr("height", calc.height)
            .attr("fill", calc.fill)
            .attr("stroke", "grey");

        node.append("text")
            .attr("fill", "black")
            .attr("dx", calc.dx)
            .attr("dy", ".35em")
            .style("font", calc.font)
            .style("opacity", "0.8")
            .text(function(d) {
                return d.label;
            });

        simulation
            .nodes(nodes)
            .on("tick", ticked);

        simulation.force("charge")
            .strength(options.charge);
        simulation.force("link")
            .id(function(d) {
                return d.id;
            })
            .distance(options.distance)
            .links(links);

        // simulation.alpha(1).restart();
        simulation.alphaTarget(0.3).restart();

        function ticked() {
            link
                .attr("x1", function(d) {
                    return d.source.x;
                })
                .attr("y1", function(d) {
                    return d.source.y;
                })
                .attr("x2", function(d) {
                    return d.target.x;
                })
                .attr("y2", function(d) {
                    return d.target.y;
                });
            node
                .attr("transform", function(d) {
                    return "translate(" + d.x + "," + d.y + ")"
                })
        }

        function clicked(d) {
            if (globalStore.mode == "selection") {
                var sel = d3.select(this)
                var psel = d3.select(this.parentNode);
                if (JSON.parse(psel.attr("selected"))) {
                    d.fixed = false;
                    sel.attr("stroke", "grey");
                    psel.attr("selected", false);
                } else {
                    d.fixed = true;
                    sel.attr("stroke", "red")
                    psel.attr("selected", true);
                }
            } else {
                d.fixed = !d.fixed;
            }
            dragended(d);
        }

        function dragstarted(d) {
            if (!d3.event.active) simulation.alphaTarget(0.3).restart();
            d.fx = d.x;
            d.fy = d.y;
        }

        function dragged(d) {
            d.fx = d3.event.x;
            d.fy = d3.event.y;
        }

        function dragended(d) {
            if (!d3.event.active) simulation.alphaTarget(0);
            if (!d.fixed) {
                d.fx = null;
                d.fy = null;
            } else {
                d.fx = d.x;
                d.fy = d.y;
            }
            if (globalStore.pause) {
                simulation.stop();
            }
        }

        function mouseover() {
            d3.select(this).select("rect").transition()
                .duration(300)
                .attr("x", calc.xL)
                .attr("y", calc.yL)
                .attr("width", calc.widthL)
                .attr("height", calc.heightL)
            d3.select(this).select("text").transition()
                .duration(300)
                .attr("dx", calc.dxL)
                .style("font", calc.fontL)
                .style("opacity", 1);
        }

        function mouseout() {
            d3.select(this).select("rect").transition()
                .duration(500)
                .attr("x", calc.x)
                .attr("y", calc.y)
                .attr("width", calc.width)
                .attr("height", calc.height)
            d3.select(this).select("text").transition()
                .duration(500)
                .attr("dx", calc.dx)
                .style("font", calc.font)
                .style("opacity", 0.8);
        }

        function drawLegend() {
            function pair(array) {
                return array.slice(1).map(function(b, i) { return [array[i], b]; });
            }
            function domain(max, min) {
                if(min >= 0 && max <= 1) { return [1, 0]; }
                if(max - min <= 0.2) { return [max + 0.5, min - 0.5]; }
                return [max, min];
            }

            activePanel = globalObj.getActivePanel();
            nodes = activePanel.selectAll(".node");
            legend = activePanel.select(".legend");
            colorFunc = globalObj.getStore().colorFunc;

            colorValues = {};
            nodes.each(
                function(d) {
                    if (!(d.color_scheme in colorValues)) colorValues[d.color_scheme] = [];
                    colorValues[d.color_scheme].push(d.color);
                });

            legend.selectAll("*").remove();
            offset = 0;
            for(key in colorValues) {
                max = Math.max.apply(null, colorValues[key]);
                min = Math.min.apply(null, colorValues[key]);
                colorDomain = domain(max, min);
                colorScale = colorFunc[key];

                g = legend.append("g")
                    .attr("transform", "translate(" + offset + ", 0)");

                var legendScale = d3.scaleLinear()
                    .domain(colorDomain)
                    .range([0, 200])
                    .nice();

                g.selectAll("rect")
                    .data(pair(legendScale.ticks(10)))
                    .enter().append("rect")
                    .attr("width", 8)
                    .attr("y", function(d) {
                        return legendScale(d[0]);
                    })
                    .attr("height", function(d) {
                        return legendScale(d[1]) - legendScale(d[0]);
                    })
                    .style("fill", function(d) {
                        return colorScale(d[1]);
                    });

                var axis = d3.axisRight(legendScale)
                    .tickSize(10)
                    .tickFormat(d3.format("+.1f"));
                g.call(axis);

                offset -= 40;
            }
        }
        globalStore.drawLegendFunc = drawLegend;

        var legend = svg.append("g")
            .attr("class", "legend")
            .attr("transform", "translate(" + (width - 80) + ", 50)");

        drawLegend();

        var title = svg.append("g")
            .attr("class", "title")
            .attr("transform", "translate(" + (width / 2) + ", 30)")
            .append("text")
            .style("font", "20px")
            .style("fill", "black")
            .attr("text-anchor", "middle")
            .attr("font-weight", "bold")
            .text(options.title);

        var icons = ['\uf01e', '\uf0c7'];
        var buttons = svg.append("g")
            .selectAll(".button")
            .data(icons).enter()
            .append("g")
            .attr("class", "button")
            .style("cursor", "pointer")
            .on("click", btnClick)
            .on("mouseover", function() {
                d3.select(this).select("rect")
                .transition().duration(300)
                .attr("fill-opacity", "0.9");
            })
            .on("mouseout", function() {
                d3.select(this).select("rect")
                .transition().duration(300)
                .attr("fill-opacity", "0.3");
            });

        buttons.append("rect")
            .attr("width", 25).attr("height", 25)
            .attr("x", function(d, i) {return i * 30})
            .attr("rx", 5).attr("ry", 5)
            .attr("fill", "#0d6dbc")
            .attr("fill-opacity", "0.3");
        buttons.append("text")
            .attr("font-family", "FontAwesome")
            .attr("x",function(d,i) {
                return 0 + (25+5)*i + 25/2;
            })
            .attr("y",0+25/2)
            .attr("text-anchor","middle")
            .attr("dominant-baseline","central")
            .attr("fill","white")
            .text(function(d) {return d;})

        function btnClick(d, i) {
            if(i == 0) {
                // reset zoom
                svg.call(zoomHandler.transform, d3.zoomIdentity);
            } else if (i == 1) {
                buttons.attr("visibility", "hidden");
                var svgData = svg.attr("version", 1.1)
                    .attr("xmlns", "http://www.w3.org/2000/svg")
                    .node().parentNode.outerHTML;
                buttons.attr("visibility", "visible");

                var svgBlob = new Blob([svgData], {type:"image/svg+xml;charset=utf-8"});
                var svgUrl = URL.createObjectURL(svgBlob);
                var tmpLink = document.createElement("a");
                tmpLink.href = svgUrl;
                tmpLink.download = "network.svg";
                document.body.appendChild(tmpLink);
                tmpLink.click();
                document.body.removeChild(tmpLink);
            }
        }
    },

    update: function(el, x, simulation) {
        globalStore = globalObj.getStore();
        activePanel = globalObj.getActivePanel();

        if ('pause' in x) {
            globalStore.pause = x.pause;
            x.pause ? simulation.stop() : simulation.restart();
        }

        function select(type) {
            var map = {node: "rect", label: "text"};
            var cls = map[type];

            if (globalStore.mode == 'selection') {
                return activePanel.selectAll(".node[selected='true'] > " + cls);
            }
            var sel = activePanel.selectAll(".node > " + cls);
            if (globalStore.mode == 'all') {
                return sel;
            }
            var set = globalStore.rawdata.options.nodeOptions[globalStore.mode];
            return sel.filter(function(d) {
                return set.indexOf(d.id) >= 0
            });
        }

        if ('selection' in x) {
            //TODO, the set name can also be 'selection'
            globalStore.mode = x.selection;
            if (globalStore.mode != 'selection') {
                activePanel.selectAll(".node").attr("selected", false);
                activePanel.selectAll(".node > rect").attr("stroke", "grey");
            }
        }

        if ('shape' in x) {
            var sel = select('node')

            if (x.shape == 'circle') {
                sel.transition().duration(300).attr("rx", 1000).attr("ry", 1000)
            } else if (x.shape == 'rect') {
                sel.transition().duration(300).attr("rx", 0).attr("ry", 0)
            }
        }

        if('color' in x) {
            var colorFunc = globalStore.colorFunc;

            select('node')
                .each(function(d) {d.color_scheme = x.color})
                .transition().duration(300)
                .attr("fill", function(d) {return colorFunc[d.color_scheme](d.color)});

            globalStore.drawLegendFunc();
        }

        if ('scale' in x) {
            var scale = parseFloat(x.scale);

            select('node')
                .each(function(d) { d.scale = scale })
                .transition().duration(300)
                .attr("x", globalStore.calcFunc.x)
                .attr("y", globalStore.calcFunc.y)
                .attr("width", globalStore.calcFunc.width)
                .attr("height", globalStore.calcFunc.height);
            select('label')
                .transition().duration(300)
                .attr("dx", globalStore.calcFunc.dx)
                .style("font", globalStore.calcFunc.font);
        }

        if ('nodename' in x) {
            key = "label_" + x.nodename
            select('label').text(function(d) {
                return key in d ? d[key] : d.label;
            });
        }

        if ('label' in x) {
            select('label').transition().duration(300).attr("visibility", x.label ? "visible" : "hidden");
        }

        if ('charge' in x) {
            simulation.force("charge").strength(x.charge);
            simulation.alphaTarget(0.3).restart();
        }

        if ('distance' in x) {
            simulation.force("link").distance(x.distance);
            simulation.alphaTarget(0.3).restart();
        }

        if ('process' in x) {
            var options = globalStore.rawdata.options;
            var series = options.seriesData;
            var index = JSON.parse(x.process) - 1;

            var sel = activePanel.selectAll(".node > rect");
            var colorFunc = globalStore.colorFunc;

            sel.transition().duration(300)
            .attr("fill", function(d) {
                if(index == 0) return "#fff";
                return colorFunc[d.color_scheme](d["color." + series[index]]);
            });
        }
    }
});
