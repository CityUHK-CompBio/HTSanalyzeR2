HTMLWidgets.widget(global = {
    name: "forceGraph",
    type: "output",

    // storeFormat:
    // store: {elementId1: elementState1, elementId2: elementState2, ...}
    // elementState: {width, height, svg, simulation, controllers, currentSubId, hashId1: subState1, hashId2: Substate2, ...}
    // subState: {pause, charge, distance, title, ...}
    store: {},

    defaultState: {
        pause: false,
        charge: -400,
        distance: 200,

        title: "",
        titleSize: 22,
        legendTitle: "",

        label: "id",
        labelColor: "#000000",  // black
        labelOpacity: 0.8,
        labelScale: 1,

        nodeScale: 1,
        nodeScheme: "linear2",
        nodeShape: "circle",
        nodeBorderColor: "#808080", // grey
        nodeBorderWidth: 1,
        nodeBorderOpacity: 1,

        edgeScale: 1,
        edgeColor: "#808080",  // grey
        edgeOpacity: 0.6,

        palettes: {
            linear2: { domain: [-1, 1], range: ["#0E0F7E", "#87420E"] },
            linear3: { domain: [-1, 0, 1], range: ["#69D2E7", "#E3E3E3", "#FA6900"] },
            dual: { domain: [0, 1], range: ["#F03C18", "#BEC9E8"] },
            dualPos: { domain: [0, 1], range: ["#F03C18", "#BEC9E8"] },
            dualNeg: { domain: [0, 1], range: ["#517281", "#BEC9E8"] },
        },
        scalers: {
            wrapper: function(schemeId, color, scheme) {
                if(color == null) {
                    return "#FFFFFF"
                }
                if(schemeId == "dual") {
                    return this[schemeId+scheme](color);
                } else {
                    return this[schemeId](color);
                }
            }
            //generated from palettes when constructing views.
        },

        modified: false
    },

    initialize: function(el, width, height) {
        el.style.height = "90vh";

        var state = global.getElementState(el);

        var svg = d3.select(el).append("svg")
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

        $.extend(state, { svg: svg, width: width, height: height, simulation: simulation })

        return simulation;
    },

    resize: function(el, width, height, simulation) {
        var state = global.getElementState(el);
        $.extend(state, { width: width, height: height });

        d3.select(el).select("svg")
            .attr("width", width)
            .attr("height", height);

        d3.select(el).select("svg")
            .select(".title")
            .attr("transform", "translate(" + (state.width / 2) + ", 40)");

        d3.select(el).select("svg")
            .select(".legend")
            .attr("transform", "translate(" + (state.width - 100) + ", 50)");

        simulation
            .force("center", d3.forceCenter(width / 2, height / 2));
    },

    getElementState: function(el) {
        var elId = el.id;
        if (!(elId in global.store)) {
            global.store[elId] = {elId: elId, controller: {}};
        }
        return global.store[elId];
    },

    getSelection: function(elState, type) {
        var svg = elState.svg;
        switch (type) {
            case 'node':
                return svg.selectAll(".node");
            case 'circle':
                return svg.selectAll(".node > .shape-circle");
            case 'polygon':
                return svg.selectAll(".node > .shape-polygon");
            case 'label':
                return svg.selectAll(".node > text");
            case 'edge':
                return svg.selectAll(".link");
            case 'title':
                return svg.selectAll(".title > text");
            case 'legend':
                return svg.select(".legend");
            case 'legendTitle':
                return svg.select(".legend > text");
        }
    },

    renderValue: function(el, x, simulation) {
        // console.log(x);
        var elState = global.getElementState(el);

        if (x.update) {
            global.update(elState, x, simulation);
        } else {
            var hashKey = JSON.stringify(x).hashCode().toString();
            if(!(hashKey in elState)) {
                elState[hashKey] = $.extend({}, global.defaultState);
            }
            elState.currentSubId = hashKey;
            global.construct(elState, x, simulation);
        }
    },

    construct: function(elState, x, simulation) {
        // TODO: use smarter loader, check proverty available.
        // console.log(x)
        var options = x.options;
        var curState = elState[elState.currentSubId];

        if(!curState.modified) {
        //     curState.palettes.default.domain = options.colorDomain;
        //     curState.palettes.scheme1.domain = options.colorDomain;
        //     curState.palettes.scheme2.domain = options.colorDomain;

            var keys = ["charge","distance", "seriesData",
                "title","titleSize","legendTitle",
                "label","labelColor","labelOpacity","labelScale",
                "edgeScale","edgeColor","edgeOpacity",
                "nodeScale","nodeScheme","nodeShape","nodeBorderColor","nodeBorderWidth","nodeBorderOpacity"]

            for(var i in keys) {
                var key = keys[i]
                if(options.hasOwnProperty(key)) {
                    curState[key] = options[key];
                }
            }

            curState.modified = true;
        }

        var nodes = HTMLWidgets.dataframeToD3(x.nodes);
        var links = HTMLWidgets.dataframeToD3(x.links);

        var svg = elState.svg;
        svg.selectAll("*").remove();
        var view = svg.append("g").attr("class", "view");

        var zoomHandler = d3.zoom()
            .scaleExtent([1 / 4, 8])
            .on("zoom", function() {
                view.attr("transform", d3.event.transform);
            });
        svg.call(zoomHandler).on("dblclick.zoom", null);

        for (var scheme in curState.palettes) {
            var palette = curState.palettes[scheme];
            curState.scalers[scheme] = d3.scaleLinear()
                .domain(palette.domain)
                .range(palette.range)
                .interpolate(d3.interpolateHcl);
        }
        curState.scalers["dual"] = curState.scalers["dualPos"];

        // Start
        var title = svg.append("g")
            .attr("class", "title")
            .attr("transform", "translate(" + (elState.width / 2) + ", 40)")
            .append("text")
            .attr("font-size", curState.titleSize)
            .attr("text-anchor", "middle")
            .attr("font-weight", "bold")
            .style("fill", "black")
            .text(curState.title);

        var legend = svg.append("g")
            .attr("class", "legend")
            .attr("transform", "translate(" + (elState.width - 100) + ", 50)");

        global.drawLegend(elState);

        var link = view.append("g")
            .attr("class", "links")
            .selectAll("line")
            .data(links)
            .enter().append("line")
            .attr("class", "link")
            .attr("stroke", curState.edgeColor)
            .attr("opacity", curState.edgeOpacity)
            .attr("stroke-width", function(d) {
                return d.weight * curState.edgeScale;
            });

        var node = view.append("g")
            .attr("class", "nodes")
            .selectAll("g")
            .data(nodes)
            .enter().append("g")
            .attr("class", "node")
            .on("mouseover", mouseover)
            .on("mouseout", mouseout)
            .call(d3.drag()
                .on("start", dragstarted)
                .on("drag", dragged)
                .on("end", dragended));

        node.append("circle")
            .attr("class", "shape-circle")
            .attr("r", function(d) {
                return d.size * curState.nodeScale;
            })
            .attr("fill", function(d) {
                return curState.scalers.wrapper(curState.nodeScheme, d.color, d.scheme);
            })
            .attr("stroke", curState.nodeBorderColor)
            .attr("stroke-width", curState.nodeBorderWidth)
            .attr("stroke-opacity", curState.nodeBorderOpacity)
            .attr("visibility", curState.nodeShape == "circle" ? "visible" : "hidden");

        node.append("polygon")
            .attr("class", "shape-polygon")
            .attr("points", function(d) {
                return d3ForceCalc.points(curState.nodeShape, d.size * curState.nodeScale);
            })
            .attr("fill", function(d) {
                return curState.scalers.wrapper(curState.nodeScheme, d.color, d.scheme);
            })
            .attr("stroke", curState.nodeBorderColor)
            .attr("stroke-width", curState.nodeBorderWidth)
            .attr("stroke-opacity", curState.nodeBorderOpacity)
            .attr("visibility", curState.nodeShape != "circle" ? "visible" : "hidden");

        node.append("text")
            .attr("fill", curState.labelColor)
            .attr("dx", function(d) {
                return d.size * curState.nodeScale + 2;
            })
            .attr("dy", ".35em")
            .style("font", function(d) {
                return (d.size * curState.nodeScale * curState.labelScale / 2 + 7) + "px serif"
            })
            .style("opacity", curState.labelOpacity)
            .text(function(d) {
                return d["label_" + curState.label];
            });

        node.filter(function(d){return d.color == null}).attr("opacity", 0)

        simulation.nodes(nodes)
            .on("tick", ticked);
        simulation.force("charge")
            .strength(curState.charge);
        simulation.force("link")
            .id(function(d) {
                return d.id;
            })
            .distance(curState.distance)
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
            if (curState.pause) {
                simulation.stop();
            }
        }

        function mouseover() {
            d3.select(this).select("circle").transition()
                .duration(300)
                .attr("r", function(d) {
                    return (d.size * curState.nodeScale + 8)
                });
            d3.select(this).select("polygon").transition()
                .duration(300)
                .attr("points", function(d) {
                    return d3ForceCalc.points(curState.nodeShape, d.size * curState.nodeScale * 1.5);
                });
            d3.select(this).select("text")
                .transition().duration(300)
                .attr("dx", function(d) {
                    return d.size * curState.nodeScale + 10
                })
                .style("font", function(d) {
                    return (d.size * curState.nodeScale * curState.labelScale / 2 + 11) + "px serif"
                })
                .style("opacity", 1);
        }

        function mouseout() {
            d3.select(this).select("circle").transition()
                .duration(500)
                .attr("r", function(d) {
                    return (d.size * curState.nodeScale)
                })
            d3.select(this).select("polygon").transition()
                .duration(300)
                .attr("points", function(d) {
                    return d3ForceCalc.points(curState.nodeShape, d.size * curState.nodeScale);
                });
            d3.select(this).select("text").transition()
                .duration(500)
                .attr("dx", function(d) {
                    return d.size * curState.nodeScale + 2;
                })
                .style("font", function(d) {
                    return (d.size * curState.nodeScale * curState.labelScale / 2 + 7) + "px serif"
                })
                .style("opacity", curState.labelOpacity);
        }

        // For setting panel
        global.generateControllers(elState);
        configureSettingPanel(elState);
    },

    generateControllers: function(elState) {
        var simulation = elState.simulation;
        var curState = elState[elState.currentSubId];

        // General
        elState.controller.title = function(val) {
            curState.title = val;
            var title = global.getSelection(elState, 'title');
            title.text(curState.title);
        }

        elState.controller.titleSize = function(val) {
            curState.titleSize = val;
            var title = global.getSelection(elState, 'title');
            title.style("font-size", curState.titleSize);
        }

        elState.controller.legendTitle = function(val) {
            curState.legendTitle = val;
            var legendTitle = global.getSelection(elState, 'legendTitle');
            legendTitle.text(curState.legendTitle);
        }

        elState.controller.charge = function(val) {
            curState.charge = val;
            simulation.force("charge").strength(curState.charge);
            simulation.alphaTarget(0.3).restart();
            if (curState.pause) {
                setTimeout(function() { simulation.stop(); }, 600);
            }
        }
        elState.controller.distance = function(val) {
            curState.distance = val;
            simulation.force("link").distance(curState.distance);
            simulation.alphaTarget(0.3).restart();
            if (curState.pause) {
                setTimeout(function() { simulation.stop(); }, 600);
            }
        }

        //Label
        elState.controller.labelOption = function(type) {
            // type == id, term, none
            curState.label = type;
            var sel = global.getSelection(elState, 'label');
            if (curState.label == 'none') {
                sel.transition().duration(300).attr("visibility", "hidden");
            } else {
                var key = "label_" + curState.label;
                sel.transition().duration(300).attr("visibility", "visible").text(function(d) {
                    return key in d ? d[key] : d.label;
                });
            }
        }

        elState.controller.labelColor = function(color) {
            curState.labelColor = color;
            var sel = global.getSelection(elState, 'label');
            sel.transition().duration(300).attr("fill", curState.labelColor)
        }

        elState.controller.labelOpacity = function(val) {
            curState.labelOpacity = val;
            var sel = global.getSelection(elState, 'label');
            sel.transition().duration(300).style("opacity", curState.labelOpacity)
        }

        elState.controller.labelScale = function(val) {
            curState.labelScale = val;
            var sel = global.getSelection(elState, 'label');
            sel.transition().duration(300).style("font", function(d) {
                return (d.size * curState.nodeScale * curState.labelScale / 2 + 7) + "px serif"
            })
        }

        // Node
        elState.controller.nodeShape = function(shape) {
            // shape == circle, triangle, rectangle, diamond
            curState.nodeShape = shape;
            var sel_circle = global.getSelection(elState, 'circle');
            var sel_polygon = global.getSelection(elState, 'polygon');

            if (shape == 'circle') {
                sel_circle.transition().duration(300).attr('visibility', 'visible');
                sel_polygon.transition().duration(300).attr('visibility', 'hidden');
            } else {
                sel_circle.transition().duration(300).attr('visibility', 'hidden');
                sel_polygon.transition().duration(300).attr('visibility', 'visible')
                    .attr("points", function(d) {
                        return d3ForceCalc.points(curState.nodeShape, d.size * curState.nodeScale);
                    })
            }
        }

        elState.controller.nodeScale = function(val) {
            curState.nodeScale = val;
            var sel_circle = global.getSelection(elState, 'circle');
            var sel_polygon = global.getSelection(elState, 'polygon');
            sel_circle.attr("r", function(d) {
                return d.size * curState.nodeScale;
            })
            sel_polygon.attr("points", function(d) {
                return d3ForceCalc.points(curState.nodeShape, d.size * curState.nodeScale);
            })
        }

        elState.controller.nodeScheme = function(schemeId) {
            // scheme: "linear2", "linear3", "dual"
            curState.nodeScheme = schemeId;
            var sel_circle = global.getSelection(elState, 'circle');
            var sel_polygon = global.getSelection(elState, 'polygon');
            sel_circle.transition().duration(300).attr("fill", function(d) {
                return curState.scalers.wrapper(schemeId, d.color, d.scheme);
            })
            sel_polygon.transition().duration(300).attr("fill", function(d) {
                return curState.scalers.wrapper(schemeId, d.color, d.scheme);
            })
            global.drawLegend(elState);
        }

        elState.controller.nodeBorderColor = function(color) {
            curState.nodeBorderColor = color;
            var sel_circle = global.getSelection(elState, 'circle');
            var sel_polygon = global.getSelection(elState, 'polygon');
            sel_circle.transition().duration(300).attr("stroke", function(d) {
                return curState.nodeBorderColor
            })
            sel_polygon.transition().duration(300).attr("stroke", function(d) {
                return curState.nodeBorderColor
            })
        }

        elState.controller.nodeBorderOpacity = function(val) {
            curState.nodeBorderOpacity = val;
            var sel_circle = global.getSelection(elState, 'circle');
            var sel_polygon = global.getSelection(elState, 'polygon');
            sel_circle.transition().duration(300).attr("stroke-opacity", function(d) {
                return curState.nodeBorderOpacity
            })
            sel_polygon.transition().duration(300).attr("stroke-opacity", function(d) {
                return curState.nodeBorderOpacity
            })
        }

        elState.controller.nodeBorderWidth = function(val) {
            curState.nodeBorderWidth = val;
            var sel_circle = global.getSelection(elState, 'circle');
            var sel_polygon = global.getSelection(elState, 'polygon');
            sel_circle.transition().duration(300).attr("stroke-width", function(d) {
                return curState.nodeBorderWidth
            })
            sel_polygon.transition().duration(300).attr("stroke-width", function(d) {
                return curState.nodeBorderWidth
            })
        }


        // Edge
        elState.controller.edgeColor = function(color) {
            curState.edgeColor = color;
            var sel = global.getSelection(elState, 'edge');
            sel.transition().duration(300).attr('stroke', curState.edgeColor);
        }

        elState.controller.edgeOpacity = function(val) {
            curState.edgeOpacity = val;
            var sel = global.getSelection(elState, 'edge');
            sel.transition().duration(300).attr('opacity', curState.edgeOpacity);
        }

        elState.controller.edgeScale = function(val) {
            curState.edgeScale = val;
            var sel = global.getSelection(elState, 'edge');
            sel.transition().duration(300).attr('stroke-width', function(d) {
                return d.weight * curState.edgeScale;
            });
        }

        //Schemes
        elState.controller.changeScheme = function(schemeId, domain, range) {
            // schemeId: "linear2", "linear3", "dualPos", "dualNeg"
            var palette = { domain: domain, range: range };
            curState.palettes[schemeId] = palette;
            var scaler = curState.scalers[schemeId];
            scaler.domain(palette.domain).range(palette.range);

            // TODO: revise needed
            scaler['dual'] = scaler['dualPos'];

            if (schemeId.startsWith(curState.nodeScheme)) {
                elState.controller.nodeScheme(curState.nodeScheme);
                global.drawLegend(elState);
            }
        }

        // BorderButtons
        elState.controller.pause = function() {
            curState.pause = !curState.pause;
            curState.pause ? simulation.stop() : simulation.restart();
        }

        elState.controller.saveSvg = function() {
            var svg = elState.svg;
            var svgData = svg.attr("version", 1.1)
                .attr("xmlns", "http://www.w3.org/2000/svg")
                .node().parentNode.outerHTML;

            var svgBlob = new Blob([svgData], { type: "image/svg+xml;charset=utf-8" });
            var svgUrl = URL.createObjectURL(svgBlob);
            var tmpLink = document.createElement("a");
            tmpLink.href = svgUrl;
            tmpLink.download = "network.svg";
            document.body.appendChild(tmpLink);
            tmpLink.click();
            document.body.removeChild(tmpLink);
        }
    },

    drawLegend: function(elState) {
        var curState = elState[elState.currentSubId];

        function pair(array) {
            return array.slice(1).map(function(b, i) {
                return [array[i], b];
            });
        }

        if(curState.nodeScheme != "dual") {
        	palette = curState.palettes[curState.nodeScheme];
	        colorFunc = curState.scalers[curState.nodeScheme];
	        colorDomain = [palette.domain[palette.domain.length - 1], palette.domain[0]];

	        var legendScale = d3.scaleLinear()
	            .domain(colorDomain)
	            .range([0, 200])
	            .nice();

	        var axis = d3.axisRight(legendScale)
	            .tickSize(10)
	            .tickFormat(d3.format("+.1f"));

	        var legend = global.getSelection(elState, "legend");
	        legend.selectAll("*").remove();
	        legend.selectAll("rect")
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
	                return colorFunc(d[1]);
	            });
	        legend.call(axis);

	        legend.append("text")
	            .attr("transform", "translate(10, 215)")
	            .attr("font-size", 10)
	            .attr("text-anchor", "middle")
	            .style("fill", "black")
	            .text(curState.legendTitle);
        } else {
        	// curState.nodeScheme == 'dual'
        	var colorFunc = {Pos: curState.scalers["dualPos"], Neg: curState.scalers["dualNeg"]};
        	var colorDomain = {};
			var domPos = curState.palettes["dualPos"]["domain"];
			var domNeg = curState.palettes["dualNeg"]["domain"];
			colorDomain.Pos = [domPos[1], domPos[0]];
			colorDomain.Neg = [domNeg[1], domNeg[0]];

	        var legend = global.getSelection(elState, "legend");
	        legend.selectAll("*").remove();

	        var subClasses = ["Pos", "Neg"];
	        for(var i in subClasses) {
	        	i = JSON.parse(i);
	        	var sc = subClasses[i];
	        	var legendSc = legend.append("g")
	        		.attr("class", "dual" + sc)
	        		.attr("transform", "translate(" + (1 - i) * 40 + ", 0)");

	        	var legendScale = d3.scaleLinear()
		            .domain(colorDomain[sc])
		            .range([0, 200])
		            .nice();
		        var axis = d3.axisRight(legendScale)
		            .tickSize(10)
		            .tickFormat(d3.format("+.1f"));
				legendSc.selectAll("rect")
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
		                return colorFunc[sc](d[1]);
		            });
		        legendSc.call(axis);
	        }

	        legend.append("text")
	            .attr("transform", "translate(10, 215)")
	            .attr("font-size", 10)
	            .attr("text-anchor", "middle")
	            .style("fill", "black")
	            .text(curState.legendTitle);
        }

    },

    update: function(elState, x, simulation) {
        // update interface for R
        var curState = elState[elState.currentSubId];
        var schemeId = curState.nodeScheme;

        if ('process_map' in x) {
            var series = curState.seriesData;
            var index = JSON.parse(x.process_map) - 1;

            if(series == null) {
                return;
            }

            var node = global.getSelection(elState, 'node');
            var link = global.getSelection(elState, 'edge');
            var sel_circle = global.getSelection(elState, 'circle');
            var sel_polygon = global.getSelection(elState, 'polygon');

            node.each(function(d) {d.color = d["color." + series[index]] });
            node.each(function(d) { d.scheme = d["scheme." + series[index]] });
            link.each(function(d) {d.weight = d["weight." + series[index]] });

            node.transition().duration(300).attr("opacity", 1)
                .filter(function(d){return d.color == null}).attr("opacity", 0);
            sel_circle.transition().duration(300).attr("fill", function(d) {
                return curState.scalers.wrapper(schemeId, d.color, d.scheme);
            })
            sel_polygon.transition().duration(300).attr("fill", function(d) {
                return curState.scalers.wrapper(schemeId, d.color, d.scheme);
            })
            link.transition().duration(300).attr("stroke-width", function(d) {
                return d.weight * curState.edgeScale;
            });
        }

        if ('process_net' in x) {
            var series = curState.seriesData;
            var index = JSON.parse(x.process_net) - 1;

            if(series == null) {
                return;
            }

            var node = global.getSelection(elState, 'node');
            var link = global.getSelection(elState, 'edge');
            var sel_circle = global.getSelection(elState, 'circle');
            var sel_polygon = global.getSelection(elState, 'polygon');

            node.each(function(d) {d.color = d["color." + series[index]] });

            node.transition().duration(300).attr("opacity", 1)
                .filter(function(d){return d.color == null}).attr("opacity", 0);
            sel_circle.transition().duration(300).attr("fill", function(d) {
                return curState.scalers.wrapper(schemeId, d.color, d.scheme);
            })
            sel_polygon.transition().duration(300).attr("fill", function(d) {
                return curState.scalers.wrapper(schemeId, d.color, d.scheme);
            })
        }

    }
}
);
