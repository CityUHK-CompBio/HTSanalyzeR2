HTMLWidgets.widget(globalObj = {
  name: "forceGraph",
  type: "output",
  mode: "all",

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

    return simulation;
  },

  resize: function(el, width, height, simulation) {

    d3.select(el).select("svg")
    .attr("width", width)
    .attr("height", height);

    d3.select(el).select(".legend")
    .attr("transform", "translate(" + (parseInt(width) - 80) + ", 50)");

    d3.select(el).select(".title")
    .attr("transform", "translate(" + (parseInt(width) / 2) + ", 30)")

    simulation
    .force("center", d3.forceCenter(width / 2, height / 2))

  },

  updateValue: function(el, x, simulation) {
    if('pause' in x) {
      x.pause ? simulation.stop() : simulation.restart()
    }

    if('selection' in x) {
        //TODO, the set name can also be 'selection'
        globalObj.mode = x.selection;
        if(globalObj.mode != 'selection') {
            d3.selectAll(".node").attr("selected", false);
            d3.selectAll(".node > rect").attr("stroke", "grey");
        }
    }

    function getSelection(type) {
        if(type == 'node') {
            if(globalObj.mode == 'selection') {
                return d3.selectAll(".node[selected='true'] > rect");
            }
            return d3.selectAll(".node > rect");
        } else if(type == 'label') {
            if(globalObj.mode == 'selection') {
                return d3.selectAll(".node[selected='true'] > text");
            }
            return d3.selectAll(".node > text");
        }
    }

    if('shape' in x) {
        // var sel = x.selection
        selection = getSelection('node')

        if(x.shape == 'circle') {
            selection.attr("rx", 1000).attr("ry", 1000)
        } else if(x.shape == 'rect') {
            selection.attr("rx", 0).attr("ry", 0)
        }

        // arr.forEach(
        //     function (id) {
        //         d3.select("#" + id)
        //             .select("rect").transition().duration(300)
        //             .attr("rx", 0)
        //             .attr("ry", 0);
        //     }
        // );
    }

    // TODO size change shoule be a scale
    if('size' in x ) {
        var size = parseInt(x.size)

        d3.selectAll(".node").each(function(d) {d.size = size});

        getSelection('node').attr("x", -size)
            .attr("y", -size)
            .attr("width", size * 2)
            .attr("height", size * 2);
        getSelection('label').attr("dx", size + 2)
            .style("font", (size / 2 + 7) + "px serif")
    }

    if('label' in x) {
        // var sel = x.selection
        selection = getSelection('label')
        if(x.label) {
            selection.attr("visibility", "visible")
        } else {
            selection.attr("visibility", "hidden")
        }
    }

    if('charge' in x) {
        simulation.force("charge").strength(x.charge);
        simulation.alphaTarget(0.3).restart();
    }
    if('distance' in x) {
        simulation.force("link").distance(x.distance);
        simulation.alphaTarget(0.3).restart();
    }

  },

  renderValue: function(el, x, simulation) {
    if(x.update) {
        this.updateValue(el, x, simulation)
        return
    }

    var options = x.options;
    var nodes = HTMLWidgets.dataframeToD3(x.nodes);
    var links = HTMLWidgets.dataframeToD3(x.links);

    var width = parseInt(el.offsetWidth);
    var height = parseInt(el.offsetHeight);

    var svg = d3.select(el).select("svg");
    svg.selectAll("*").remove();

    var color = d3.scaleLinear()
    .domain(options.colorDomain)
    .range(["#4575b4", "#ffffbf", "#a50026"])
    .interpolate(d3.interpolateHcl);

    var link = svg.append("g")
        .attr("class", "links")
        .selectAll("line")
        .data(links)
        .enter().append("line")
        .attr("class", "link")
        .attr("stroke", "grey")
        .attr("opacity", "0.6")
        .attr("stroke-width", function(d) { return d.weight; });

    var node = svg.append("g")
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

    node.append("rect")
        .on("click", clicked)
        .attr("rx", 1000)
        .attr("ry", 1000)
        .attr("x", function (d) {
            return -d.size;
        })
        .attr("y", function (d) {
            return -d.size;
        })
        .attr("width", function (d) {
            return d.size * 2;
        })
        .attr("height", function (d) {
            return d.size * 2;
        })
        .attr("fill", function(d) { return color(d.color);})
        .attr("stroke", "grey");

    node.append("text")
        .attr("fill", function(d) {return "black"})
        .attr("dx", function(d) {return d.size + 2})
        .attr("dy", ".35em")
        .style("font", function(d) {return (d.size / 2 + 7) + "px serif"})
        .style("opacity", "0.8")
        .text(function(d) {return d.label;});

    simulation
        .nodes(nodes)
        .on("tick", ticked);

    simulation.force("charge")
        .strength(options.charge);
    simulation.force("link")
        .id(function(d) {return d.id;})
        .distance(options.distance)
        .links(links);

    // simulation.alpha(1).restart();
    simulation.alphaTarget(0.3).restart();

    function ticked() {
        link
            .attr("x1", function(d) { return d.source.x; })
            .attr("y1", function(d) { return d.source.y; })
            .attr("x2", function(d) { return d.target.x; })
            .attr("y2", function(d) { return d.target.y; });
        node
            .attr("transform", function (d) {
                return "translate(" + d.x + "," + d.y + ")"
            })
    }

    function clicked(d) {
        if(globalObj.mode == "selection") {
            var sel = d3.select(this)
            var psel = d3.select(this.parentNode);
            if(JSON.parse(psel.attr("selected"))) {
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
    }

    function mouseover() {
      d3.select(this).select("rect").transition()
        .duration(300)
        .attr("x", function (d) {
            return -(d.size + 8);
        })
        .attr("y", function (d) {
            return -(d.size + 8);
        })
        .attr("width", function (d) {
            return 2 * (d.size + 8);
        })
        .attr("height", function (d) {
            return 2 * (d.size + 8);
        });
      d3.select(this).select("text").transition()
        .duration(300)
        .attr("dx", function(d) {return d.size + 10;})
        .style("font", function(d) {return (d.size / 2 + 11) + "px serif"})
        .style("opacity", 1);
    }

    function mouseout() {
      d3.select(this).select("rect").transition()
        .duration(500)
        .attr("x", function (d) {
            return -d.size;
        })
        .attr("y", function (d) {
            return -d.size;
        })
        .attr("width", function (d) {
            return d.size * 2;
        })
        .attr("height", function (d) {
            return d.size * 2;
        });
      d3.select(this).select("text").transition()
        .duration(500)
        .attr("dx", function(d) {return d.size + 1;})
        .style("font", function(d) {return (d.size / 2 + 7) + "px serif"})
        .style("opacity", 0.8);
    }

    function pair(array) {
        return array.slice(1).map(function (b, i) {
            return [array[i], b];
        });
    }

    var legendScale = d3.scaleLinear()
        .domain(options.legendDomain.reverse())
        .range([0, 200])
        .nice();

    var axis = d3.axisRight(legendScale)
        .tickSize(10)
        .tickFormat(d3.format("+.1f"));

    var legend = svg.append("g")
        .attr("class", "legend")
        .attr("transform", "translate(" + (width - 80) + ", 50)");

    legend.selectAll("rect")
        .data(pair(legendScale.ticks(10)))
        .enter().append("rect")
        .attr("width", 8)
        .attr("y", function(d) { return legendScale(d[0]); })
        .attr("height", function(d) { return legendScale(d[1]) - legendScale(d[0]); })
        .style("fill", function(d) { return color(d[1]); });

    legend.call(axis)
        .append("text")
        .attr("style", "fill:black")
        .attr("transform", "translate(0, -10)")
        .text(options.legendTitle);

    var title = svg.append("g")
        .attr("class", "title")
        .attr("transform", "translate(" + (width / 2) + ", 30)")
        .append("text")
        .style("font", "20px")
        .style("fill", "black")
        .attr("text-anchor", "middle")
        .attr("font-weight", "bold")
        .text(options.title);

  }
});
