HTMLWidgets.widget({
  name: "forceGraph",
  type: "output",

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

    simulation
    .force("center", d3.forceCenter(width / 2, height / 2))

  },

  renderValue: function(el, x, simulation) {
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
        .attr("stroke", "grey")
        .attr("stroke-width", function(d) { return d.weight });

    var node = svg.append("g")
        .attr("class", "nodes")
        .selectAll("circle")
        .data(nodes)
        .enter().append("circle")
        .attr("r", function(d) {return d.size})
        .attr("fill", function(d) { return color(d.color)})
        .on("click", clicked)
        .call(d3.drag()
            .on("start", dragstarted)
            .on("drag", dragged)
            .on("end", dragended));

    var label = svg.append("g")
        .attr("class", "labels")
        .selectAll("text")
        .data(nodes)
        .enter().append("text")
        .attr("fill", function(d) { return color(d.color)})
        .attr("x", function(d) {return d.size;})
        .attr("y", function(d) {return d.size / 2;})
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

    simulation.alpha(1).restart();

    function ticked() {
        link
            .attr("x1", function(d) { return d.source.x; })
            .attr("y1", function(d) { return d.source.y; })
            .attr("x2", function(d) { return d.target.x; })
            .attr("y2", function(d) { return d.target.y; });
        node
            .attr("cx", function(d) { return d.x; })
            .attr("cy", function(d) { return d.y; });

        label
            .attr("transform", function (d) {
                return "translate(" + d.x + "," + d.y + ")"
            })
    }

    function clicked(d) {
        d.fixed = !d.fixed;
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


    var legendScale = d3.scaleLinear()
        .domain(options.colorDomain)
        .range([0, 100, 200]);

    var axis = d3.axisRight(d3.scaleLinear()
        .domain(options.colorDomain)
        .range([0, 100, 200]))
        .tickSize(10)
        .tickFormat(d3.format("+.1f"));

    var legend = svg.append("g")
        .attr("class", "legend")
        .attr("transform", "translate(" + (width - 80) + ", 50)");

    function pair(array) {
        return array.slice(1).map(function (b, i) {
            return [array[i], b];
        });
    }

    legend.selectAll("rect")
        .data(pair(legendScale.ticks(10)))
        .enter().append("rect")
        .attr("width", 8)
        .attr("y", function(d) { return legendScale(d[0]); })
        .attr("height", function(d) { return legendScale(d[1]) - legendScale(d[0]); })
        .style("fill", function(d) { return color(d[0]); });

    legend.call(axis)
        .append("text")
        .attr("style", "fill:black")
        .attr("transform", "translate(0, -10)")
        .text(options.legendTitle);

  }
});
