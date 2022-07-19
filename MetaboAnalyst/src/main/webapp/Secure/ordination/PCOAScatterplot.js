/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/JSP_Servlet/JavaScript.js to edit this template
 */
var margin = { top: 30, right: 160, bottom: 50, left: 60 },
    width = 480 - margin.left - margin.right,
    height = 400 - margin.top - margin.bottom;

// append the svg object to the body of the pagevar svg = d3.select("#my_dataviz")
var svg = d3
    .select("#my_dataviz")
    .style("margin", "auto")
    .style("display", "flex")
    .style("flex-direction", "column")
    .style("align-items", "center")
    .style("justify-content", "center")
    .append("svg")
    .style("order", 2)
    .style("border", "solid")
    .style("border-width", "1px")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

var input = d3.select("#my_dataviz").append("input").attr("type", "number");

//Read the data
d3.json("Scatterplot.json", function (data) {
    //Extract data
    const title = data.graphName;
    const xLabel = data.xAxisLabel;
    const yLabel = data.yAxisLabel;
    const values = Object.values(data.data);

    //Set default value for input
    input.attr(
        "value",
        d3.max(values, (d) => d.x)
    );

    //Define Xaxis
    var x = d3
        .scaleLinear()
        .domain(d3.extent(values, (d) => d.x))
        .range([0, width]);
    var xAxis = svg
        .append("g")
        .attr("transform", "translate(0," + height + ")")
        .call(d3.axisBottom(x));

    svg.append("text")
        .text(xLabel)
        .attr("x", width / 3)
        .attr("y", height + 30)
        .style("font-size", "15px");

    // Add Y axis
    var y = d3
        .scaleLinear()
        .domain(d3.extent(values, (d) => d.y))
        .range([height, 0]);
    svg.append("g").call(d3.axisLeft(y));

    svg.append("text")
        .text(yLabel)
        .attr("y", -30)
        .attr("x", -(height / 3))
        .style("text-anchor", "end")
        .attr("transform", "rotate(-90)")
        .style("font-size", "15px");

    // Color scale: give me a specie name, I return a color
    var color = d3
        .scaleOrdinal()
        .domain(values.map((value) => value.name))
        .range(["#440154ff", "#21908dff", "#fde725ff"]);

    // Add dots
    svg.append("g")
        .selectAll(".mypoint")
        .data(values)
        .enter()
        .append("circle")
        .attr("id", "mypoint")
        .attr("x-value", function (d) {
            return d.x;
        })
        .attr("y-value", function (d) {
            return d.y;
        })
        .attr("cx", function (d) {
            return x(d.x);
        })
        .attr("cy", function (d) {
            return y(d.y);
        })
        .attr("r", 5)
        .style("fill", function (d) {
            return color(d.name);
        })
        .on("mouseover", function (d) {
            tooltip
                .style("opacity", 0.8)
                .html(d.name + " (" + d.x + "," + d.y + ")")
                .style("left", event.pageX + 5 + "px")
                .style("top", event.pageY + "px");
        })
        .on("mouseout", function (d) {
            tooltip.transition(200).style("opacity", 0);
        });

    var tooltip = d3
        .select("body")
        .append("div")
        .attr("id", "tooltip")
        .style("opacity", 0)
        .style("position", "absolute")
        .attr("class", "tooltip")
        .style("background-color", "white")
        .style("border", "solid")
        .style("border-width", "1px")
        .style("border-radius", "5px")
        .style("padding", "10px");

    //Legends
    var legendContainer = svg.append("g").attr("id", "legend");

    var legend = legendContainer
        .selectAll("#legend")
        .data(color.domain())
        .enter()
        .append("g")
        .attr("class", "legend-lable")
        .attr("transform", function (d, i) {
            return "translate(0," + (height / 2 - i * 20) + ")";
        });

    legend
        .append("circle")
        .attr("cx", width + margin.right - 120)
        .attr("r", 5)
        .style("fill", color);

    legend
        .append("text")
        .attr("x", width + margin.right - 110)
        .attr("y", 1)
        .attr("dy", ".35em")
        .text(function (d) {
            return d;
        });

    //        create title
    var titleDiv = d3
        .select("#my_dataviz")
        .append("div")
        .html(title)
        .style("text-align", "center")
        .style("order", 1)
        .style("font-size", "18px")
        .style("margin-bottom", "1rem");

    // Update Xaxis
    function updatePlot() {
        // Get the value of the button
        xlim = this.value;

        var oldDomain = x.domain();
        // Update X axis
        x.domain([oldDomain[0], xlim]);
        xAxis.transition().duration(1000).call(d3.axisBottom(x));

        // Update chart
        svg.selectAll("circle")
            .data(values)
            .transition()
            .duration(1000)
            .attr("cx", function (d) {
                return x(d.x);
            })
            .attr("cy", function (d) {
                return y(d.y);
            });
    }

    input.on("input", updatePlot);
});
