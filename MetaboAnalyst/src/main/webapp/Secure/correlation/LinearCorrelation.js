/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/JSP_Servlet/JavaScript.js to edit this template
 */
var margin = { top: 30, right: 30, bottom: 50, left: 60 },
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
    .style("order", 3)
    .style("border", "solid")
    .style("border-width", "1px")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

//Read the data
d3.json("LinearCorrelation.json", function (data) {
    //Extract data
    const title = data.main;
    const [xLabel, yLabel] = data.axis;
    const point_coords = data.points.coords;
    const line_coords = data.lines.coords;
    const CI = data.lines.ci;
    const equation = `${yLabel} = ${data.slope} * ${xLabel} + ${data.yint}`;
    const Rsquare = data.r_sq;
    const RsquareAdjusted = data.r_sq_adj;

    //This will be used to draw the line and the confidence interval
    const line_values = [];
    line_coords.x.forEach((e, i) => {
        const line_coord = {
            x: e,
            y: line_coords.y[i],
            CI_down: CI.CI_down[i],
            CI_up: CI.CI_up[i],
        };
        line_values.push(line_coord);
    });

    //Create an array of objects of points
    const values = [];
    point_coords.x.forEach((e, i) => {
        const point = { x: e, y: point_coords.y[i] };
        values.push(point);
    });

    //Convert to number type. Make sure that they are numbers
    values.forEach((d) => {
        d.x = +d.x;
        d.y = +d.y;
        d.yhat = +d.yhat;
    });

    //Define Xaxis, add xAxis and xAxis label
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

    // Add Y axis, add yAxis and yAxis label
    var y = d3
        .scaleLinear()
        .domain(d3.extent(values, (d) => d.y))
        .range([height, 0]);
    var yAxis = svg.append("g").call(d3.axisLeft(y));

    svg.append("text")
        .text(yLabel)
        .attr("y", -30)
        .attr("x", -(height / 3))
        .style("text-anchor", "end")
        .attr("transform", "rotate(-90)")
        .style("font-size", "15px");

    //Create line
    var line = d3
        .line()
        .x(function (d) {
            return x(d.x);
        })
        .y(function (d) {
            return y(d.y);
        });

    // Add a clipPath: everything out of this area won't be drawn.
    var clip = svg
        .append("defs")
        .append("SVG:clipPath")
        .attr("id", "clip")
        .append("SVG:rect")
        .attr("width", width)
        .attr("height", height)
        .attr("x", 0)
        .attr("y", 0);

    // Create the scatter variable: where both the circles and the brush take place
    var scatter = svg.append("g").attr("clip-path", "url(#clip)");

    // Set the zoom and Pan features: how much you can zoom, on which part, and what to do when there is a zoom
    var zoom = d3
        .zoom()
        .scaleExtent([0.5, 20]) // This control how much you can unzoom (x0.5) and zoom (x20)
        .extent([
            [0, 0],
            [width, height],
        ])
        .on("zoom", updateChart);

    // This add an invisible rect on top of the chart area. This rect can recover pointer events: necessary to understand when the user zoom
    scatter
        .append("rect")
        .attr("width", width)
        .attr("height", height)
        .style("fill", "none")
        .style("pointer-events", "all")
        .lower();

    scatter.call(zoom);
    // now the user can zoom and it will trigger the function called updateChart

    // A function that updates the chart when the user zoom and thus new boundaries are available
    function updateChart() {
        // recover the new scale
        var newX = d3.event.transform.rescaleX(x);
        var newY = d3.event.transform.rescaleY(y);

        // update axes with these new boundaries
        xAxis.call(d3.axisBottom(newX));
        yAxis.call(d3.axisLeft(newY));

        // update circle position
        scatter
            .selectAll("circle")
            .attr("cx", function (d) {
                return newX(d.x);
            })
            .attr("cy", function (d) {
                return newY(d.y);
            });

        scatter.select(".line").attr(
            "d",
            d3
                .line()
                .x((d) => newX(d.x))
                .y((d) => newY(d.y))
        );

        scatter.select(".confidence_interval").attr(
            "d",
            d3
                .area()
                .x(function (d) {
                    return newX(d.x);
                })
                .y0(function (d) {
                    return newY(d.CI_up);
                })
                .y1(function (d) {
                    return newY(d.CI_down);
                })
        );
    }

    // Appennd confidence interval
    scatter
        .append("path")
        .classed("confidence_interval", true)
        .datum(line_values)
        .attr("fill", "#CBCBCB")
        .attr("stroke", "none")
        .attr(
            "d",
            d3
                .area()
                .x(function (d) {
                    return x(d.x);
                })
                .y0(function (d) {
                    return y(d.CI_up);
                })
                .y1(function (d) {
                    return y(d.CI_down);
                })
        );

    // Add dots
    scatter
        .selectAll("circle")
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
        .attr("r", 3)
        .style("fill", "black")
        .on("mouseover", function (d) {
            tooltip
                .style("opacity", 0.8)
                .html("(" + d.x + "," + d.y + ")")
                .style("left", event.pageX + 5 + "px")
                .style("top", event.pageY + "px");
        })
        .on("mouseout", function (d) {
            tooltip.transition(200).style("opacity", 0);
        });

    //Tool tip
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

    // Append line
    scatter
        .append("path")
        .datum(line_values)
        .attr("class", "line")
        .attr("d", line)
        .style("stroke", "black")
        .style("stroke-width", "3")
        .style("fill", "none")
        .on("mouseover", function (d) {
            tooltip
                .style("opacity", 0.8)
                .html(equation)
                .style("left", event.pageX + 5 + "px")
                .style("top", event.pageY + "px");
        })
        .on("mouseout", function (d) {
            tooltip.transition(200).style("opacity", 0);
        });

    //        create title
    d3.select("#my_dataviz")
        .append("div")
        .html(title)
        .style("text-align", "center")
        .style("order", 1)
        .style("font-size", "18px")
        .style("margin-bottom", "1rem");

    // Equation;
    var equationGroup = svg.append("g").classed("equation", true);
    var equationLabel = equationGroup.append("text").text(equation);
    var RsquareLabel = equationGroup
        .append("text")
        .text(`R Square = ${Rsquare}`)
        .attr("y", 15)
        .style("opacity", 0)
        .attr("x", width / 2);
    var RsquareAdjustedLabel = equationGroup
        .append("text")
        .text(`R Square Adjusted = ${RsquareAdjusted}`)
        .attr("y", 30)
        .style("opacity", 0)
        .attr("x", width / 2);

    // function updateEquation() {
    //     if (d3.select("#checkbox1").property("checked"))
    //         RsquareLabel.transition(200).style("opacity", "1");
    //     else RsquareLabel.transition(200).style("opacity", "0");
    //     if (d3.select("#checkbox2").property("checked"))
    //         RsquareAdjustedLabel.transition(200).style("opacity", "1");
    //     else RsquareAdjustedLabel.transition(200).style("opacity", "0");
    // }
});
