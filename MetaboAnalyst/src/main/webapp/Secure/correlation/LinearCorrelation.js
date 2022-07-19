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
    .style("order", 3)
    .style("border", "solid")
    .style("border-width", "1px")
    .attr("width", width + margin.left + margin.right)
    .attr("height", height + margin.top + margin.bottom)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

function calLinearRegression(data) {
    var n = data.length;
    var x_mean = 0;
    var y_mean = 0;
    var term1 = 0;
    var term2 = 0;

    data.forEach((d) => {
        x_mean += d.x;
        y_mean += d.y;
    });

    // calculate mean x and y
    x_mean /= n;
    y_mean /= n;

    // calculate coefficients
    var xr = 0;
    var yr = 0;
    data.forEach((d) => {
        xr = d.x - x_mean;
        yr = d.y - y_mean;
        term1 += xr * yr;
        term2 += xr * xr;
    });

    var b1 = term1 / term2;
    var b0 = y_mean - b1 * x_mean;
    // perform regression

    // fit line using coeffs
    data.forEach((d) => {
        d.yhat = b0 + d.x * b1;
    });
}

//Read the data
d3.json("LinearCorrelation.json", function (data) {
    //Extract data
    const title = data.graphName;
    const xLabel = data.xAxisLabel;
    const yLabel = data.yAxisLabel;
    const values = Object.values(data.data);
    const equation = `${yLabel} = ${data.slope} * ${xLabel} + ${data["y-intercept"]}`;
    const Rsquare = data.RSquare;
    const RsquareAdjusted = data.RSquareAdjusted;

    calLinearRegression(values);

    //Convert to number type. Make sure that they are numbers
    values.forEach((d) => {
        d.x = +d.x;
        d.y = +d.y;
        d.yhat = +d.yhat;
    });

    //Define Xaxis
    var x = d3
        .scaleLinear()
        .domain(d3.extent(values, (d) => d.x))
        .range([0, width]);
    svg.append("g")
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

    //Create line
    var line = d3
        .line()
        .x(function (d) {
            return x(d.x);
        })
        .y(function (d) {
            return y(d.yhat);
        });

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

    // Appennd confidence interval
    svg.append("path")
        .datum(values)
        .attr("fill", "#cce5df")
        .attr("stroke", "none")
        .attr(
            "d",
            d3
                .area()
                .x(function (d) {
                    return x(d.x);
                })
                .y0(function (d) {
                    return y(0.1);
                })
                .y1(function (d) {
                    return y(0.3);
                })
        );

    // Append line
    svg.append("path")
        .datum(values)
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

    //Equation
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

    // Create checkboxes
    var options = ["Show R square", "Show R square adjusted"];
    var checkBoxContainer = d3
        .select("#my_dataviz")
        .append("div")
        .style("display", "inherit")
        .style("order", 2)
        .selectAll(".checkbox")
        .data(options)
        .enter()
        .append("div")
        .classed("checkbox", true);

    checkBoxContainer
        .append("input")
        .attr("type", "checkbox")
        .attr("id", (d, i) => `checkbox${i + 1}`)
        .attr("value", (d) => d)
        .on("change", updateEquation);

    checkBoxContainer
        .append("label")
        .text((d) => d)
        .attr("for", (d, i) => `checkbox${i + 1}`);

    function updateEquation() {
        if (d3.select("#checkbox1").property("checked"))
            RsquareLabel.transition(200).style("opacity", "1");
        else RsquareLabel.transition(200).style("opacity", "0");
        if (d3.select("#checkbox2").property("checked"))
            RsquareAdjustedLabel.transition(200).style("opacity", "1");
        else RsquareAdjustedLabel.transition(200).style("opacity", "0");
    }
});
