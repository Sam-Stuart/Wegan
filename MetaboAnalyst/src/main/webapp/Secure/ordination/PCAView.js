const URL = document.getElementById("mydir").value;

var margin = { top: 30, right: 30, bottom: 50, left: 60 },
    width = 480 - margin.left - margin.right,
    height = 400 - margin.top - margin.bottom;

const svg = d3
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
    .attr("width", width)
    .attr("height", height)
    .append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

//Follow the example width and height must be divided by 2
const origin = [width / 2, height / 2];
const startAngle = Math.PI / 4;
const scale = 20;

const point3d = d3
    ._3d()
    .x(function (d) {
        return d.x;
    })
    .y(function (d) {
        return d.y;
    })
    .z(function (d) {
        return d.z;
    })
    .origin(origin)
    .rotateY(startAngle)
    .rotateX(-startAngle)
    .scale(scale);

d3.json("/MetaboAnalyst" + URL + "/pca_score3d_0_.json", function (data) {
    const title = data.main;
    const coords = data.points.coords;
    const colors = data.points.cols;
    const names = data.points.names;
    const categories = data.points.categories;
    const points = [];

    names.forEach((name, index) => {
        points.push({
            name,
            x: coords.PC1[index],
            y: coords.PC2[index],
            z: coords.PC3[index],
            color: colors[index],
            category: categories[index],
        });
    });

    function posPointX(d) {
        return d.projected.x;
    }

    function posPointY(d) {
        return d.projected.y;
    }

    const pointsOnSVG = svg.selectAll("circle").data(point3d(points));

    pointsOnSVG
        .enter()
        .append("circle")
        .merge(pointsOnSVG)
        .attr("class", "_3d")
        .attr("opacity", 0)
        .attr("cx", posPointX)
        .attr("cy", posPointY)
        .transition()
        .duration(200)
        .attr("r", 3)
        .attr("fill", function (d) {
            return d.color;
        })
        .attr("opacity", 1)
        .attr("cx", posPointX)
        .attr("cy", posPointY);

    pointsOnSVG.exit().remove();

    console.log(point3d(points));
});
