const URL = document.getElementById("mydir").value;

const margin = { top: 10, right: 10, bottom: 10, left: 10 },
    width = 480 - margin.left - margin.right,
    height = 400 - margin.top - margin.bottom;
let mx, my, mouseX, mouseY;

//Follow the example width and height must be divided by 2
const origin = [width / 2, height / 2];
const startAngle = Math.PI / 4;
const scale = 20;

//Define prototype for scale
const scale3d = d3
    ._3d()
    .shape("LINE_STRIP")
    .origin(origin)
    .rotateY(startAngle)
    .rotateX(-startAngle)
    .scale(scale);

//Define prototype for points
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

//Getter functions for plotting points
function posPointX(d) {
    return d.projected.x;
}

function posPointY(d) {
    return d.projected.y;
}

d3.json("/MetaboAnalyst" + URL + "/pca_score3d_0_.json", function (data) {
    //Define svg
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
        .call(
            d3
                .drag()
                .on("drag", dragged)
                .on("start", dragStart)
                .on("end", dragEnd)
        )
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    /**
     * @define A function to 3D-plot
     * @param {Array} data
     * @param {Number} tt duration for transition
     */
    const processData = (data, tt) => {
        //Draw points
        const pointsOnSVG = svg.selectAll("circle").data(data[0]);

        pointsOnSVG
            .enter()
            .append("circle")
            .merge(pointsOnSVG)
            .attr("class", "_3d")
            .attr("opacity", 0)
            .attr("cx", posPointX)
            .attr("cy", posPointY)
            .transition()
            .duration(tt)
            .attr("r", 3)
            .attr("fill", function (d) {
                return d.color;
            })
            .attr("id", function (d, i) {
                return `point-${i}`;
            })
            .attr("opacity", 1)
            .attr("x", (d) => d.x)
            .attr("y", (d) => d.y)
            .attr("cx", posPointX)
            .attr("cy", posPointY);

        pointsOnSVG.exit().remove();

        //Draw x-Axis
        const xScale = svg.selectAll("path.xScale").data(data[1]);

        xScale
            .enter()
            .append("path")
            .attr("class", "_3d xScale")
            .merge(xScale)
            .attr("stroke", "black")
            .attr("stroke-width", 0.5)
            .attr("d", scale3d.draw);

        xScale.exit().remove();

        /* ----------- x-Scale Text ----------- */
        const xText = svg.selectAll("text.xText").data(data[1][0]);

        xText
            .enter()
            .append("text")
            .attr("class", "_3d xText")
            .attr("dx", ".3em")
            .merge(xText)
            .each(function (d) {
                d.centroid = { x: d.rotated.x, y: d.rotated.y, z: d.rotated.z };
            })
            .attr("x", posPointX)
            .attr("y", posPointY)
            .text(function (d) {
                return d[0].toFixed(2);
            })
            .style("transform", "translate(-50px, 0)");

        xText.exit().remove();

        //Draw y-Axis
        const yScale = svg.selectAll("path.yScale").data(data[2]);

        yScale
            .enter()
            .append("path")
            .attr("class", "_3d yScale")
            .merge(yScale)
            .attr("stroke", "black")
            .attr("stroke-width", 0.5)
            .attr("d", scale3d.draw);

        yScale.exit().remove();

        /* ----------- y-Scale Text ----------- */
        const yText = svg.selectAll("text.yText").data(data[2][0]);

        yText
            .enter()
            .append("text")
            .attr("class", "_3d yText")
            .attr("dx", ".3em")
            .merge(yText)
            .each(function (d) {
                d.centroid = { x: d.rotated.x, y: d.rotated.y, z: d.rotated.z };
            })
            .attr("x", function (d) {
                return d.projected.x;
            })
            .attr("y", function (d) {
                return d.projected.y;
            })
            .text(function (d) {
                return d[1].toFixed(2);
            });

        yText.exit().remove();

        //Draw z-Axis
        const zScale = svg.selectAll("path.zScale").data(data[3]);

        zScale
            .enter()
            .append("path")
            .attr("class", "_3d zScale")
            .merge(zScale)
            .attr("stroke", "black")
            .attr("stroke-width", 0.5)
            .attr("d", scale3d.draw);

        zScale.exit().remove();

        /* ----------- y-Scale Text ----------- */
        const zText = svg.selectAll("text.zText").data(data[3][0]);

        zText
            .enter()
            .append("text")
            .attr("class", "_3d zText")
            .attr("dx", ".3em")
            .merge(zText)
            .each(function (d) {
                d.centroid = { x: d.rotated.x, y: d.rotated.y, z: d.rotated.z };
            })
            .attr("x", function (d) {
                return d.projected.x;
            })
            .attr("y", function (d) {
                return d.projected.y;
            })
            .text(function (d) {
                return d[2].toFixed(2);
            })
            .style("transform", "translate(0, -20px)");

        zText.exit().remove();

        d3.selectAll("._3d").sort(d3._3d().sort);
    };

    //Extract data
    const title = data.main;
    const coords = data.points.coords;
    const colors = data.points.cols;
    const names = data.points.names;
    const categories = data.points.categories;
    const points = [];

    // Create an array of objects (points)
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

    //Prepare datas for drawing
    //Points
    const data_for_drawings = [];
    data_for_drawings.push(point3d(points));

    //Axes
    //Prepare min max first
    const [minX, maxX] = d3.extent(points.map((p) => p.x));
    const [minY, maxY] = d3.extent(points.map((p) => p.y));
    const [minZ, maxZ] = d3.extent(points.map((p) => p.z));

    const generateLine = (min, max, order) => {
        const line = [];
        const minOnAxis = Math.floor(min);
        const maxOnAxis = Math.ceil(max);
        const steps = (Math.abs(min) + Math.abs(max)) / 5;
        for (let i = minOnAxis; i <= maxOnAxis; i = i + steps) {
            if (order === 0) {
                //X line
                line.push([i, minY, minZ]);
                continue;
            }
            if (order === 1) {
                //Y Line
                line.push([minX, i, minZ]);
                continue;
            }
            // Z line
            line.push([minX, minY, i]);
        }
        return line;
    };

    //X-axis
    const xLine = generateLine(minX, maxX, 0);
    data_for_drawings.push(scale3d([xLine]));

    // Y-axis
    const yLine = generateLine(minY, maxY, 1);
    data_for_drawings.push(scale3d([yLine]));

    // Z-axis
    const zLine = generateLine(minZ, maxZ, 2);
    data_for_drawings.push(scale3d([zLine]));

    //Draw
    processData(data_for_drawings, 200);

    //Drag handlers
    function dragStart() {
        mx = d3.event.x;
        my = d3.event.y;
    }

    function dragged() {
        mouseX = mouseX || 0;
        mouseY = mouseY || 0;
        beta = ((d3.event.x - mx + mouseX) * Math.PI) / 230;
        alpha = (((d3.event.y - my + mouseY) * Math.PI) / 230) * -1;
        var data = [
            // grid3d.rotateY(beta + startAngle).rotateX(alpha - startAngle)(xGrid),
            point3d.rotateY(beta + startAngle).rotateX(alpha - startAngle)(
                points
            ),
            scale3d.rotateY(beta + startAngle).rotateX(alpha - startAngle)([
                xLine,
            ]),
            scale3d.rotateY(beta + startAngle).rotateX(alpha - startAngle)([
                yLine,
            ]),
            scale3d.rotateY(beta + startAngle).rotateX(alpha - startAngle)([
                zLine,
            ]),
        ];
        processData(data, 0);
    }

    function dragEnd() {
        mouseX = d3.event.x - mx + mouseX;
        mouseY = d3.event.y - my + mouseY;
    }
});
