const URL = document.getElementById("mydir").value;

d3.json("/MetaboAnalyst" + URL + "/pca_score3d_0_.json", function (data) {
    //Extract data
    const title = data.main;
    const coords = data.points.coords;
    const color = data.points.cols;
    const names = data.points.names;
    const axesName = data.axis;
    const categories = data.points.categories;
    //Prepare hover text
    const text = names.map(
        (e, i) => `Name: ${e}<br>Category: ${categories[i]}`
    );

    //Center everything inside div
    const container = d3
        .select("#my_dataviz")
        .style("display", "flex")
        .style("flex-direction", "column")
        .style("align-items", "center")
        .style("border", "1px solid black")
        .style("width", "480px")
        .style("margin", "auto")
        .style("padding", "20px 30px");

    //Title
    container
        .append("div")
        .attr("id", "title")
        .style("order", "-1")
        .append("b")
        .html(title)
        .style("font-size", "18px");

    //3D plot using plotly
    //Prepare data
    var trace = {
        x: coords.PC1,
        y: coords.PC2,
        z: coords.PC3,
        mode: "markers",
        hovertemplate:
            "X: %{x:.2f}" +
            "<br>Y: %{y:.2f}<br>" +
            "Z: %{z:.2f}<br>" +
            "%{text}",
        text,
        marker: {
            size: 5,
            line: {
                color: "rgba(217, 217, 217, 0.14)",
                width: 0.5,
            },
            color,
            opacity: 0.8,
        },
        type: "scatter3d",
    };

    var data = [trace];
    //Customize graph
    var layout = {
        scene: {
            xaxis: { title: axesName[0] },
            yaxis: { title: axesName[1] },
            zaxis: { title: axesName[2] },
        },
        margin: {
            l: 10,
            r: 10,
            b: 10,
            t: 10,
            pad: 10,
        },
    };

    Plotly.newPlot("my_dataviz", data, layout);
});
