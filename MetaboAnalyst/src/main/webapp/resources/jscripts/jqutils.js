function getTickPositions() {
    var minX = document.getElementById("minX").value;
    var maxX = document.getElementById("maxX").value;
    var xticks = [];
    while (minX <= maxX) {
        xticks.push(minX++);
    }
    return xticks;
}

