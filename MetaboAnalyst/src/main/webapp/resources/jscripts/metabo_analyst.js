
var sigInst, settings, domainURL, vizMode;

function showSigmaNetwork() {
    vizMode = "other";
    domainURL = window.parent.document.getElementById("myurl").value;
    sigInst = new sigma({
        renderers: [
            {
                container: document.getElementById('sigma-example'),
                type: 'canvas' // sigma.renderers.canvas works as well
            }
        ],
        settings: {
            defaultLabelColor: '#000',
            defaultLabelSize: 12,
            defaultLabelBGColor: '#fff',
            defaultLabelHoverColor: '#000',
            labelThreshold: 4,
            defaultEdgeLabelColor: '#fff',
            defaultEdgeLabelSize: 10,
            defaultEdgeLabelBGColor: '#fff',
            defaultEdgeLabelHoverColor: '#fff',
            edgeLabelThreshold: 4,
            labelAlignment: 'right',
            minEdgeSize: 1,
            maxEdgeSize: 2,
            enableEdgeHovering: true,
            edgeHoverColor: 'edge',
            minNodeSize: 2,
            maxNodeSize: 10,
            sideMargin: 12,
            borderSize: 2,
            doubleClickEnabled: false,
            nodeBorderColor: "default",
            defaultNodeBorderColor: '#fff'

        }
    });
    sigma.plugins.dragNodes(sigInst, sigInst.renderers[0]);

    sigInst.bind('doubleClickNode', function (e) {
        $.ajax({
            dataType: "html",
            type: "POST",
            url: domainURL + 'faces/AjaxCall',
            data: {function: 'prepareMsetView', id: e.data.node.id},
            async: false,
            cache: false,
            success: function () {
                updateMset();
                PF('msetDialog').show();
            },
            error: function () {
                alert('Error', 'Failed to process the request!', 'error');
            }
        });
    });

    var config = {
        nodeMargin: 8.0,
        scaleNodes: 1.3
    };

// Configure the algorithm
    sigInst.configNoverlap(config);
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value + "/msea_network.json",
            function (data) {
                for (var i = 0; i < data.nodes.length; i++) {
                    var nd = data.nodes[i];
                    nd.x = parseInt(nd.x);
                    nd.y = parseInt(nd.y);
                    sigInst.graph.addNode(nd);
                }
                for (var j = 0; j < data.edges.length; j++) {
                    var eg = data.edges[j];
                    eg.color = "grey";
                    sigInst.graph.addEdge(eg);
                }
                sigInst.refresh();
                sigInst.startNoverlap();//adjust time for laying out, 
            });
}

function export2SVG() {
    sigInst.toSVG({download: true, filename: 'msea_network.svg', labels: true, background: "#ffffff"});
}
