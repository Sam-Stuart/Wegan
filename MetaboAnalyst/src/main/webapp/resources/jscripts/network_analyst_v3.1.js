/*
 * Javascript functions for networkanalyst
 * 2014-9-21
 * Jeff Xia (jeff.xia@mcgill.ca)
 */

//global vars
var greyColor = '#959595';
var greyColorB = '#959595';
var greyColorW = '#888888';
var highlightColor = '#0080ff';
var backgroundColor = "#222222";
var up_color = "#FF0000";
var white_color = "#ffffff";
var down_color = "#008000"; //#80d0f0
var view_mode = "topo";
var anal_type = "list"; //list, single, meta
var node_rows = [];
var sub_modules = [];
var hidden_mode = false;
var dim_mode = false;
var lasso_active = false;
var highlight_mode = false;
var mouseover_mode = false; // hide remote nodes
var dragListener;
var minEdgeSize = 0.2;
var edgeColoring = false;
var defaultEdgeSize = 0.9;
var current_fun_nodes;
var dependants;
var selectedNodeIDs = []; //for free style drag-and-drop
var usr_dir, org, vismode, dbid; //vismode: ppi, mir, drug; dbid is used for ppi and tf
var sigInst, settings;
var nodeIDsArrPosition = []; //Dicionary with node IDs as keys and array position of node as value
var domainURL;                             // for faster search of node positions.
var cmpdnameOpt = 'show';

sigma.utils.pkg('sigma.canvas.nodes');
sigma.canvas.nodes.def = function (node, context, settings) {
    var prefix = settings('prefix') || '';

    context.fillStyle = node.color || settings('defaultNodeColor');
    context.beginPath();
    context.arc(
            node[prefix + 'x'],
            node[prefix + 'y'],
            node[prefix + 'size'],
            0,
            Math.PI * 2,
            true
            );

    context.closePath();
    context.fill();
    if (node.borderColor) {
        context.lineWidth = 2;
        context.strokeStyle = node.borderColor;
        context.stroke();
    }
};

function initNetwork() {
    initFunctions();
    domainURL = window.parent.document.getElementById("myurl").value;
    //first get user home dir
    var res = $.ajax({
        type: "GET",
        url: domainURL + 'faces/AjaxCall?function=getjobinfo' + "&viewmode=net&ignoreMe=" + new Date().getTime(),
        async: false
    }).responseText;
    //    alert(res);
    res =res.split("||");
    usr_dir = res[0];
    org = res[1];
    vismode = res[2];
    dbid = res[3];
    setupNetworkOpts(res.splice(4));
    setupNetwork('networkanalyst_0.json');
}

function setupNetworkOpts(nvec) {
    //need to set up options for functional annotation
    var nOpts = $('#networkOpt');
    nOpts.empty(); //need to clear all previous options
    for (var i = 0; i < nvec.length; i++) {
        var net_nm = nvec[i];
        nOpts.append('<option value="' + net_nm + '">' + net_nm + '</option>');
    }
}

function updateNetworkOpts(netNm) {
    //need to set up options for functional annotation
    var res = $.ajax({
        type: "GET",
        url: domainURL + 'faces/AjaxCall?function=getnetinfo' + "&ignoreMe=" + new Date().getTime(),
        async: false
    }).responseText.split("||");
    setupNetworkOpts(res);
    $('#networkOpt').val(netNm);
}

//initiate variables and attach functions
function initFunctions() {
    vizMode = "other";
    //dynamic update centerpane width based on user browser
    $('#network-parent').css("width", $(window).width() - 640);
    $('#network-parent').css("height", $(window).height() - 40);

    $('#ss').slider({
        tipFormatter: function (value) {
            return value + '%';
        }
    });

    // Instanciate sigma.js and customize rendering :
    sigInst = new sigma({
        renderers: [
            {
                container: document.getElementById('networkview'),
                type: 'canvas' // sigma.renderers.canvas works as well
            }
        ],
        settings: {
            defaultLabelColor: '#fff',
            defaultLabelSize: 11,
            defaultLabelBGColor: '#fff',
            defaultLabelHoverColor: '#000',
            labelThreshold: 6,
            defaultEdgeColor: 'default',
            doubleClickEnabled: false,
            minNodeSize: 1,
            maxNodeSize: 10,
            sideMargin: 1,
            minEdgeSize: defaultEdgeSize,
            maxEdgeSize: defaultEdgeSize,
            defaultNodeBorder: 0,
            enableEdgeHovering: true,
//            edgeHoverColor: 'white',
//            defaultEdgeHoverColor: greyColor,
            edgeHoverSizeRatio: 0.5
        }
    });

    // Initialize the activeState plugin:
    var activeState = sigma.plugins.activeState(sigInst);

    // Initialize the dragNodes plugin:
    var dragListener = new sigma.plugins.dragNodes(sigInst, sigInst.renderers[0], activeState);

    dragListener.bind('startdrag', function (event) {
        current_node = event.data.node;
        current_node.start_x = current_node.x;
        current_node.start_y = current_node.y;
        dependants = [];
        if ($('#selectOpt').val() === "function") {
            if (!current_fun_nodes) {
                $.messager.alert('', "Click a row in <b>Function Explorer</b> to select an enriched function.", 'info');
            } else {
                sigInst.graph.nodes().forEach(function (n) {
                    if (current_fun_nodes.indexOf(n.id) !== -1) {
                        n.start_x = n.x;
                        n.start_y = n.y;
                        dependants.push(n);
                    }
                });
            }
        } else if ($('#selectOpt').val() === "neighbour") {
            var neighbors = {};
            neighbors[current_node.id] = 1;
            sigInst.graph.edges().forEach(function (e) {
                if (current_node.id === e.source || current_node.id === e.target) {
                    neighbors[e.source] = 1;
                    neighbors[e.target] = 1;
                }
            });
            sigInst.graph.nodes().forEach(function (n) {
                if (neighbors[n.id]) {
                    n.start_x = n.x;
                    n.start_y = n.y;
                    dependants.push(n);
                }
            });
        } else if ($('#selectOpt').val() === "highlight") {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.highlight) {
                    n.start_x = n.x;
                    n.start_y = n.y;
                    dependants.push(n);
                }
            });
        }
    });

    dragListener.bind('drag', function (event) {
        var mv_x = current_node.x - current_node.start_x;
        var mv_y = current_node.y - current_node.start_y;
        var g;
        for (var i = 0; i < dependants.length; i++) {
            g = dependants[i];
            g.x = g.start_x + mv_x;
            g.y = g.start_y + mv_y;
        }
        sigInst.refresh();
    });

    dragListener.bind('dragend', function (event) {
        if (lasso_active) {
            resetNodes(selectedNodeIDs);
        }
        sigInst.refresh();
    });

    // Initialize the lasso plugin:
    var lasso = new sigma.plugins.lasso(sigInst, sigInst.renderers[0], {
        'strokeStyle': 'orange',
        'lineWidth': 1,
        'fillWhileDrawing': true,
        'fillStyle': 'rgba(255, 255, 177, 0.4)',
        'cursor': 'crosshair'
    });

    // Listen for selectedNodes event from the lasso instance:
    lasso.bind('selectedNodes', function (event) {
        // set all edges as "inactive" to avoid mixing active nodes and edges:
        activeState.dropEdges();

        // nodes within the lasso area:
        var nodes = event.data;

        // set all nodes as "inactive" if no node is selected:
        if (!nodes.length)
            activeState.dropNodes();

        // add the selected nodes to the "active" nodes:
        activeState.addNodes(nodes.map(function (n) {
            return n.id;
        }));

        //highlight the selected node
        selectedNodeIDs = [];
        for (var j = 0; j < nodes.length; j++) {
            selectedNodeIDs.push(nodes[j].id);
        }

        highlightSelectedNodes(selectedNodeIDs, "Manual Selection");

        setTimeout(function () {
            // disable the lasso tool after a selection:
            lasso.deactivate();
            //de-highlight nodes
            //unHighlightFunEnrichNodes(selectedNodeIDs);
            // refresh the display to see the active nodes:
            sigInst.refresh({skipIdexation: true});
        }, 0);
    });

    sigInst.bind('overNode', function (event) {
        if (hidden_mode) {
            return false;
        }
        if (mouseover_mode) {
            alert("mouse over mode on");
            var node = event.data.node;
            var neighbors = {};
            neighbors[node.id] = 1;
            sigInst.graph.edges().forEach(function (e) {
                if (node.id === e.source || node.id === e.target) {
                    neighbors[e.source] = 1;
                    neighbors[e.target] = 1;
                }
            });
            sigInst.graph.nodes().forEach(function (n) {
                if (!neighbors[n.id]) {
                    n.hidden = true;
                } else {
                    n.hidden = false;
                }
            });
            sigInst.refresh();
        }
    });

    sigInst.bind('outNode', function () {
        if (hidden_mode) {
            return false;
        }
        if (mouseover_mode) {
            sigInst.graph.edges().forEach(function (e) {
                e.hidden = false;
            });
            sigInst.graph.nodes().forEach(function (n) {
                n.hidden = false;
            });
            sigInst.refresh();
        }
    });

    sigInst.bind('clickEdge', function (event) {
        var edge = event.data.edge;
        displayEdgeInfo(edge);
    });

    sigInst.bind('clickNode', function (event) {
        var node = event.data.node;
        displayNodeInfo(node);
    });

    sigInst.bind('doubleClickNode', function (e) {
        highlightSelectedNode(e.data.node.id);
    });

    //add action listeners for navigation buttons
    var moveDelay = 50;
    $('#zoomInBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            ratio: c.ratio / c.settings('zoomingRatio')
        }, {
            duration: 200
        });
    });
    $('#zoomOutBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            ratio: c.ratio * c.settings('zoomingRatio')
        }, {
            duration: 200
        });
    });
    $('#moveUpBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: c.x,
            y: c.y + moveDelay
        },
                {duration: 200});
    });
    $('#moveDownBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: c.x,
            y: c.y - moveDelay
        },
                {duration: 200});
    });
    $('#moveLeftBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: c.x + moveDelay,
            y: c.y
        },
                {duration: 200});
    });
    $('#moveRightBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: c.x - moveDelay,
            y: c.y
        },
                {duration: 200});
    });
    $('#autofitBn').bind('click keypress', function (event) {
        var c = sigInst.camera;
        sigma.misc.animation.camera(c, {
            x: 0,
            y: 0,
            ratio: 1.0
        },
                {duration: 200});
    });
    $('#resetBn').bind('click keypress', function (event) {
        resetNetwork();
        event.stopPropagation();
        return false;
    });

    $('#selectBn').bind('click keypress', function (event) {

        if (lasso_active) {
            lasso_active = false;
            lasso.deactivate();
            activeState.dropNodes(); //back to regular single node state
        } else {
            lasso_active = true;
            lasso.activate();
        }

        event.stopPropagation();
        return false;
    });

    $('#extractBn').bind('click keypress', function (event) {
        extractNetwork();
        event.stopPropagation();
        return false;
    });

    //set up node property table
    $('#dg').datagrid({
        loadFilter: pagerFilter,
        onSortColumn: function (id, order) {
            sortNodeTable(id, order);
        },
        onSelect: function (index, row) {
            searchNetwork(row.ID);
        },
        onUnselect: function (index, row) {
            resetNodes([row.ID]);
            sigInst.refresh();
        },
        onCheckAll: function (rows) {
            var nodeIDs = [];
            for (var i = 0; i < rows.length; i++) {
                nodeIDs.push(rows[i].ID);
            }
            highlightRegularNodes(nodeIDs);
        },
        onUncheckAll: function (rows) {
            var nodeIDs = [];
            for (var i = 0; i < rows.length; i++) {
                nodeIDs.push(rows[i].ID);
            }
            resetNodes(nodeIDs);
            halo_nodes = [];
            sigInst.refresh();
        }
    });

    $('#dg2').datagrid({
        onSelect: function (index, row) {
            var nodeIDs = focus_fun_anot[row.pathname];
            current_fun_nodes = nodeIDs;
            highlightFunEnrichNodes(nodeIDs, row.pathname);
        }
    });

    $("#custom").spectrum({
        color: "#0080ff",
        showInitial: true,
        change: function (color) {
            highlightColor = color.toHexString();
        },
        showInput: true,
        showPalette: true,
        showSelectionPalette: true, // true by default
        palette: [
            ['black', 'white'],
            ['#666666', '#0080ff']
        ]
    });

    $('#highlightOpt').change(function () {
        view_mode = $('#highlightOpt').val();
        resetNetwork();
    });

    $('#queryOpts').change(function () {
        if ($('#queryOpts').val() === "highlight") {
            $('#enrichAlgo').val("hyp");
        }
    });

    $('#enrichAlgo').change(function () {
        if ($('#enrichAlgo').val() === "emp") {
            if ($('#queryOpts').val() === "highlight") {
                $('#enrichAlgo').val("hyp");
                $.messager.alert('', "Error: Empirical sampling is only designed for testing all target genes!", 'error');
            }
        }
    });

    $('#exportOpt').change(function () {
        var type = $('#exportOpt').val();
        if (type === 'NA')
            return;
        if (type === "png") {
            document.getElementById("downloadimage").src = export2Image();
            $("#pngdialog").dialog('open');
        } else if (type === "svg") {
            sigInst.toSVG({download: true, filename: 'network_analyst.svg', labels: false, background: backgroundColor});
        } else if (type === "graphml") {
            doGraphExport(function (result) {
                var fileLnk = $("#fileLnk");
                fileLnk.empty();
                fileLnk.append("Right click the link below, then 'Save Link As ... ' to download the file<br/><br/>");
                fileLnk.append('<strong><a href="' + usr_dir + '/' + result + '" target="_blank"><u>' + result + '</u></a></strong>');
                $.messager.progress('close');
                $("#filedialog").dialog('open');
            }, type);
        } else {
            return;
        }
    });

    $('#layoutOpt').change(function () {
        var type = $('#layoutOpt').val();
        if (type === 'na') {
            return;
        }
        if (type === 'ForceAtlas') {
            $.messager.progress({
                text: 'Processing .....'
            });
            sigInst.startForceAtlas2({gravity: 100, scalingRatio: 70, slowDown: 100, barnesHutOptimize: true, startingIterations: 30, iterationsPerRender: 20});
            //adjust time for laying out, 
            var runtime = 500;
            var node_num = node_rows.length;
            if (node_num > 5000) {
                runtime = 10000;
            } else if (node_num > 1000) {
                runtime = node_num + 1000;
            } 
            setTimeout(function () {
                sigInst.killForceAtlas2();
                $.messager.progress('close');
            }, runtime);
        } else {
            doLayoutUpdate(type, function (result) {
                if (result !== "NA") {
                    updateNetworkLayout(result);
                } else {
                    $.messager.alert('', "Error: failed to update the layout!", 'error');
                }
                $.messager.progress('close');
            });
        }
        //reset to default
        $('#layoutOpt').val("na");
    });

    $('#backgroundOpt').change(function () {
        backgroundColor = $('#backgroundOpt').val();
        if (backgroundColor === 'NA')
            return;
        updateBackground(backgroundColor);
    });

    $('#networkOpt').change(function () {
        var netNm = $('#networkOpt').val();
        if (netNm === 'NA') {
            return;
        } else {
            doNetworkPrep(netNm, function (result) {
                if (result !== "NA") {
                    setupNetwork(result);
                } else {
                    $.messager.alert('', "Error: no network update performed!", 'error');
                }
                $.messager.progress('close');
            });
        }
    });

    $('#selectOpt').change(function () {
        if (lasso_active) {
            lasso_active = false;
            lasso.deactivate();
            activeState.dropNodes(); //back to regular single node state
        }
    });
}

//loadNetwork and NodeTable
function setupNetwork(fileNm) {
    sigInst.graph.clear();
    $.getJSON(usr_dir + '/' + fileNm, function (data) {

        var node_table = [];
        //var stat = "-";
        var node_len = data.nodes.length;
        for (var i = 0; i < node_len; i++) {
            var nd = data.nodes[i];
            nodeIDsArrPosition[nd.id] = i;
            //need to covert the attribute val to numeric, the default is string with quotes
            nd.degree = parseInt(nd.attributes.degree);
            nd.between = parseFloat(nd.attributes.between);
            nd.expr = nd.attributes.expr;
            nd.hmdb = nd.hmdb['HMDB'];
            nd.expr_colorb = nd.attributes.expcolb;
            nd.expr_colorw = nd.attributes.expcolw;
            nd.true_color_b = nd.colorb;
            nd.true_color_w = nd.colorw;
            nd.true_size = nd.size;  //record original
            if (backgroundColor === "#222222") {
                nd.color = nd.colorb;
                nd.expr_color = nd.expr_colorb;
            } else {
                nd.color = nd.colorw;
                nd.expr_color = nd.expr_colorw;
            }

            if (!nd.expr) {
                nd.expr = 0;
            }

            sigInst.graph.addNode(nd);
            node_table.push({
                ID: nd.id,
                Label: nd.label,
                Degree: nd.degree,
                Betweenness: nd.between,
                Status: nd.expr
            });
        }

        //need to set up edge color based on degree or expression values
        var display_mode = 0;

        if (view_mode === "topo") {
            if (backgroundColor === "#222222") {
                display_mode = 0;
            } else {
                display_mode = 1;
            }
        } else {
            if (backgroundColor === "#222222") {
                display_mode = 2;
            } else {
                display_mode = 3;
            }
        }

        for (var j = 0; j < data.edges.length; j++) {
            var myEdge = data.edges[j];
            var sourceNode = sigInst.graph.nodes(myEdge.source);
            var targetNode = sigInst.graph.nodes(myEdge.target);
            if (sourceNode.degree > targetNode.degree) {
                myEdge.true_color_b = sourceNode.true_color_b;
                myEdge.true_color_w = sourceNode.true_color_w;
            } else {
                myEdge.true_color_b = targetNode.true_color_b;
                myEdge.true_color_w = targetNode.true_color_w;
            }

            if (Math.abs(sourceNode.expr) > Math.abs(targetNode.expr)) {
                myEdge.expr_colorb = sourceNode.expr_colorb;
                myEdge.expr_colorw = sourceNode.expr_colorw;
            } else {
                myEdge.expr_colorb = targetNode.expr_colorb;
                myEdge.expr_colorw = targetNode.expr_colorw;
            }

            var myCol;
            switch (display_mode) {
                case 0:
                    myCol = myEdge.true_color_b;
                    break;
                case 1:
                    myCol = myEdge.true_color_w;
                    break;
                case 2:
                    myCol = myEdge.expr_colorb;
                    break;
                default:
                    myCol = myEdge.expr_colorb;
            }
            if(edgeColoring){
                myEdge.color = myCol;
            }else{
                myEdge.color = greyColor;  
            }
            myEdge.true_color = myCol;
            myEdge.type = 'line';
            myEdge.size = 0.3;
            sigInst.graph.addEdge(myEdge);
        }

        node_rows = node_table;
        //init sort the node table based on their betweenness then degree
        node_rows.sort(function (a, b) {
            return b.Degree - a.Degree;
        });

        var dg = $('#dg');
        //clear data and selection
        dg.datagrid('loadData', {
            "total": 0,
            "rows": []
        });
        dg.datagrid('clearSelections');
        dg.datagrid('clearChecked');
        dg.datagrid('loadData', node_rows);

        $("#pathLnks").empty();

        $('#dg2').datagrid('loadData', {
            "total": 0,
            "rows": []
        });

        //need to set up options for functional annotation
        var qOpts = $('#enrichdb');
        
        qOpts.empty(); //need to clear all previous options
        if (org === "hsa" | org === "mmu") {
            if (vismode === "metabo_phenotypes" | vismode === "metabo_metabolites") {
                qOpts.append(
                        '<option value="kegg">KEGG</option>');
            } else {
                qOpts.append(
                        '<option value="kegg">KEGG</option>' +
                        '<option value="reactome">Reactome</option>' +
                        '<option value="bp">GO:BP</option>' +
                        '<option value="mf">GO:MF</option>' +
                        '<option value="cc">GO:CC</option>' +
                        '<option value="motif">Motif</option>');
                
            }
        } else if (org === "cel" | org === "dme") {
            qOpts.append(
                    '<option value="kegg">KEGG</option>' +
                    '<option value="reactome">Reactome</option>' +
                    '<option value="bp">GO:BP</option>' +
                    '<option value="mf">GO:MF</option>' +
                    '<option value="cc">GO:CC</option>');
        } else if(org === "gga" |org === "bta"|org === "dre"|org === "rno") {
            qOpts.append('<option value="bp">GO:BP</option>' +
                    '<option value="mf">GO:MF</option>' +
                    '<option value="cc">GO:CC</option>');
        } else if (org === "sce"){
            qOpts.append('<option value="kegg">KEGG</option>');
         }else{
            qOpts.append('<option value="kegg">None</option>');
            }

        var enrichAlgoOpts = $('#enrichAlgo');
        enrichAlgoOpts.empty();
        enrichAlgoOpts.append('<option value="hyp">Hypergeometric test</option>');

        var myht = $(window).height() - 20;

        var tabht = myht / 3;
        if (tabht < 260) {
            tabht = 260;
        }
        $('#mypane').height(myht - tabht);
        $('#tabcontainer').tabs({height: tabht, border: false});

        $("#spinner").fadeOut();
        
        sigInst.refresh();
        sigma.misc.animation.camera(sigInst.camera, {
            x: 0,
            y: 0,
            ratio: 1.0
        }, {duration: 300});
    });
}


function searchCommunity() {
    doCommunityDetection(function (res) {
        sub_modules = res.split("||");
        if (sub_modules[0] === "NA") {
            $.messager.progress('close');
            alert(sub_modules[1]);
            return;
        }
        var size_id_sbl = sub_modules[0].split(";"); //get size, id path and symbol path
        var id_path = size_id_sbl[3].split("->");
        if (id_path.length === 1) {
            $.messager.progress('close');
            alert(res);
            return;
        }
        var mdl_rows = [];
        for (var i = 0; i < sub_modules.length; i++) {
            var stats = sub_modules[i].split(";");
            mdl_rows.push({
                module: i,
                size: stats[0],
                qnum: stats[1],
                pval: stats[2],
                color: '<span id=\"module_' + i + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
            });
        }

        $('#mdg').datagrid({
            onSelect: function (index, row) {
                highlightCommunity(index);
                updateCellColor(highlightColor, "module_" + index);
            },
            onUnselect: function (index, row) {
                resetCommunity(index);
                current_fun_nodes = null;
                updateCellColor(greyColor, "module_" + index);
            }
        }).datagrid('loadData', mdl_rows);

        $.messager.progress('close');
    });
}

function doCommunityDetection(callBack) {
    var algo = $('#dmodule').val();
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: domainURL + 'faces/AjaxCall',
        data: 'function=performCommunityDetection' + "&method=" + algo,
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
            $.messager.alert('Error', 'Failed to process the request!', 'error');
            $.messager.progress('close');
        }
    });
}

function highlightCommunity(inx) {
    var id_sbl_path = sub_modules[inx].split(";");
    var ids = id_sbl_path[3].split("->");
    //var nds = id_sbl_path[1].split("->");
    var community = [];
    highlight_mode = 1;
    //update to allow edge size change
    sigInst.settings({
        minEdgeSize: 0.3,
        maxEdgeSize: 0.7
    });
    //sigInst.position(0, 0, 1).draw(2, 2, 2);
    sigInst.graph.edges().forEach(function (e) {
        if (ids.indexOf(e.target) !== -1 & ids.indexOf(e.source) !== -1) {
            e.size = 2;
            e.color = highlightColor;
            e.highlight = 1;
        } else {
            if (!e.highlight) { //other community
                e.color = greyColor;
                e.size = 1;
            }
        }
    });
    sigInst.graph.nodes().forEach(function (n) {
        if (ids.indexOf(n.id) !== -1) {
            n.color = highlightColor;
            n.highlight = 1;
            community.push({id: n.id, label: n.label});
        } else {
            if (!n.highlight) { //other community
                n.color = greyColor;
            }
        }
    });
    current_fun_nodes = ids;
    sigInst.refresh();
    displayCurrentSelectedNodes(community, "");
}

//when unselect a community, restore
function resetCommunity(inx) {
    var id_sbl_path = sub_modules[inx].split(";");
    var ids = id_sbl_path[3].split("->");

    //update to allow edge size change
    sigInst.settings({
        minEdgeSize: 0.3,
        maxEdgeSize: 0.3
    });
    //sigInst.position(0, 0, 1).draw(2, 2, 2);
    sigInst.graph.edges().forEach(function (e) {
        if (ids.indexOf(e.target) !== -1 & ids.indexOf(e.source) !== -1) {
            e.size = 1;
            e.color = greyColor;
            e.highlight = 0;
        }
    });
    sigInst.graph.nodes().forEach(function (n) {
        if (ids.indexOf(n.id) !== -1) {
            n.color = greyColor;
            n.highlight = 0;
        }
    });
    sigInst.refresh();
    $("#stats").empty();
}
