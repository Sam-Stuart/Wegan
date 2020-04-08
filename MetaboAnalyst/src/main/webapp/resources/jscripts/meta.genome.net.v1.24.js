/*
 * Javascript functions for MicrobiomeAnalyst
 * 2014-9-21
 * Jeff Xia (jeff.xia@mcgill.ca)
 */

//global vars
var net_path = '';
var greyColorB = '#bfbfbf';
var greyColorW = '#888888';
var pathnameOpt = 'hide';
var cmpdnameOpt = 'show';
var minorNodeColor = 'rgba(128,128,128,0.001)'; //hide the gene node
//var highlightColor = '#bb3300';
var highlightColor = '#FFFF00';
var backgroundColor = "#222222";
var up_color = "#FF0000";
var white_color = "#ffffff";
var down_color = "#008000";
var style_mode = "kegg";
var view_mode = "compound";
var node_rows = [];
var usr_dir;
var sigInst;
var settings = {
    edgeLabelSize: 'fixed',
    edgeLabelThreshold: 1
};
var sub_modules = [];
var hidden_mode = false;
var dim_mode = false;
var highlight_mode = false;
var mouseover_mode = false; // hide remote nodes
var dragListener;
var minEdgeSize = 0.1;
var defaultEdgeSize = 0.4;
var mediumEdgeSize = 0.3;
var thickEdgeSize = 0.5;
var focus_fun_anot_kos;
var focus_fun_anot_cmpds;
var focus_fun_anot_edges;
var expr_vals;
var current_fun_kos;
var current_fun_edges;
var current_fun_nodes;
var start_x;
var start_y;
var dependants;
var foreGround = {
    null: '#333',
    undefined: '#333',
    1: '#FFDADA', 2: '#FFCFCF', 3: '#FFC3C3', 4: '#FFB8B8', 5: '#FFACAC', 6: '#FFA1A1', 7: '#FF9595', 8: '#FF8A8A', 9: '#FF7E7E', 10: '#FF7373', 11: '#FF6767', 12: '#FF5C5C', 13: '#FF5050', 14: '#FF4545', 15: '#FF3939', 16: '#FF2E2E', 17: '#FF2222', 18: '#FF1717', 19: '#FF0B0B', 20: '#FF0000'
            // 1:'#FFCCCC', 2:'#FFC2C2', 3:'#FFB8B8', 4:'#FFADAD', 5:'#FFA3A3', 6:'#FF9999', 7:'#FF8F8F', 8:'#FF8585', 9:'#FF7A7A', 10:'#FF7070', 11:'#FF6666', 12:'#FF5C5C', 13:'#FF5252', 14:'#FF4747', 15:'#FF3D3D', 16:'#FF3333', 17:'#FF2929', 18:'#FF1F1F', 19:'#FF1414', 20:'#FF0A0A'
            //  1: '#FFFFFF', 2: '#FFF2F2', 3: '#FFE6E6', 4: '#FFD9D9', 5: '#FFCCCC', 6: '#FFBFBF', 7: '#FFB3B3', 8: '#FFA6A6', 9: '#FF9999', 10: '#FF8C8C', 11: '#FF8080', 12: '#FF7373', 13: '#FF6666', 14: '#FF5959', 15: '#FF4C4C', 16: '#FF4040', 17: '#FF3333', 18: '#FF2626', 19: '#FF1919', 20: '#FF0D0D'
// 1: '#FEF0D9', 2: '#FAE3CD', 3: '#F6D6C2', 4: '#F2CAB6', 5: '#EEBDAB', 6: '#EAB09F', 7: '#E6A494', 8: '#E29789', 9: '#DE8A7D', 10: '#DA7E72', 11: '#D67166', 12: '#D2655B', 13: '#CE584F', 14: '#CA4B44', 15: '#C63F39', 16: '#C2322D', 17: '#BE2522', 18: '#BA1916', 19: '#B60C0B', 20: '#B30000'
};

function initMicrobiomeAnalyst() {
    initFunctions();
    //first get user home dir
    domainURL = window.parent.document.getElementById("myurl").value;
    var res = $.ajax({
        type: "GET",
        url: domainURL + 'faces/AjaxCall?function=getjobinfo' + "&viewmode=pathwaynet&ignoreMe=" + new Date().getTime(),
        async: false
    }).responseText;
    //    alert(res);
    res =res.split("||");

    usr_dir = res[0];
    loadQueryData();
    loadEnrichTable("network_enrichment_pathway_0.json");
    setupMicrobiomeAnalyst();
    //perform default enrichment analysis
    $("#spinner").fadeOut("slow");
}

//construct dataSet name to sig.genes map
//load combo box for dataset names
var query_stat = {};
function loadQueryData() {
//clear it first
    $.getJSON(usr_dir + '/network_query.json', function (rawdata) {
        //note, we only have one object element
        query_stat = {}; //contain counts all unique genes
        $.each(rawdata, function (k, val) {
            query_stat[k] = val;
        });
    });
}

function setupMicrobiomeAnalyst() {
    $.messager.progress({
        text: 'Processing .....'
    });

    sigInst.graph.clear();

    //we only have one global map
    var net_path = '/MetaboAnalyst/faces/resources/libs/network/ko01100_map.json';

    $.getJSON(net_path, function (data) {
        for (var i = 0; i < data.nodes.pathways.length; i++) {
            var nd = data.nodes.pathways[i];
            nd.x = parseInt(nd.x);
            nd.y = parseInt(nd.y);
            nd.type = 'label.anchor';
            nd.size = 0.1;
            nd.color = minorNodeColor;
            //keep a copy
            nd.attr = {x: nd.x, y: nd.y, size: nd.size, color: nd.color, type: 'label.anchor'};
            sigInst.graph.addNode(nd);
        }

        for (var i = 0; i < data.nodes.compounds.length; i++) {
            var nd = data.nodes.compounds[i];
            nd.x = parseInt(nd.x);
            nd.y = parseInt(nd.y);
            nd.size = 2;
            nd.type = 'circle';
            //keep a copy
            nd.attr = {x: nd.x, y: nd.y, size: nd.size, color: nd.color, cid: nd.label, type: 'compound'};
            if (nd.hasOwnProperty('name')) {
                nd.label = nd.name; //for display, KEGG CID is now in attr cid
            }
            sigInst.graph.addNode(nd);
        }

        //need to add a fake node for scale properly
        var fake_nd = {id: 'mynode', x: 0, y: 0, size: 10, color: backgroundColor,
            attr: {x: 0, y: 0, size: 10, color: backgroundColor}};
        sigInst.graph.addNode(fake_nd);

        //do edges
        for (var j = 0; j < data.edges.length; j++) {
            var eg = data.edges[j];
            eg.size = defaultEdgeSize;
            var lvl = 0;
            if (query_stat[eg.id]) {
                lvl = query_stat[eg.id];
            }
            eg.attr = {color: eg.color, expr: lvl};
            sigInst.graph.addEdge(eg);
        }

        if (style_mode !== "kegg") {
            setupStyleMode();
        }
        sigInst.refresh();

        sigma.misc.animation.camera(sigInst.camera, {
            x: 0,
            y: 0,
            ratio: 1.0
        }, {duration: 250});

        //need to update the dg2 table, if selected, need to unselect
        var rows = $('#dg2').datagrid('getSelections');
        for (var i = 0; i < rows.length; i++) {
            updateCellColor("#ffffff", "function_" + rows[i].ID);
        }

        $('#dg2').datagrid('clearSelections');
        $('#dg2').datagrid('clearChecked');
        $.messager.progress('close');
    });
}

function setupStyleMode() {
    if (style_mode === "kegg") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === 'mynode') {
                n.color = backgroundColor;
            } else {
                n.x = n.attr.x;
                n.y = n.attr.y;
                n.color = n.attr.color;
            }
            n.hidden = false;
            n.highlight = false;
        });
        sigInst.graph.edges().forEach(function (e) {
            e.color = e.attr.color;
        });
    } else if (style_mode === "expr") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.attr.type === 'compound') {
                if (n.attr.cid in expr_vals) {
                    n.size = 4;
                    if (expr_vals[n.attr.cid] > 0) {
                        n.color = "#FF0000";
                    } else {
                        n.color = "#00FF00";
                    }
                }
            }
        });
        sigInst.graph.edges().forEach(function (e) {
            console.log(expr_vals);
            if (e.label in expr_vals) {
                e.color = highlightColor;
                e.highlight = true;
                e.size = 5 + Math.abs(e.attr.expr);
            }
        });
    } else if (style_mode === "plain") { //plain mode
        var mycol = greyColorW;
        if (backgroundColor === "#222222") {
            mycol = greyColorB;
        }
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === 'mynode') {
                n.color = backgroundColor;
            } else {
                if (!n.highlight) {
                    n.color = mycol;
                }
            }
        });
        sigInst.graph.edges().forEach(function (e) {
            if (!e.highlight) {
                e.color = mycol;
            }
        });
    } else {
        resetNetwork();
    }
    sigInst.refresh();
}

function setupViewMode() {
    sigInst.settings({
        minNodeSize: 0.1,
        maxNodeSize: 10
    });
    if (view_mode === "gene") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === 'mynode') {
                n.size = 10;
            } else {
                if (n.attr.type === 'compound') {
                    n.size = 0.1;
                } else {
                    n.size = 2;
                }
            }
        });
    } else { //compound mode
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === 'mynode') {
                n.size = 10;
            } else {
                if (n.attr.type === 'compound') {
                    n.size = 2;
                } else {
                    n.size = 0.1;
                }
            }
        });
    }
    sigInst.refresh();
}

var currentEnrichFile = "";

function loadEnrichTable(result) {
    $.getJSON(usr_dir + '/' + result, function (raw_data) {
        currentEnrichFile = result.substring(0, result.length - 5);
        focus_fun_anot_kos = raw_data['hits.query'];
        focus_fun_anot_edges = raw_data['hits.edge'];
        focus_fun_anot_nodes = raw_data['hits.node'];
        expr_vals = raw_data['expr.mat'];
        var fun_hit = raw_data['hit.num'];
        var fun_pval = raw_data['fun.pval'];
        var path_id = raw_data['path.id'];
        var data_grid = $('#dg2');
        //empty if there is any
        data_grid.datagrid('loadData', {
            "total": 0,
            "rows": []
        });
        var mdl_rows = [];
        var idx = 0;
        $.each(focus_fun_anot_kos, function (k, v) {
            mdl_rows.push({
                ID: path_id[idx],
                pathname: k,
                hit: fun_hit[idx],
                pval: fun_pval[idx],
                color: '<span style=\"background-color:#ffffff\" id=\"function_' + path_id[idx] + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
            });
            idx = idx + 1;
        });

        data_grid.datagrid({
            onSelect: function (index, row) {
                if (view_mode === 'gene') {
                    current_fun_kos = focus_fun_anot_kos[row.pathname];
                    highlightFunEnrichNodes(current_fun_kos, row.pathname);
                } else {
                    current_fun_edges = focus_fun_anot_edges[row.pathname];
                    if (typeof current_fun_edges !== 'undefined'){
                        if(current_fun_edges.length !== 0){
                            highlightFunEnrichEdges(current_fun_edges, row.pathname);
                        }
                    }
                    
                    current_fun_nodes = focus_fun_anot_nodes[row.pathname];
                    if (typeof current_fun_nodes !== 'undefined'){
                        if(current_fun_nodes.length !== 0){
                            highlightFunEnrichNodes(current_fun_nodes, row.pathname);  
                        }
                    }
                }
                updateCellColor(highlightColor, "function_" + row.ID);
            },
            onUnselect: function (index, row) {
                current_fun_edges = focus_fun_anot_edges[row.pathname];
                if (typeof current_fun_edges !== 'undefined'){
                    if(current_fun_edges.length !== 0){
                        unHighlightFunEnrichEdges(current_fun_edges);
                        updateCellColor("#ffffff", "function_" + row.ID);
                    }
                }
                current_fun_nodes = focus_fun_anot_nodes[row.pathname];
                if (typeof current_fun_nodes !== 'undefined'){
                    if(current_fun_nodes.length !== 0){
                        unHighlightFunEnrichNodes2(current_fun_nodes, row.pathname); 
                        updateCellColor("#ffffff", "function_" + row.ID);
                    }
                }
            }
        }).datagrid('loadData', mdl_rows);
    });
}
function testEnrichment() {
    doEnrichmentTests(function (result) {
        if (result === 'NA.json') {
            alert("Failed to perform enrichment test!");
        } else {
            loadEnrichTable(result);
        }
        $.messager.progress('close');
    });
}

function highlightFunEnrichNodes(nodes, title) {
//note: ko is in label, not node id
    sigInst.graph.nodes().forEach(function (n) {
        if (nodes.indexOf(n.id) !== -1) {
            n.size = n.size + 2;
            n.color = highlightColor;
            n.highlight = true;
            n.type = "circle";
        }
    });
    sigInst.refresh();
    
    displayPathInfo(title);
}

function unHighlightFunEnrichNodes2(nodes, title) {
//note: ko is in label, not node id
    sigInst.graph.nodes().forEach(function (n) {
        if (nodes.indexOf(n.id) !== -1) {
            n.size = 2;
            n.color = n.attr.color;
            n.highlight = false;
        }
    });
    sigInst.refresh();
}

function highlightFunEnrichEdges(edgeIDs, title) {
    sigInst.settings({
        minEdgeSize: defaultEdgeSize,
        maxEdgeSize: 5
    });

    sigInst.graph.edges().forEach(function (e) {
        if (edgeIDs.indexOf(e.id) !== -1) {
            e.color = highlightColor;
            e.highlight = true;
            e.size = 5 + Math.abs(e.attr.expr);
        } else if (!e.highlight) {
            e.size = defaultEdgeSize;
        }
    });
    sigInst.refresh();
    
    displayPathInfo(title);
}

function unHighlightFunEnrichEdges(edgeIDs, title) {
    var no_highlight = true; // need to see if there is no highlighted nodes left
    sigInst.graph.edges().forEach(function (e) {
        if (edgeIDs.indexOf(e.id) !== -1) {
            e.size = defaultEdgeSize;
            e.highlight = false;
            e.color = e.attr.color;
        } else {
            if (e.highlight) {
                no_highlight = false;
            }
        }
    });
    if (no_highlight) {
        sigInst.settings({
            minEdgeSize: defaultEdgeSize,
            maxEdgeSize: defaultEdgeSize
        });
    }
    sigInst.refresh();
       displayPathInfo('');
}

function doEnrichmentTests(callBack) {

    var fundb = $('#cbox').val();
    domainURL = window.parent.document.getElementById("myurl").value;
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "POST",
        url: domainURL + 'faces/AjaxCall',
        data: {function: 'enrichment_kO01100', db: fundb},
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

