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
var backgroundColor = "#f9f9f9";
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
var defaultEdgeSize = 0.4;
var current_fun_nodes;
var dependants;
var selectedNodeIDs = []; //for free style drag-and-drop
var usr_dir, org, vismode, dbid; //vismode: ppi, mir, drug; dbid is used for ppi and tf
var sigInst, settings;
var curr_pname;
var initialized = false;
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

function initNetwork(fname) {
    if(!initialized){
        initFunctions();
    }

    //first get user home dir
    //note domainURL should already initiated in pathviewer.js
    var res = $.ajax({
        type: "GET",
        url: domainURL + '?function=getjobinfo' + "&viewmode=pathway&ignoreMe=" + new Date().getTime(),
        async: false
    }).responseText;
    //    alert(res);
    res =res.split("||");
    usr_dir = res[0];
    org = res[1];
    setupNetwork(fname);
}

function downloadpng(){
    document.getElementById("downloadimage").src = export2Image();
    $("#pngdialog").dialog('open');
}

function downloadsvg(){
    sigInst.toSVG({download: true, filename: 'network_analyst.svg', labels: false, background: backgroundColor});
}

function downloadpng(){
    document.getElementById("downloadimage").src = export2Image();
    $("#pngdialog").dialog('open');
}

function downloadsvg(){
    sigInst.toSVG({download: true, filename: 'network_analyst.svg', labels: false, background: backgroundColor});
}

//initiate variables and attach functions
function initFunctions() {
    
    initialized = true;
    
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
            defaultLabelColor: 'black',
            defaultLabelSize: 12,
            defaultLabelBGColor: '#fff',
            defaultLabelHoverColor: '#000',
            labelThreshold: 6,
            defaultEdgeColor: 'default',
            doubleClickEnabled: false,
            minNodeSize: 1,
            maxNodeSize: 10,
            sideMargin: 1,
            minEdgeSize: defaultEdgeSize,
            maxEdgeSize: 1.4,
            defaultNodeBorder: 0
        }
    });
    
    var config = {
        nodeMargin: 8.0,
        scaleNodes: 1.3
    };

    // Configure the algorithm
    sigInst.configNoverlap(config);

    // Initialize the activeState plugin:
    var activeState = sigma.plugins.activeState(sigInst);

    // Initialize the dragNodes plugin:
    var dragListener = new sigma.plugins.dragNodes(sigInst, sigInst.renderers[0], activeState);
}

function loadNetwork(pname) {
    vizMode = "network";
    curr_pname = pname;
    $.ajax({
        dataType: "html",
        type: "POST",
        url: domainURL + '?function=viewNetwork' + '&name=' + pname,
        async: false,
        cache: false,
        timeout: 8000,
        success: function (result) {
            initNetwork(result);
            return;
        },
        error: function () {
            $.messager.alert('Error', 'Failed to process the request!', 'error');
            $.messager.progress('close');
        }
    });
}

//loadNetwork and NodeTable
function setupNetwork(content) {
    $.messager.progress({
        text: 'Processing .....'
    });
    
    sigInst.graph.clear();

    //we only have one global map
    //var net_path = '/MetaboAnalyst/faces/resources/libs/network/ko01100_map.json';
    //var $element = $("select[name='form:smpdbfname_input'] option:selected").val();
    content  = content.split(";");
    var fname = content[0];
    var metabo_matched = content[1].split(",");
    document.getElementById('title').innerHTML = '<a href="javascript:void(0);" onclick="window.open(\'http://www.smpdb.ca/view/' + content[2] + '\');">'+content[3]+'</a> '
            
    var net_path = usr_dir + '/' + fname;
    $.getJSON(net_path, function (data) {
        for (var i = 0; i < data.nodes.length; i++) {
            var nd = data.nodes[i];
            nd.size = 5;
            if("pwp_id" in nd){
                nd.x = parseInt(nd.x);
                nd.y = parseInt(nd.y);

                if(isNaN(nd.x)){
                    nd.x = 0;
                }
                if(isNaN(nd.y)){
                    nd.y = 0;
                }
                nd.color = "grey";
                nd.type = "square";
                nd.label = nd.name;
                //keep a copy
                nd.attr = {x: nd.x, y: nd.y, size: nd.size, color: nd.color, type: nd.type, label: nd.label};
                sigInst.graph.addNode(nd);
                
            } else{
                nd.x = parseInt(nd.x);               
                nd.y = parseInt(nd.y);
                    if(isNaN(nd.x)){
                        nd.x = 0;
                    }
                    if(isNaN(nd.y)){
                        nd.y = 0;
                    }
                nd.color = "#78B0FF";
                nd.label = nd.name;
                
                if(metabo_matched.indexOf(nd.hmdb_id) !== -1){
                    nd.borderColor = "#FF6700";
                    nd.size = nd.size + 1;
                } else {
                    nd.size = nd.size - 1;
                }
                
                //keep a copy
                nd.attr = {x: nd.x, y: nd.y, size: nd.size, color: nd.color, label: nd.label};
                sigInst.graph.addNode(nd);
            }
        }

        //need to add a fake node for scale properly
        var fake_nd = {id: 'mynode', hidden:true, x: 0, y: 0, size: 10, color: backgroundColor,
            attr: {x: 0, y: 0, size: 10, color: backgroundColor}};
        sigInst.graph.addNode(fake_nd);

        //do edges
        for (var j = 0; j < data.edges.length; j++) {
            var eg = data.edges[j];
            eg.size = defaultEdgeSize;
            eg.id = j.toString();
            eg.color = "grey";
            var lvl = 1;
            eg.attr = {color: eg.color, expr: lvl};
            sigInst.graph.addEdge(eg);
        }

        $("#spinner").fadeOut();
        
        //sigInst.startForceAtlas2({gravity: 200, scalingRatio: 70, slowDown: 100, barnesHutOptimize: true, startingIterations: 30, iterationsPerRender: 20});
        sigInst.refresh();
        sigInst.startNoverlap();
        // Following code is for startForceAtlas2 effect
        //    setTimeout(function () {
        //        sigInst.killForceAtlas2();
        //    }, 2000);
        sigma.misc.animation.camera(sigInst.camera, {
            x: 0,
            y: 0,
            ratio: 1.0
        }, {duration: 250});

        $.messager.progress('close');
    });
}
