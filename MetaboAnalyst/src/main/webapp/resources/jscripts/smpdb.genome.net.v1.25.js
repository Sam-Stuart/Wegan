/*
 * Javascript functions for network analysis
 * 2014-9-21
 * Jeff Xia (jeff.xia@mcgill.ca)
 */

function highlightMyNodes() {//custom signature profiling
    //need to get each line
    var unhigh = $("#unhigh").prop('checked');
    var ids = $('#signature').val().split('\n');
    highlightNodes(ids, unhigh);
}

function highlightNodes(ids, unhigh) {//custom signature profiling
    var borderCol = getBorderColor();
    sigInst.graph.nodes().forEach(function (n) {
        if (ids.indexOf(n.id) !== -1 || ids.indexOf(n.label) !== -1) {
            n.size = n.size + 2;
            n.highlight = 1;
            n.color = highlightColor;
            n.borderColor = borderCol;
            found = true;
        } else {
            n.borderColor = null;
            if (unhigh) {
                n.size = n.true_size;
                if (backgroundColor === "#222222") {
                    n.color = n.colorb;
                } else {
                    n.color = n.colorw;
                }
                n.highlight = 0;
            }
        }
    });
    sigInst.refresh();
    sigInst.camera.goTo({x: 0, y: 0, angle: 0, ratio: 1.0});
}

function highlightSelectedNode(nodeID) {
    highlight_mode = 1;
    var borderCol = getBorderColor();
    if ($('#selectOpt').val() === "neighbour") {
        var neighbors = {};
        neighbors[nodeID] = 1;
        sigInst.graph.edges().forEach(function (e) {
            if (nodeID === e.source || nodeID === e.target) {
                neighbors[e.source] = 1;
                neighbors[e.target] = 1;
                e.color = highlightColor;
                e.highlight = 1;
            }
        });
        sigInst.graph.nodes().forEach(function (n) {
            if (neighbors[n.id]) {
                n.color = highlightColor;
                n.borderColor = borderCol;
                n.highlight = 1;
                n.size = n.size + 2;
            } else {
                n.borderColor = null;
            }
        });
    } else {
        var found = false;
        sigInst.graph.nodes().forEach(function (n) {
            if (n.id === nodeID || n.label.split("; ").indexOf(nodeID) !== -1) {
                n.size = n.size + 2;
                n.highlight = 1;
                n.color = highlightColor;
                n.borderColor = borderCol;
                found = true;
            } else {
                n.borderColor = null;
            }
        });
        if (!found) {
            alert("Cannot find a node with the given ID!");
        }
    }
    sigInst.refresh();
}

function updateBackground(backgroundColor) {
    $("#networkspace").css('background-color', '').css('background-color', backgroundColor);
    if (backgroundColor === "white") {
        sigInst.settings({
            defaultLabelColor: '#fff',
            defaultLabelBGColor: '#fff'
        });
        if (view_mode === "topo") {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.true_color_b;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.true_color_b;
                });
            }
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.expr_colorb;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.expr_colorb;
                });
            }
        }
    } else {
        sigInst.settings({
            defaultLabelColor: '#000',
            defaultLabelBGColor: '#000'
        });

        if (view_mode === "topo") {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.true_color_w;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.true_color_w;
                });
            }
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                n.color = n.expr_colorw;
            });
            if (edgeColoring) {
                sigInst.graph.edges().forEach(function (e) {
                    e.color = e.expr_colorw;
                });
            }
        }
    }
    sigInst.refresh();
}

function displayNodeInfo(node) {
    var stats = $("#stats");
    stats.empty();
    if (vismode === "gene_metabolites") {
        if (node.type === "circle") {
            stats.append('<li><strong>Symbol</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>Name</strong>: ' + node.genename + '</li>');
            stats.append('<li><strong>Genbank</strong>: ' + '<a href="http://www.ncbi.nlm.nih.gov/gene/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>KEGG COMPOUND</strong>: ' + '<a href=" http://www.genome.jp/dbget-bin/www_bget?cpd:' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "global") {
        if (node.type === "circle") {
            stats.append('<li><strong>Symbol</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>Name</strong>: ' + node.genename + '</li>');
            stats.append('<li><strong>Genbank</strong>: ' + '<a href="http://www.ncbi.nlm.nih.gov/gene/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else if (node.type === "diamond") {
            stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>KEGG COMPOUND</strong>: ' + '<a href=" http://www.genome.jp/dbget-bin/www_bget?cpd:' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>OMIM</strong>: ' + '<a href=" https://www.omim.org/entry/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "metabo_phenotypes") {
        stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');
        if (node.type === "circle") {
            stats.append('<li><strong>KEGG COMPOUND</strong>: ' + '<a href=" http://www.genome.jp/dbget-bin/www_bget?cpd:' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>OMIM</strong>: ' + '<a href=" https://www.omim.org/entry/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "gene_phenotypes") {
        if (node.type === "circle") {
            stats.append('<li><strong>Symbol</strong>: ' + node.label + '</li>');
            stats.append('<li><strong>Name</strong>: ' + node.genename + '</li>');
            stats.append('<li><strong>Genbank</strong>: ' + '<a href="http://www.ncbi.nlm.nih.gov/gene/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>OMIM</strong>: ' + '<a href=" https://www.omim.org/entry/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "metabo_metabolites") {
        stats.append('<li><strong>Name</strong>: ' + node.label + '</li>');
        stats.append('<li><strong>KEGG COMPOUND</strong>: ' + '<a href=" http://www.genome.jp/dbget-bin/www_bget?cpd:' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
    } else if (vismode === "ppi") {
        if (dbid === "string") {
            stats.append('<li><strong>Ensembl</strong>: ' + '<a href="http://www.ensembl.org/id/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>Uniprot</strong>: ' + '<a href="http://www.uniprot.org/uniprot/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "mir") {
        if (node.type === "circle") {
            stats.append('<li><strong>Genbank</strong>: ' + '<a href="http://www.ncbi.nlm.nih.gov/gene/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>MirBase</strong>: ' + '<a href="http://www.mirbase.org/cgi-bin/mature.pl?mature_acc=' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "drug") {
        if (node.type === "circle") {
            stats.append('<li><strong>Uniprot</strong>: ' + '<a href="http://www.uniprot.org/uniprot/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>DrugBank</strong>: ' + '<a href="https://www.drugbank.ca/drugs/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "chem") {
        if (node.type === "circle") {
            stats.append('<li><strong>Genbank</strong>: ' + '<a href="http://www.ncbi.nlm.nih.gov/gene/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        } else {
            stats.append('<li><strong>CTD</strong>: ' + '<a href=" http://ctdbase.org/detail.go?type=chem&acc=' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
        }
    } else if (vismode === "tf") {
        stats.append('<li><strong>Genbank</strong>: ' + '<a href="http://www.ncbi.nlm.nih.gov/gene/' + node.id + '" target="_blank"><u>' + node.id + '</u></a>' + '</li>');
    }
}

function doNodeEntrezSearch(ID, callBack) {
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Retrieving node information .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: '/MetaboAnalyst/faces/AjaxCall',
        //data: 'function=getNodeEntrez' + "&uniprot=" + ID,
        data: 'function=getNodeStrEntrez' + "&emblprotein=" + ID,
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
            alert("Failed to process the request!");
            $.messager.progress('close');
        }
    });
}

function displayCurrentSelectedNodes(nodes, title) {
    var stats = $("#stats");
    stats.empty();
    if (title !== "") {
        stats.append('<lh><b>' + title + '</b></lh>');
    }
    for (var i = 0; i < nodes.length; i++) {
        // stats.append('<li><a href="http://www.genome.jp/dbget-bin/www_bget?ko:' + nodes[i].id + '" target="_blank"><u>' + nodes[i].label + '</u></a></li>');
        stats.append('<li>' + nodes[i].label + '</li>');
    }
}

//network layout updates will remove all node properties, need to re-do record
function updateNetworkLayout(fileNm) {
    $.getJSON(usr_dir + '/' + fileNm, function (data) {
        var nd_pos = {};
        for (var i = 0; i < data.nodes.length; i++) {
            var nd = data.nodes[i];
            nd_pos[nd.id] = [nd.x, nd.y];
        }
        var my_nd;
        sigInst.graph.nodes().forEach(function (nd) {
            my_nd = nd_pos[nd.id];
            nd.x = my_nd[0];
            nd.y = my_nd[1];
        });
        sigInst.refresh();
        sigma.misc.animation.camera(sigInst.camera, {
            x: 0,
            y: 0,
            ratio: 1.0
        }, {duration: 200});
    });
}

function pagerFilter(data) {
    if (typeof data.length === 'number' && typeof data.splice === 'function') {    // is array
        data = {
            total: data.length,
            rows: data
        };
    }
    var dg = $('#dg');
    var opts = dg.datagrid('options');
    var pager = dg.datagrid('getPager');
    pager.pagination({
        showPageList: false,
        showRefresh: false,
        displayMsg: "",
        onSelectPage: function (pageNum, pageSize) {
            opts.pageNumber = pageNum;
            opts.pageSize = pageSize;
            pager.pagination('refresh', {
                pageNumber: pageNum,
                pageSize: pageSize
            });
            dg.datagrid('loadData', data);
        }
    });
    if (!data.originalRows) {
        data.originalRows = (data.rows);
    }
    var start = (opts.pageNumber - 1) * parseInt(opts.pageSize);
    var end = start + parseInt(opts.pageSize);
    data.rows = (data.originalRows.slice(start, end));
    return data;
}

function reloadNodeTable() {
    var dg = $('#dg');
    //clear data and selection
    dg.datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    dg.datagrid('clearSelections');
    dg.datagrid('clearChecked');
    dg.datagrid('loadData', node_rows);
    resetNetwork();
}

function resetNodes(nodeIDs) {
    sigInst.graph.nodes().forEach(function (n) {
        if (nodeIDs.indexOf(n.id) !== -1) {
            if (n.highlight) {
                n.highlight = 0;
                n.borderColor = null;
                n.size = n.true_size;
                if (view_mode == "topo") {
                    if (backgroundColor === "#222222") {
                        n.color = n.colorb;
                    } else {
                        n.color = n.colorw;
                    }
                } else { //expression mode
                    if (backgroundColor === "#222222") {
                        n.color = n.expr_colorb;
                    } else {
                        n.color = n.expr_colorw;
                    }
                }
                n.hidden = false;
            }
        }
    });
    sigInst.refresh();
}

function updateMouseoverEffect() {
    var type = $('#mouseOpt').val();
    if (type === 'enable') {
        mouseover_mode = true;
    } else if (type === 'disable') {
        mouseover_mode = false;
    } else {
        //nothing
    }
}

function updateHighlightColor() {
    var type = $('#visOpt').val();
    if (type === "dim") {
        hidden_mode = false;
        dim_mode = true;
        sigInst.graph.edges().forEach(function (e) {
            e.color = greyColorB;
        });
        sigInst.graph.nodes().forEach(function (n) {
            n.hidden = false;
            if (!n.highlight) {
                n.color = greyColorB;
            }
        });
    } else if (type === "hide") {
        hidden_mode = true;
        sigInst.graph.nodes().forEach(function (n) {
            if (!n.highlight) {
                n.hidden = true;
            }
        });
    } else {
        hidden_mode = false;
        dim_mode = false;
        sigInst.graph.nodes().forEach(function (n) {
            if (!n.highlight) {
                n.hidden = false;
                n.size = n.true_size;
                if (backgroundColor === "#222222") {
                    n.color = n.colorb;
                } else {
                    n.color = n.colorw;
                }
            }
        });
    }
    sigInst.refresh();
}

//used for manual mode
function highlightQueries(nodeVec) {
    var community = [];
    highlight_mode = 1;
    //sigInst.position(0, 0, 1).draw(2, 2, 2);

    sigInst.graph.edges().forEach(function (e) {
        e.color = greyColorB;
    });
    sigInst.graph.nodes().forEach(function (n) {
        if (nodeVec.indexOf(n.id) !== -1) {
            n.color = highlightColor;
            n.highlight = 1;
            community.push({id: n.id, label: n.label});
        } else {
            if (!n.highlight) {
                n.color = greyColorB;
            }
        }
    });

    sigInst.refresh();
    highlight_mode = 1;
    displayCurrentSelectedNodes(community, "");
}

function searchNodeTable() {
    var search = $('#nodeid').val();
    if (search === "") {
        resetNetwork();
        return;
    }
    var hitrow = "NA";
    var current_row;
    for (var i = 0; i < node_rows.length; i++) {
        current_row = node_rows[i];
        if (current_row.ID === search | current_row.Label === search) {
            hitrow = current_row;
            //node_rows.splice(i, 1);

            //note, page size is 30
            var page_num = Math.ceil(i / 30);
            var row_num = i % 30;
            $('#dg').datagrid('gotoPage', page_num);
            $('#dg').datagrid('loadData', node_rows);
            $('#dg').datagrid('selectRow', row_num);
            break;
        }
    }
    if (hitrow === "NA") {
        $.messager.alert('', "Could not find the given node: " + search, 'info');
        return;
    }
}

function updateNodeTable(nodeIDs) {
    var current_rows = [];
    for (var i = 0; i < node_rows.length; i++) {
        if (nodeIDs.indexOf(node_rows[i].ID) !== -1) {
            current_rows.push(node_rows[i]);
        }
    }

    var data_grid = $('#dg');
    //empty it first if there is any
    data_grid.datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    data_grid.datagrid('loadData', current_rows);
}

function sortNodeTable(colID, order) {
    if (order === 'desc') {
        if (colID === "Betweenness") {
            node_rows.sort(function (a, b) {
                return b.Betweenness - a.Betweenness;
            });
        } else if (colID === "Degree") {
            node_rows.sort(function (a, b) {
                return b.Degree - a.Degree;
            });
        } else if (colID === "Label") {
            node_rows.sort(function (a, b) {
                if (a.Label < b.Label)
                    return 1;
                if (a.Label > b.Label)
                    return -1;
                return 0;
            });
        } else if (colID === "Status") {
            node_rows.sort(function (a, b) {
                if (a.Status === "-" & b.Status !== "-")
                    return 1;
                if (a.Status !== "-" & b.Status === "-")
                    return -1;
                if (a.Status < b.Status)
                    return 1;
                if (a.Status > b.Status)
                    return -1;
                return 0;
            });
        } else {
            node_rows.sort(function (a, b) {
                if (a.ID < b.ID)
                    return 1;
                if (a.ID > b.ID)
                    return -1;
                return 0;
            });
        }
    } else {
        if (colID === "Betweenness") {
            node_rows.sort(function (a, b) {
                return a.Betweenness - b.Betweenness;
            });
        } else if (colID === "Degree") {
            node_rows.sort(function (a, b) {
                return a.Degree - b.Degree;
            });
        } else if (colID === "Label") {
            node_rows.sort(function (a, b) {
                if (a.Label < b.Label)
                    return -1;
                if (a.Label > b.Label)
                    return 1;
                return 0;
            });
        } else if (colID === "Status") {
            node_rows.sort(function (a, b) {
                if (a.Status === "-" & b.Status !== "-")
                    return -1;
                if (a.Status !== "-" & b.Status === "-")
                    return 1;
                if (a.Status < b.Status)
                    return -1;
                if (a.Status > b.Status)
                    return 1;
                return 0;
            });
        } else {
            node_rows.sort(function (a, b) {
                if (a.ID < b.ID)
                    return -1;
                if (a.ID > b.ID)
                    return 1;
                return 0;
            });
        }
    }

    var data_grid = $('#dg');
    //empty it first if there is any
    data_grid.datagrid('loadData', {
        "total": 0,
        "rows": []
    });
    data_grid.datagrid('loadData', node_rows);
}

function getBorderColor() {
    if (backgroundColor === "#222222") {
        return("#FAFAD2");
    } else {
        return("#A0522D");
    }
}
function searchNetwork(nodeID) {
    //$.messager.progress();
    var hit = 0;
    //centering 
    //sigInst.position(0, 0, 1).draw(2, 2, 2);
    //then Loop all nodes
    var borderCol = getBorderColor();
    sigInst.graph.nodes().forEach(function (n) {
        if (n.id === nodeID) {
            hit = 1;
            n.size = n.size + 2;
            //n.borderColor = "#FFFF00";
            n.borderColor = borderCol;
            n.highlight = 1;
            sigma.misc.animation.camera(
                    sigInst.camera,
                    {
                        x: n[sigInst.camera.readPrefix + 'x'],
                        y: n[sigInst.camera.readPrefix + 'y'],
                        ratio: 0.35
                    },
                    {duration: 300});
            displayNodeInfo(n);
        } else {
            n.borderColor = null;
        }
    });
    sigInst.refresh();
    if (!hit) {
        $.messager.alert('Error', "Node " + nodeID + " was not found in the current network!", 'error');
    }
    //$.messager.progress('close');
    return;
}

function resetNetwork() {
    sigInst.settings({
        minEdgeSize: defaultEdgeSize,
        maxEdgeSize: defaultEdgeSize
    });
    //default no edge coloring
    $('#eColOpt').val("off");

    sigInst.camera.goTo({x: 0, y: 0, angle: 0, ratio: 1.0});
    if (view_mode === "topo") {
        if (backgroundColor === "#222222") {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.true_size;
                n.color = n.true_color_b;
                n.borderColor = null;
                n.hidden = false;
                n.highlight = false;
            });
            sigInst.graph.edges().forEach(function (e) {
                //   e.color = e.true_color_b;
                e.size = 1;
                e.highlight = false;
                e.hidden = false;
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.true_size;
                n.color = n.true_color_w;
                n.borderColor = null;
                n.hidden = false;
                n.highlight = false;
            });
            sigInst.graph.edges().forEach(function (e) {
                //e.color = e.true_color_w;
                e.size = 1;
                e.highlight = false;
                e.hidden = false;
            });
        }
    } else if (view_mode === "expr") {
        if (backgroundColor === "#222222") {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.true_size;
                n.color = n.expr_colorb;
                n.borderColor = null;
                n.hidden = false;
                n.highlight = false;
            });
            sigInst.graph.edges().forEach(function (e) {
                //e.color = e.expr_colorb;
                e.size = 1;
                e.highlight = false;
                e.hidden = false;
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.true_size;
                n.color = n.expr_colorw;
                n.borderColor = null;
                n.hidden = false;
                n.highlight = false;
            });
            sigInst.graph.edges().forEach(function (e) {
                //   e.color = e.expr_colorw;
                e.size = 1;
                e.highlight = false;
                e.hidden = false;
            });
        }
    } else { //plain mode
        sigInst.graph.nodes().forEach(function (n) {
            n.size = n.true_size;
            n.color = greyColorB;
            n.borderColor = null;
            n.hidden = false;
            n.highlight = false;
        });
        sigInst.graph.edges().forEach(function (e) {
            e.color = greyColorB;
            e.size = 1;
            e.highlight = false;
            e.hidden = false;
        });
    }
    sigInst.refresh();
}

function highlightSelectedNodes(nodes, title) {
    if (nodes.length === 1) {
        searchNetwork(nodes[0]);
    } else {
        var nodeVec = [];
        highlight_mode = 1;
        var borderCol = getBorderColor();
        //sigInst.position(0, 0, 1).draw(2, 2, 2);
        if (dim_mode) {
            sigInst.graph.edges().forEach(function (e) {
                e.color = greyColorB;
            });
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.size = n.size + 2;
                    if (n.size > 10) {
                        n.size = 10;
                    }
                    n.borderColor = borderCol;
                    n.highlight = 1;
                    nodeVec.push({id: n.id, label: n.label});
                } else {
                    if (!n.highlight) {
                        n.color = greyColorB;
                    }
                    n.borderColor = null;
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.size = n.size + 2;
                    if (n.size > 10) {
                        n.size = 10;
                    }
                    n.borderColor = borderCol;
                    n.highlight = 1;
                    nodeVec.push({id: n.id, label: n.label});
                } else {
                    n.borderColor = null;
                }
            });
        }
        sigInst.refresh();
        displayCurrentSelectedNodes(nodeVec, title);
    }
}

function highlightFunEnrichNodes(nodes, title) {
    if (nodes.length === 1) {
        searchNetwork(nodes[0]);
    } else {
        var nodeVec = [];
        highlight_mode = 1;
        //var borderCol = getBorderColor();
        //sigInst.position(0, 0, 1).draw(2, 2, 2);
        if (dim_mode) {
            sigInst.graph.edges().forEach(function (e) {
                e.color = greyColorB;
            });
            if (view_mode === "topo") {
                sigInst.graph.nodes().forEach(function (n) {
                    if (nodes.indexOf(n.id) !== -1) {
                        n.size = n.size + 2;
                        if (n.size > 10) {
                            n.size = 10;
                        }
                        n.color = highlightColor;
                        n.highlight = 1;
                        nodeVec.push({id: n.id, label: n.label});
                    } else {
                        if (!n.highlight) {
                            n.color = greyColorB;
                        }
                    }
                });
            } else { //expression, use border to highlight
                sigInst.graph.nodes().forEach(function (n) {
                    if (nodes.indexOf(n.id) !== -1) {
                        n.size = n.size + 2;
                        if (n.size > 10) {
                            n.size = 10;
                        }
                        n.borderColor = highlightColor;
                        n.highlight = 1;
                        nodeVec.push({id: n.id, label: n.label});
                    } else {
                        if (!n.highlight) {
                            n.borderColor = null;
                        }
                    }
                });
            }
        } else {
            if (view_mode === "topo") {
                sigInst.graph.nodes().forEach(function (n) {
                    if (nodes.indexOf(n.id) !== -1) {
                        n.size = n.size + 2;
                        if (n.size > 10) {
                            n.size = 10;
                        }
                        n.color = highlightColor;
                        n.highlight = 1;
                        nodeVec.push({id: n.id, label: n.label});
                    }
                });
            } else {
                sigInst.graph.nodes().forEach(function (n) {
                    if (nodes.indexOf(n.id) !== -1) {
                        n.size = n.size + 2;
                        if (n.size > 10) {
                            n.size = 10;
                        }
                        n.borderColor = highlightColor;
                        n.highlight = 1;
                        nodeVec.push({id: n.id, label: n.label});
                    } else {
                        if (!n.highlight) {
                            n.borderColor = null;
                        }
                    }
                });
            }
        }
        sigInst.refresh();
        displayCurrentSelectedNodes(nodeVec, title);
    }
}

function unHighlightFunEnrichNodes(nodes) {

    if (view_mode === "topo") {
        if (backgroundColor === "#222222") {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.borderColor = null;
                    n.size = n.true_size;
                    n.color = n.true_color_b;
                    n.highlight = 0;
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.borderColor = null;
                    n.size = n.true_size;
                    n.color = n.true_color_w;
                    n.highlight = 0;
                }
            });
        }
    } else if (view_mode === "expr") {
        if (backgroundColor === "#222222") {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.borderColor = null;
                    n.size = n.true_size;
                    n.color = n.expr_colorb;
                    n.highlight = 0;
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (nodes.indexOf(n.id) !== -1) {
                    n.borderColor = null;
                    n.size = n.true_size;
                    n.color = n.expr_colorw;
                    n.highlight = 0;
                }
            });
        }
    } else { //plain mode
        sigInst.graph.nodes().forEach(function (n) {
            if (nodes.indexOf(n.id) !== -1) {
                n.borderColor = null;
                n.size = n.true_size;
                n.color = greyColorB;
                n.highlight = 0;
            }
        });
    }

    sigInst.refresh();
}

function highlightRegularNodes(nodes) {
    if (nodes.length === 1) {
        searchNetwork(nodes[0]);
    } else {
        var nodeVec = [];
        highlight_mode = 1;

        sigInst.graph.edges().forEach(function (e) {
            e.color = greyColorB;
        });
        sigInst.graph.nodes().forEach(function (n) {
            if (nodes.indexOf(n.id) !== -1) {
                n.size = n.size + 1;
                n.color = highlightColor;
                n.highlight = 1;
                nodeVec.push({id: n.id, label: n.label});
            } else {
                if (!n.highlight) {
                    n.color = greyColorB;
                }
            }
        });
        sigInst.refresh();
        displayCurrentSelectedNodes(nodeVec, "");
    }
}

function extractNetwork() {
    var nodeIDs = [];
    sigInst.graph.nodes().forEach(function (n) {
        if (n.highlight) {
            nodeIDs.push(n.id);
        }
    });
    if (nodeIDs.length === 0) {
        $.messager.alert('Error', 'No highlighted nodes found in the network!', 'error');
        return;
    }
    doNetworkExtract(nodeIDs, function (result) {
        if (result !== "NA") {
            setupNetwork(result);
            var netNm = result.substring(0, result.length - 5);
            updateNetworkOpts(netNm);
        } else {
            $.messager.alert('Error', 'Could not find a module with at least 3 nodes. Make sure the highlighted nodes are connected!', 'error');
        }
        $.messager.progress('close');
    });
}

function doNetworkExtract(nodeIDs, callBack) {
    $.messager.confirm('Extract Confirmation',
            "Are you sure to perform module extraction? Note, all created networks are availalbe in the <b>Network</b> menu on top. ",
            function (r) {
                if (r) {
                    $.ajax({
                        beforeSend: function () {
                            $.messager.progress({
                                text: 'Processing .....'
                            });
                        },
                        dataType: "html",
                        type: "POST",
                        url: 'MetaboAnalyst/faces/AjaxCall',
                        data: {function: 'extractModule', nodeIDs: nodeIDs.join(";")},
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
            });
}

//when user resize the network, trigger this function
function resizeNetwork() {
    //dynamic update centerpane width based on user browser
    $('#network-parent').css("width", $(window).width() - 634);
    $('#network-parent').css("height", $(window).height() - 4);
    if (sigInst !== undefined) {
        sigInst.refresh();
    }
}

function updateNodeSize() {
    var type = $('#nodeOpt').val();
    var val = $('#sizeOpt').val();
    if (val === 'increase') {
        if (type === "all") {
            sigInst.graph.nodes().forEach(function (n) {
                n.size = n.size + 2;
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.highlight) {
                    n.size = n.size + 2;
                }
            });
        }
    } else {
        if (type === 'all') {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.size > 2) {
                    n.size = n.size - 1;
                }
            });
        } else {
            sigInst.graph.nodes().forEach(function (n) {
                if (n.highlight) {
                    if (n.size > 2) {
                        n.size = n.size - 1;
                    }
                }
            });
        }
    }
    sigInst.refresh();
}

function updateEdgeView() {
    var type = $('#shapeOpt').val();
    var eCol = $('#eColOpt').val();
    var val = $('#widthOpt').val();
    if (val === 'increase') {
        defaultEdgeSize = defaultEdgeSize + 0.2;
    } else {
        defaultEdgeSize = defaultEdgeSize - 0.1;
    }
    if (defaultEdgeSize < 0.1) {
        defaultEdgeSize = 0.1;
    }
    sigInst.settings({
        minEdgeSize: defaultEdgeSize,
        maxEdgeSize: defaultEdgeSize
    });

    if (eCol === "on") {
        edgeColoring = true;
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
        sigInst.graph.edges().forEach(
                function (e) {
                    e.type = type;
                    switch (display_mode) {
                        case 0:
                            e.color = e.true_color_b;
                            break;
                        case 1:
                            e.color = e.true_color_w;
                            break;
                        case 2:
                            e.color = e.expr_colorb;
                            break;
                        default:
                            e.color = e.expr_colorb;
                    }
                });
    } else {
        edgeColoring = false;
        var edgeCol = greyColorW;
        if (backgroundColor === "#222222") {
            edgeCol = greyColorB;
        }
        sigInst.graph.edges().forEach(
                function (e) {
                    e.type = type;
                    e.color = edgeCol;
                });
    }
    sigInst.refresh();
}

function doLayoutUpdate(algo, callBack) {
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: 'MetaboAnalyst/faces/AjaxCall',
        data: "function=updateNetworkLayout" + "&layoutalgo=" + algo,
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

function doNetworkPrep(netNm, callBack) {
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: 'MetaboAnalyst/faces/AjaxCall',
        data: "function=prepareNetwork" + "&netName=" + netNm,
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

function computeShortestPath() {
    var pathLnks = $("#pathLnks");
    pathLnks.empty();
    doShortestPathCompute(function (res) {
        nds_paths = res.split("||");
        var id_sbl_path = nds_paths[0].split(";"); //get id path and symbol path
        var id_path = id_sbl_path[0].split("->");
        if (id_path.length === 1) {
            $.messager.progress('close');
            alert(res);
            return;
        }

        for (var i = 0; i < nds_paths.length; i++) {
            pathLnks.append('<li><a href="#" onclick="highlightShortestPath(' + i + ');return false;"><u>' + nds_paths[i].split(";")[1] + '</u></a></li>');
        }

        if (nds_paths.length > 0) {
            highlightShortestPath(0);
        }
        $.messager.progress('close');
    });
}

function highlightShortestPath(inx) {
    var nodeVec = [];
    highlight_mode = 1;
    var id_sbl_path = nds_paths[inx].split(";");
    var ids = id_sbl_path[0].split("->");
    var nds = id_sbl_path[1].split("->");
    //update to allow edge size change
    sigInst.settings({
        maxEdgeSize: 1.2
    });
    //sigInst.refresh();

    sigInst.graph.edges().forEach(function (e) {
        if (ids.indexOf(e.target) !== -1 & ids.indexOf(e.source) !== -1) {
            e.size = 1.2;
            e.color = highlightColor;
            e.highlight = 1;
        } else {
            if (!e.highlight) {
                e.color = greyColorB;
                e.size = defaultEdgeSize;
            }
        }
    });
    sigInst.graph.nodes().forEach(function (n) {
        if (nds.indexOf(n.label) !== -1) {
            n.color = highlightColor;
            n.highlight = 1;
            n.size = n.size + 3;
            nodeVec.push({id: n.id, label: n.label});
        } else {
            if (!n.highlight) {
                n.size = n.true_size;
                n.color = greyColorB;
            }
        }
    });
    sigInst.refresh();
    displayCurrentSelectedNodes(nodeVec, "");
}

function doShortestPathCompute(callBack) {
    var src = $('#source').val();
    var target = $('#target').val();
    //note, convert them to a valid node ID
    var srcID = "NA";
    var targetID = "NA";
    var ndLbls = ["NA"];
    sigInst.graph.nodes().forEach(function (n) {
        ndLbls = n.label.split("; ");
        if (ndLbls.indexOf(src) !== -1 || n.id === src) {
            srcID = n.id;
        } else if (ndLbls.indexOf(target) !== -1 || n.id === target) {
            targetID = n.id;
        }
    });
    if (srcID === "NA") {
        $.messager.alert('Error', 'This source node is not in the network!', 'error');
        $.messager.progress('close');
        return;
    }
    if (targetID === "NA") {
        $.messager.alert('Error', 'This target node is not in the network!', 'error');
        $.messager.progress('close');
        return;
    }
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: 'MetaboAnalyst/faces/AjaxCall',
        data: 'function=getShortestPaths' + "&source=" + srcID + "&target=" + targetID,
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

function updateCellColor(color, id) {
    $("#" + id).css("background-color", color);
}

function exportResultTable(name) {
    if (name === "funtb") {
        if ($('#dg2').datagrid('getRows').length === 0) {
            $.messager.alert('Error', 'No functional enrichment analysis has been performed!', 'error');
            return;
        }
    }
    if (name === "comtb") {
        setupFileDownload("module_table.csv");
    } else if (name === "funtb") {
        setupFileDownload(currentEnrichFile + ".csv");
    } else {
        doGraphExport(setupFileDownload, name);
    }
}

function setupFileDownload(result) {
    var fileLnk = $("#fileLnk");
    fileLnk.empty();
    fileLnk.append("Right click the link below, then 'Save Link As ... ' to download the file<br/><br/>");
    fileLnk.append('<strong><a href="' + usr_dir + '/' + result + '" target="_blank"><u>' + result + '</u></a></strong>');
    $.messager.progress('close');
    $("#filedialog").dialog('open');
}

//should be called from exportNetwork
function doGraphExport(callBack, format) {
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: 'MetaboAnalyst/faces/AjaxCall',
        data: "function=exportNetwork" + "&format=" + format,
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

var currentEnrichFile = "";
var focus_fun_anot;
function testEnrichment() {
    doEnrichmentTests(function (result) {
        if (result === 'NA.json') {
            alert("Failed to perform enrichment test!");
        } else {
            $.getJSON(usr_dir + '/' + result, function (raw_data) {
                currentEnrichFile = result.substring(0, result.length - 5);
                focus_fun_anot = raw_data['fun.anot'];

                if (typeof focus_fun_anot === 'undefined') {
                    focus_fun_anot = raw_data['hits.query'];
                }

                var fun_hit = raw_data['hit.num'];
                var fun_pval = raw_data['fun.pval'];

                var data_grid = $('#dg2');
                //empty if there is any
                data_grid.datagrid('loadData', {
                    "total": 0,
                    "rows": []
                });
                var mdl_rows = [];
                var idx = 0;
                $.each(focus_fun_anot, function (k, v) {
                    mdl_rows.push({
                        pathname: k,
                        hit: fun_hit[idx],
                        pval: fun_pval[idx],
                        color: '<span id=\"function_' + idx + '\">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp</span>'
                    });
                    idx = idx + 1;
                });

                data_grid.datagrid({
                    onSelect: function (index, row) {
                        var nodeIDs = focus_fun_anot[row.pathname];
                        current_fun_nodes = nodeIDs;
                        highlightFunEnrichNodes(nodeIDs, row.pathname);
                        updateCellColor(highlightColor, "function_" + index);
                    },
                    onUnselect: function (index, row) {
                        var nodeIDs = focus_fun_anot[row.pathname];
                        current_fun_nodes = null;
                        unHighlightFunEnrichNodes(nodeIDs);
                        updateCellColor(greyColor, "function_" + index);
                    }
                }).datagrid('loadData', mdl_rows);
            });
        }
        $.messager.progress('close');
    });
}

function doEnrichmentTests(callBack) {

    var node_ids = "";
    var query = $('#queryView').val();
    var count = 0;
    if (query === "all") {
        sigInst.graph.nodes().forEach(function (n) {
            count++;
            node_ids = node_ids + '; ' + n.id;
        });
    } else if (query === "up") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.expr > 0) {
                count++;
                node_ids = node_ids + '; ' + n.id;
            }
        });
    } else if (query === "down") {
        sigInst.graph.nodes().forEach(function (n) {
            if (n.expr < 0) {
                count++;
                node_ids = node_ids + '; ' + n.id;
            }
        });
    } else { //highlighted
        var count = 0;
        sigInst.graph.nodes().forEach(function (n) {
            if (n.highlight) {
                count++;
                node_ids = node_ids + '; ' + n.id;
            }
        });
    }
    if (count === 0) {
        $.messager.alert('', 'No such nodes were found!', 'info');
    } else {
        //remove leading "; "
        node_ids = node_ids.substring(2, node_ids.length);
        var fundb = $('#enrichdb').val();
        $.ajax({
            beforeSend: function () {
                $.messager.progress({
                    text: 'Processing .....'
                });
            },
            dataType: "html",
            type: "POST",
            url: 'MetaboAnalyst/faces/AjaxCall',
            data: {function: 'networkEnrichment', IDs: node_ids, funDB: fundb},
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
}

function export2Image() {
    // Retrieving a dataUrl of the rendered graph
    var dataUrl = sigInst.renderers[0].snapshot({format: 'png', background: backgroundColor, filename: 'network-graph.png'});
    return dataUrl;
}

function performSetOperation() {
    var ids = [];
    var rows = $('#dg').datagrid('getSelections');
    for (var i = 0; i < rows.length; i++) {
        ids.push(rows[i].ID);
    }
    if (ids.length === 0) {
        $.messager.alert('', "Error: please first select nodes using the check boxes.", 'error');
        return;
    }

    var neighbors = {};
    sigInst.graph.edges().forEach(function (e) {
        if (ids.indexOf(e.source) !== -1) { //node is the source
            if (e.target in neighbors) {
                neighbors[e.target] = neighbors[e.target] + 1;
            } else {
                neighbors[e.target] = 1;
            }
        } else if (ids.indexOf(e.target) !== -1) { // node is the target
            if (e.source in neighbors) {
                neighbors[e.source] = neighbors[e.source] + 1;
            } else {
                neighbors[e.source] = 1;
            }
        }
    });

    var operation = $('#setOpt').val();
    var nds = [];
    if (operation === "union") {
        for (var key in neighbors) {
            nds.push(key);
        }
    } else {
        var len = ids.length;
        for (var key in neighbors) {
            if (neighbors[key] === len) {
                nds.push(key);
            }
        }
    }
    if (nds.length === 0) {
        $.messager.alert('', "Error: no nodes meet the requirement.", 'error');
        return;
    }

    //now add the query nodes
    nds = nds.concat(ids);

    var edgeCol = greyColorW;
    if (backgroundColor === "#222222") {
        edgeCol = greyColorB;
    }
    sigInst.settings({
        maxEdgeSize: 0.8
    });

    sigInst.graph.edges().forEach(function (e) {
        if (nds.indexOf(e.source) !== -1 && nds.indexOf(e.target) !== -1) {
            e.color = highlightColor;
            e.size = 0.8;
            e.highlight = 1;
        } else {
            e.color = edgeCol;
            e.size = defaultEdgeSize;
            e.highlight = false;
        }
    });
    highlightNodes(nds, 1);
}


function performSetOperation_orig() {
    doSetOperation(function (result) {
        if (result.substring(0, 5) !== "error") {
            var edgeCol = greyColorW;
            if (backgroundColor === "#222222") {
                edgeCol = greyColorB;
            }
            sigInst.settings({
                maxEdgeSize: 0.8
            });
            var nds = result.split("||");
            sigInst.graph.edges().forEach(function (e) {
                if (nds.indexOf(e.source) !== -1 && nds.indexOf(e.target) !== -1) {
                    e.color = highlightColor;
                    e.size = 0.8;
                    e.highlight = 1;
                } else {
                    e.color = edgeCol;
                    e.size = defaultEdgeSize;
                    e.highlight = false;
                }
            });
            highlightNodes(nds, 1);
        } else {
            var msg = result.split("||")[1];
            $.messager.alert('', "Error: " + msg, 'error');
        }
        $.messager.progress('close');
    });
}

function doSetOperation(callBack) {
    var ids = [];
    var rows = $('#dg').datagrid('getSelections');
    var operation = $('#setOpt').val();
    for (var i = 0; i < rows.length; i++) {
        ids.push(rows[i].ID);
    }

    if (ids.length === 0) {
        $.messager.alert('', "Error: please first select mir IDs using the check boxes.", 'error');
        return;
    }

    //intersect_names = ids;
    $.ajax({
        beforeSend: function () {
            $.messager.progress({
                text: 'Processing .....'
            });
        },
        dataType: "html",
        type: "GET",
        url: 'MetaboAnalyst/faces/AjaxCall',
        data: 'function=performSetOperation' + "&IDs=" + ids.join(";") + "&operation=" + operation,
        async: true,
        cache: false,
        success: function (result) {
            return callBack(result);
        },
        error: function () {
            $.messager.alert('', "Error: failed to process the request!", 'error');
            $.messager.progress('close');
        }
    });
}

function deleteNodes() {
    doNodeFilter(function (result) {
        if (result.substring(0, 5) === "error") {
            var msg = result.split("||")[1];
            $.messager.alert('Error', msg, 'error');
        } else {
            $.getJSON(usr_dir + '/' + result, function (data) {
                orphans = data.deletes.split("||");
                for (var i = 0; i < orphans.length; i++) {
                    sigInst.graph.dropNode(orphans[i]);
                }
                sigInst.refresh();
                sigma.misc.animation.camera(sigInst.camera, {
                    x: 0,
                    y: 0,
                    ratio: 1.0
                }, {duration: 200});

                var node_table = [];
                for (var i = 0; i < data.nodes.length; i++) {
                    var nd = data.nodes[i];
                    node_table.push({
                        ID: nd.id,
                        Label: nd.label,
                        Degree: nd.degree,
                        Betweenness: nd.between,
                        Status: nd.expr
                    });
                }
                node_rows = node_table;
                node_rows.sort(function (a, b) {
                    return b.Degree - a.Degree;
                });
                var dg = $('#dg');
                dg.datagrid('clearSelections');
                dg.datagrid('clearChecked');
                dg.datagrid('loadData', node_rows);
                $.messager.progress('close');
                $.messager.alert('Success', "You may need to re-perform layout to achieve better view", 'OK');
            });

        }
    });
}

function doNodeFilter(callBack) {
    var ids = [];
    var rows = $('#dg').datagrid('getSelections');

    for (var i = 0; i < rows.length; i++) {
        ids.push(rows[i].ID);
    }
    if (ids.length === 0) {
        $.messager.alert('Information', 'Please first select nodes using the check boxes.', 'info');
        return;
    } else {
        $.messager.confirm('Delete Confirmation',
                "Are you sure to delete these nodes? Note the resulting isolated nodes will also be removed.",
                function (r) {
                    if (r) {
                        //delete from R igraph
                        $.ajax({
                            beforeSend: function () {
                                $.messager.progress({
                                    text: 'Processing .....'
                                });
                            },
                            dataType: "html",
                            type: "GET",
                            url: 'MetaboAnalyst/faces/AjaxCall',
                            data: 'function=performNodesFilter' + "&nodes=" + ids.join(";"),
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
                });
    }
}