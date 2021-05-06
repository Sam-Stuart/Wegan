function showPCA3D() {
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value,
            function (raw_data) {
                score_obj = raw_data['score'];
                names = score_obj['name'];
                coords = score_obj['xyz'];
                smps = score_obj['axis'];
                facA = score_obj['facA'];
                facB = score_obj['facB'];
                cols = score_obj['colors'];
                // saps = score_obj['shapes'];
                cx1 = new CanvasXpress(
                        'canvas1',
                        {
                            'z': {
                                'Color Legend': facA,
                                'Shape Legend': facB
                            },
                            'y': {
                                'vars': names,
                                'smps': smps,
                                'data': coords
                            }
                        },
                        {
                            'graphType': 'Scatter3D',
                            'xAxis': ['PC1'],
                            'yAxis': ['PC2'],
                            'zAxis': ['PC3'],
                            'colorSCheme': 'user',
                            'shapeBy': 'Shape Legend',
//                            'shapes': ['image'],
//                            'image': ['../../resources/images/icons/ca_icon.png']
                            'colorBy': 'Color Legend',
                            //'colors': cols,
                            'disableToolbar': 'true',
                            'disableMenu': 'true',
                            'gradient': 'true',
                            'xAxisMinorTicks': 'false',
                            'yAxisMinorTicks': 'false',
                            'zAxisMinorTicks': 'false',
                            'imageDir': '../../resources/images/'
                        }
                );

                var loadings_obj = raw_data['loadings'];
                var ld_names = loadings_obj['name'];
                var ld_coords = loadings_obj['xyz'];
                var ld_smps = loadings_obj['axis'];
                var cx2 = new CanvasXpress(
                        'canvas2',
                        {
                            'y': {
                                'vars': ld_names,
                                'smps': ld_smps,
                                'data': ld_coords
                            }
                        },
                        {
                            'graphType': 'Scatter3D',
                            'xAxis': ['Loadings 1'],
                            'yAxis': ['Loadings 2'],
                            'zAxis': ['Loadings 3'],
                            'colors': ['rgba(0,153,255,0.6)'],
//                            'shapes': ['image'],
//                            'image': ['../../resources/images/icons/ca_icon.png'],
                            'disableToolbar': 'true',
                            'disableMenu': 'true',
                            'gradient': 'true',
                            'imageDir': '../../resources/images/' 
                        }
                );
            });
}
;

function updatePCAColorScheme() {
    //var arrElements = document.getElementById("colRadio");
    var mycol, mysap;
    if (PF('colRadio').inputs[0].checked) {
        mycol = facA;
        mysap = facB;
    } else {
        mycol = facB;
        mysap = facA;
    }
    var data = {
        'z': {
            'Shape Legend': mysap,
            'Color Legend': mycol
        },
        'y': {
            'vars': names,
            'smps': smps,
            'data': coords
        }
    };
    cx1.updateData(data);
}

function showPCA3DScore() {
    console.log("INSIDE SHOW PCA3D SCORES")
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value,
            function (raw_data) {
//                score_obj = raw_data['score'];
//                names = score_obj['name'];
//                coords = score_obj['xyz'];
//                smps = score_obj['axis'];
//                cols = score_obj['colors'];
//                facA = score_obj['facA'];
//                cx1 = new CanvasXpress(
//                        'canvas1',
//                        {
//                            'z': {
//                                'Legend': facA
//                            },
//                            'y': {
//                                'vars': names,
//                                'smps': smps,
//                                'data': coords
//                            }
//                        },
//                        {
//                            'graphType': 'Scatter3D',
//                            'xAxis': ['PC1'],
//                            'yAxis': ['PC2'],
//                            'zAxis': ['PC3'],
//                            'colorSCheme': 'user',
//                            'colorBy': 'Legend',
//                            'colors': cols,
//                            'disableToolbar': 'true',
//                            'disableMenu': 'true',
//                            'gradient': 'true',
//                            'imageDir': '../../resources/images/'
//                        }
//                );
//                var loadings_obj = raw_data['loadings'];
//                var ld_names = loadings_obj['name'];
//                var ld_coords = loadings_obj['xyz'];
//                var ld_smps = loadings_obj['axis'];
//                var cx2 = new CanvasXpress(
//                        'canvas2',
//                        {
//                            'y': {
//                                'vars': ld_names,
//                                'smps': ld_smps,
//                                'data': ld_coords
//                            }
//                        },
//                        {
//                            'graphType': 'Scatter3D',
//                            'xAxis': ['Loadings 1'],
//                            'yAxis': ['Loadings 2'],
//                            'zAxis': ['Loadings 3'],
//                            'colors': ['rgba(0,153,255,0.6)'],
////                            'shapes': ['image'],
////                            'image': ['../../resources/images/icons/ca_icon.png'],
//                            'disableToolbar': 'true',
//                            'disableMenu': 'true',
//                            'gradient': 'true',
//                            'imageDir': '../../resources/images/' 
//                        }
//                );
                var cx3 = new CanvasXpress(
                        "canvas3", 
                        {
                            "y": {
                                "smps": ["Alcohol","Tobacco"],
                                "vars": ["North","Yorkshire","Northeast","East Midlands","West Midlands","East Anglia","Southeast","Southwest","Wales","Scotland","Northern Ireland"],
                                "data": [
//                                        [6.47,4.03],
//                                        [6.13,3.76],
//                                        [6.19,3.77],
//                                        [4.89,3.34],
//                                        [5.63,3.47],
//                                        [4.52,2.92],
//                                        [5.89,3.2],
//                                        [4.79,2.71],
//                                        [3.53,3.53],
//                                        [6.08,4.51],
                                        [4.02,4.56]
                                      ]
                            },
                            "x": {
                                     "Description": ["Average weekly household spending on alcoholic beverages in pounds","Average weekly household spending on tobacco products in pounds"]
                                }
                        }, {
                            "citation": "Moore, David S., and George P. McCabe (1989). Introduction to the Practice of Statistics, p. 179.",
                            "colorSpectrum": ["rgb(69,117,180)","rgb(145,191,219)","rgb(224,243,248)","rgb(255,255,191)","rgb(254,224,144)","rgb(252,141,89)","rgb(215,48,39)"],
                            "dataTableStyle": "stripped",
                            "decorations": {
                                "marker": [
                                    {
                                      "text": "Maybe an Outlier?",
                                      "sample": ["Alcohol","Tobacco"],
                                      "variable": ["Northern Ireland"],
                                      "x": 0.45,
                                      "y": 0.18,
//                                      "id": "scatter2d1-marker-0",
                                      "vi": [10],
                                      "si": [0,1],
                                      "type": "line",
                                      "b": [77.4943359375,58.2,513.50849609375,481.5],
                                      "len": 122.5634765625,
                                      "width": 15,
                                      "tx": false,
                                      "ty": false,
                                      "curX": 305.86191650390623,
                                      "curY": 144.87
                                    }
                                ]
                            },
                            "graphType": "Scatter2D",
                            "mapGraticuleType": "solid",
                            "setMaxY": 3.573160455986881,
                            "setMinY": 3.338655241625281,
                            "shapes": [['/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/src/main/webapp/resources/images/icons/ca_icon.png']],
                            "images": ['/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/src/main/webapp/resources/images/icons/ca_icon.png'],
                            "theme": "CanvasXpress",
                            "title": "Average weekly household spending, in British pounds, on tobacco productsand alcoholic beverages for each of the 11 regions of Great Britain.",
                            "xAxis": ["Alcohol"],
                            "xAxisTitle": "Alcohol",
                            "yAxis": ["Tobacco"],
                            "yAxisTitle": "Tobacco",
                            'imageDir': '../../resources/images/' 
                        });
                        //cx3.drawShape('/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/src/main/webapp/resources/images/icons/ca_icon.png', 6.47,4.03)
            });
}

function showPLSDA3DScore() {
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value,
            function (raw_data) {
                score_obj = raw_data['score'];
                names = score_obj['name'];
                coords = score_obj['xyz'];
                smps = score_obj['axis'];
                cols = score_obj['colors'];
                facA = score_obj['facA'];
                cx1 = new CanvasXpress(
                        'canvas1',
                        {
                            'z': {
                                'Legend': facA
                            },
                            'y': {
                                'vars': names,
                                'smps': smps,
                                'data': coords
                            }
                        },
                        {
                            'graphType': 'Scatter3D',
                            'xAxis': ['LV1'],
                            'yAxis': ['LV2'],
                            'zAxis': ['LV3'],
                            'colorBy': 'Legend',
                            'colors': cols,
                            'disableToolbar': 'true',
                            'disableMenu': 'true',
                            'gradient': 'true',
                            'imageDir': '../../resources/images/'
                        }
                );
            });
}

function showBray3DScore() {
    console.log("INSIDE SHOW BRAY3D SCORES")
    $.getJSON('/MetaboAnalyst' + document.getElementById("mydir").value,
            function (raw_data) {
                score_obj = raw_data['score'];
                names = score_obj['name'];
                coords = score_obj['xyz'];
                smps = score_obj['axis'];
//                cols = score_obj['colors'];
                facA = score_obj['facA'];
                cx1 = new CanvasXpress(
                        'canvasBray',
                        {
                            'z': {
                                'Legend': facA
                            },
                            'y': {
                                'vars': names,
                                'smps': smps,
                                'data': coords
                            }
                        },
                        {
                            'graphType': 'Scatter3D',
                            'xAxis': ['PC1'],
                            'yAxis': ['PC2'],
                            'zAxis': ['PC3'],
                            'colorSCheme': 'user',
                            'colorBy': 'Legend',
//                            'colors': cols,
                            'disableToolbar': 'true',
                            'disableMenu': 'true',
                            'gradient': 'true',
                            'imageDir': '../../resources/images/'
                        }
                );

            });
}


function export_image(name) {
    if (name === 'score') {
        document.getElementById("downloadimage").src = document.getElementById("canvas1").toDataURL();
    } else {
        document.getElementById("downloadimage").src = document.getElementById("canvas2").toDataURL();
    }
    PF('exportDialog').show();
}

