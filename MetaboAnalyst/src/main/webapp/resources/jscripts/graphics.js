//Graphics done though the CanvasXpress Library

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
                            //       'shapes': saps,
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
                            'xAxis': ['PC1'],
                            'yAxis': ['PC2'],
                            'zAxis': ['PC3'],
                            'colorSCheme': 'user',
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

function export_image(name) {
    if (name === 'score') {
        document.getElementById("downloadimage").src = document.getElementById("canvas1").toDataURL();
    } else {
        document.getElementById("downloadimage").src = document.getElementById("canvas2").toDataURL();
    }
    PF('exportDialog').show();
}

