/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers;

import java.io.File;
import java.io.Serializable;
import java.text.DecimalFormat;
import java.util.ArrayList;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.model.ListDataModel;
import metaboanalyst.models.ColumnBean;
import metaboanalyst.models.FeatureBean;
import metaboanalyst.rwrappers.ChemoMetrics;
import metaboanalyst.rwrappers.DispersalUtils;
import metaboanalyst.rwrappers.Classifying;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.SigVarSelect;
import metaboanalyst.rwrappers.TimeSeries;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.controllers.dispersal.BGDBean;
import metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;


/**
 *
 * @author jianguox
 */
@ManagedBean(name = "detailsBean")
@ViewScoped
public class DetailsBean implements Serializable {

    private String[] colnames = null;
    private String downloadTxt = "";
    private ListDataModel<FeatureBean> listModel = null;
    private boolean init = false;
    private boolean extraVis = false;

    //for detail view table
    private ColumnBean[] colVis = new ColumnBean[8];

    public boolean isColVisible(int inx) {
        return colVis[inx].isVisible();
    }

    public String getColHeader(int inx) {
        return colVis[inx].getName();
    }

    public String getDownloadTxt() {
        return downloadTxt;
    }

    public ListDataModel<FeatureBean> getFeatureBeans() {
        return listModel;
    }

    public boolean isExtraColVisible() {
        return extraVis;
    }

    public void setupDetailsTable() {
        if (init) {
            return; //important, somehow called even in ajax 
        }
        colnames = null;
        SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
        
        String from = sb.getSigSource();
        System.out.print("SetupDetailsTable Testing");
        System.out.print(from);
        extraVis = from.equals("anova");
        String[] rownames = null;
        double[][] sigmat = null;
        String[] stringCol = null;
        String fileName = "";
        
        if (from.equals("fc")) {
            rownames = UniVarTests.GetFCSigRowNames(sb);
            colnames = UniVarTests.GetFCSigColNames(sb);
            sigmat = UniVarTests.GetFCSigMat(sb);
            fileName = "fold_change.csv";
        } else if (from.equals("tt")) {
            rownames = UniVarTests.GetTTSigRowNames(sb);
            colnames = UniVarTests.GetTTSigColNames(sb);
            sigmat = UniVarTests.GetTTSigMat(sb);
            fileName = UniVarTests.GetTtestSigFileName(sb);
        } else if (from.equals("bgd")||from.equals("bgd1")||from.equals("bgd2")||from.equals("bgd3")||from.equals("bgd4")) {
            rownames = BGDBean.GetBGDSigRowNames(sb, from);
            colnames = BGDBean.GetBGDSigColNames(sb, from);
            sigmat = BGDBean.GetBGDSigMat(sb, from);
//            fileName = Dispersal.GetBGDSigFileName(sb);
            fileName= "test_fileName.csv";
        } else if (from.equals("volcano")) {
            rownames = UniVarTests.GetVolcanoSigRowNames(sb);
            colnames = UniVarTests.GetVolcanoSigColNames(sb);
            sigmat = UniVarTests.GetVolcanoSigMat(sb);
            fileName = "volcano.csv";
        } else if (from.equals("anova")) {
            rownames = UniVarTests.GetAovSigRowNames(sb);
            colnames = UniVarTests.GetAovSigColNames(sb);
            stringCol = UniVarTests.GetAovPostHocSigNames(sb);
            sigmat = UniVarTests.GetAovSigMat(sb);
            fileName = UniVarTests.GetAnovaSigFileName(sb);;
        } else if (from.equals("template")) {
            rownames = UniVarTests.GetCorSigRowNames(sb);
            colnames = UniVarTests.GetCorSigColNames(sb);
            sigmat = UniVarTests.GetCorSigMat(sb);
            fileName = UniVarTests.GetCorrSigFileName(sb);
        } else if (from.equals("ebam")) {
            sigmat = SigVarSelect.GetEBAMSigMat(sb);
            rownames = SigVarSelect.GetEBAMSigRowNames(sb);
            colnames = SigVarSelect.GetEBAMSigColNames(sb);
            fileName = "ebam_sigfeatures.csv";
        } else if (from.equals("sam")) {
            sigmat = SigVarSelect.GetSAMSigMat(sb);
            rownames = SigVarSelect.GetSAMSigRowNames(sb);
            colnames = SigVarSelect.GetSAMSigColNames(sb);
            fileName = "sam_sigfeatures.csv";
        } else if (from.equals("pca")) {
            sigmat = ChemoMetrics.GetPCALoadingScore(sb);
            rownames = ChemoMetrics.GetPCALoadingRowName(sb);
            colnames = ChemoMetrics.GetPCALoadingColName(sb);
            fileName = "pca_loadings.csv";
        } else if (from.startsWith("pls")) {
            String spec = from.split("\\.")[1];
            sigmat = ChemoMetrics.GetPLSSigMat(sb, spec);
            rownames = ChemoMetrics.GetPLSSigRowNames(sb, spec);
            colnames = ChemoMetrics.GetPLSSigColNames(sb, spec);
            fileName = "plsda_" + spec + ".csv";
        } else if (from.startsWith("opls")) {
            String spec = from.split("\\.")[1];
            RConnection RC = sb.getRConnection();
            sigmat = RDataUtils.getOPLSLoadMat(RC);
            rownames = RDataUtils.getOPLSLoadCmpds(RC);
            colnames = RDataUtils.getOPLSLoadColNames(RC);
            fileName = "oplsda_" + spec + ".csv";
        } else if (from.startsWith("spls")) {
            String spec = from.split("\\.")[1];
            RConnection RC = sb.getRConnection();
            sigmat = RDataUtils.getSPLSLoadMat(RC);
            rownames = RDataUtils.getSPLSLoadCmpds(RC);
            colnames = ChemoMetrics.GetSPLSSigColNames(sb, spec);
            fileName = "splsda_" + spec + ".csv";
        } else if (from.equals("rf")) {
            sigmat = Classifying.GetRFSigMat(sb);
            rownames = Classifying.GetRFSigRowNames(sb);
            colnames = Classifying.GetRFSigColNames(sb);
            fileName = "randomforests_sigfeatures.csv";
        } else if (from.equals("svm")) {
            sigmat = Classifying.GetSVMSigMat(sb);
            rownames = Classifying.GetSVMSigRowNames(sb);
            colnames = Classifying.GetSVMSigColNames(sb);
            fileName = "svm_sigfeatures.csv";
        } else if (from.equals("anova2")) {
            rownames = TimeSeries.GetAov2SigRowNames(sb.getRConnection());
            colnames = TimeSeries.GetAov2SigColNames(sb.getRConnection());
            sigmat = TimeSeries.GetAov2SigMat(sb.getRConnection());
            fileName = TimeSeries.GetAov2SigFileName(sb);
        } else if (from.startsWith("asca")) {
            String spec = from.split("\\.")[1];
            rownames = TimeSeries.GetAscaSigRowNames(sb, spec);
            colnames = TimeSeries.GetAscaSigColNames(sb, spec);
            sigmat = TimeSeries.GetAscaSigMat(sb, spec);
            fileName = TimeSeries.GetAscaSigFileName(sb);
        } else {
            //do nothing
            
        }
        //set up the view
        if (colnames == null || colnames.length == 0) {
            sb.updateMsg("Error", "No results are available!");
            return;
        }
        colVis = new ColumnBean[8];
        for (int i = 0; i < colVis.length; i++) {
            if (i < colnames.length) {
                colVis[i] = new ColumnBean(true, colnames[i]);
            } else {
                colVis[i] = new ColumnBean(false, "");
            }
        }

        ArrayList<FeatureBean> featureBeans = null;
        //set up content
        if (rownames != null && rownames.length > 0) {
            // errTxt.setText("No result data was found!");
            //sb.setFeatureBeans(null);
            //return null;
            featureBeans = new ArrayList();
            FeatureBean fb = null;

            //This will make value pretty, but not meaningful for column sorting
            DecimalFormat formatter = new DecimalFormat("0.#####E0");

            for (int i = 0; i < rownames.length; i++) {
                fb = new FeatureBean();
                fb.addName(rownames[i]);

                for (int m = 0; m < colnames.length; m++) {
                    fb.addValue(Double.parseDouble(formatter.format(sigmat[i][m])));
                }
                if (stringCol != null) {
                    fb.addExtra(stringCol[i]);
                }
                featureBeans.add(fb);
            }
            listModel = new ListDataModel(featureBeans);
            downloadTxt = "<b>You can download the table:</b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                    + File.separator + fileName + "\"><b>" + "here" + "</b></a>" + " (Right click and \"Save Link as....\")";
        } else {
            downloadTxt = "";
        }
        init = true;
    }

}
