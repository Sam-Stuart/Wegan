/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.correlation;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.CAUtils;
import metaboanalyst.rwrappers.RDataUtils;

import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "caBean")
public class CABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        if (!sb.isAnalInit(pageID)) {
            if (!FacesContext.getCurrentInstance().isPostback()) {
                //sb.registerPage(pageID);
                switch (pageID) {
                    case "Correlations":
                        doDefaultCorrelation();
                        break;
                    case "Linear":
                        doDefaultLinear();
                        break;
                    case "Penalized":
                        doDefaultPenalized();
                        break;
                    case "Polynomial":
                        doDefaultPolynomial();
                        break;
                    case "Multivariate":
                        doDefaultMultivariate();
                        break;
                    case "SVM":
                        doDefaultSVM();
                        break;
                    case "RF":
                        doDefaultRF();
                        break;
                    case "ANN":
                        doDefaultANN();
                        break;
                    case "Logistic":
                        doDefaultLogistic();
                        break;
                }
            }

        }
    }
//weights <- .readDataTable("/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/target/MetaboAnalyst-4.34/resources/data/dune_weights.csv")
//  cat(paste0(mSetObj$dataSet$cmpd))

    private void doDefaultCorrelation() {
        UniVarTests.PlotCorrHeatMap(sb, sb.getCurrentImage("corr"), "png", 72, "col", "pearson", "bwm", "overview", "F", "F", "F", 100);
    }

    private void doDefaultLinear() {
        
        if (!CAUtils.CreateLinearModel(sb, "NULL", "NULL", false)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC)); 
        }
        CAUtils.PlotLinearCA(sb,  "NULL", "NULL",
                false, "NULL", "NULL", false,
                 false, false, false, " "," "," ",
                sb.getCurrentImage("corr_linear"), "png", 72);
        
        CAUtils.PlotLinearPredictCA(sb, "NULL", "NULL",
                false, 
                "NULL", "NULL", false,
                false, false, false, " ", " "," ",
                sb.getCurrentImage("corr_linear_pred"), "png", 72);
        
        CAUtils.PlotLinearNormResidCA(sb, "NULL", "NULL", false,
                "NULL", "NULL", " ", " "," ",
                sb.getCurrentImage("corr_linear_normres"), "png", 72);
        
        CAUtils.PlotLinearResidFitCA(sb, "NULL", "NULL", false,
                "NULL", "NULL", " ", " "," ",
                sb.getCurrentImage("corr_linear_resfit"), "png", 72);
//        CAUtils.ConvertLinearJSONCA(sb, "NULL");
        
    }
    
//    private void doDefaultLinear() {
//      
//        CAUtils.PlotLinearCA(sb, 
////                "NULL", "NULL", 
//                false, "NULL", "NULL", false, 
//                false, false, false, "NULL","NULL","NULL",
////                sb.getCurrentImage("corr_linear"));
//        sb.getCurrentImage("corr_linear"), "png", 72);
//    }
                       
     private void doDefaultPenalized() {
       CAUtils.CreatePenalizedModel(sb, "NULL", "NULL");
        CAUtils.PlotPenalizedCA(sb, "NULL", "NULL",
                "NULL","NULL",
                 " "," "," ",
                 sb.getCurrentImage("corr_penalized"), "png", 72);
        CAUtils.PlotPenalizedCVCA(sb,"NULL", "NULL",
                "NULL", "NULL",
                " "," "," ",
                 sb.getCurrentImage("corr_penalized2"), "png", 72);
     }


    private void doDefaultPolynomial() {
        CAUtils.CreatePolynomialModel(sb, "NULL", "NULL", false);
        CAUtils.PlotPolynomialCA(sb, "NULL","NULL", "NULL",
                false, "NULL", "NULL", false,
                 false, false, false, " "," "," ",
                sb.getCurrentImage("corr_poly"), "png", 72);
        CAUtils.PlotPolynomialPredictCA(sb,  "NULL",
                "NULL", "NULL", false, 
                "NULL", "NULL", false, 
                " "," "," ",
                sb.getCurrentImage("corr_poly_pred"), "png", 72);
    }
    
    private void doDefaultMultivariate() {
        CAUtils.CreateMultivariateModel(sb, "NULL", "NULL", false);
        CAUtils.PlotMultivariateCA(sb, 
                "NULL", "NULL", false, 
                "NULL","NULL",  false,
                 " ", " "," ",
                sb.getCurrentImage("corr_multi_pred"), "png", 72);
        CAUtils.PlotMultivariateCoeffCA(sb, 
                 "NULL", "NULL", false, 
                "NULL",  false,
                 " ", " "," ",
                sb.getCurrentImage("corr_multi_relaimpo"), "png", 72);
        CAUtils.PlotMultivariateRelativeCA(sb, 
                 "NULL", "NULL", false, 
                "NULL",  false,
                 " ", " "," ",
                sb.getCurrentImage("corr_multi_relaimpo"), "png", 72);
    }


    private void doDefaultSVM() {
        CAUtils.CreateSVMModel(sb, "NULL", "NULL");
        CAUtils.PlotSVMCA(sb,
                "NULL", "NULL",
                "NULL","NULL",
                "NULL", "NULL",
                 " ", " "," ",
                sb.getCurrentImage("corr_svm_pred"), "png", 72);
    }    

    private void doDefaultRF() {
        CAUtils.CreateRFModel(sb, "NULL", "", false);
        CAUtils.PlotRFCA(sb,
                 "NULL", "", false,
                "NULL","NULL",  false,
                 " ", " "," ",
                sb.getCurrentImage("corr_rf_pred"), "png", 72);
        CAUtils.PlotRFRelativeCA(sb, 
                "NULL", "", false, 
                "NULL",  false,
                 " ", " "," ",
                sb.getCurrentImage("corr_rf_relaimpo"), "png", 72);
        CAUtils.PlotRFErrorCA(sb, 
                "NULL", "", false, 
                "NULL",  " ", " "," ",
                sb.getCurrentImage("corr_rf_error"), "png", 72);
    }  
    
    
    private void doDefaultANN() {
        CAUtils.CreateANNModel(sb, "NULL","", "");
        CAUtils.PlotANNCA(sb,
                "NULL", "","","NULL",
                "NULL","NULL","NULL"," ", 
                 sb.getCurrentImage("corr_ann_nid"), "png", 72);       
   
        CAUtils.PlotANNPredictCA(sb,
                "NULL", "", "", 
                "NULL", "NULL", 
                "NULL"," ", " ", " ", 
                sb.getCurrentImage("corr_ann_pred"), "png", 72);
}
    
    private void doDefaultLogistic() {
        CAUtils.CreateLogisticModel(sb, 
                "NULL", "NULL", false,"NULL", "NULL", "NULL");
//                "NULL", "NULL");
        CAUtils.PlotLogisticEffectCA(sb, false,
                "NULL", false, 
//                "NULL", "NULL", false, 
                " ", " ", " ",
                false,"NULL", false,
                "NULL",
                sb.getCurrentImage("corr_log_eff"), "png", 72);
        CAUtils.PlotLogisticROCCA(sb, false,
                "NULL", "NULL", " ",
                sb.getCurrentImage("corr_log_roc"), "png", 72);
    } 
    
}
