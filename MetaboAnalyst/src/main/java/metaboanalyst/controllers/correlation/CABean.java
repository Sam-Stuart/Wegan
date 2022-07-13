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
        CAUtils.PlotLinearCA(sb, false, "NULL", "NULL", false,
                 false, false, false, " "," "," ",
                sb.getCurrentImage("corr_linear"), "png", 72);
        
        CAUtils.PlotLinearPredictCA(sb, false, "NULL", "NULL", false,
                false, false, false, " ", " "," ",
                sb.getCurrentImage("corr_linear_pred"), "png", 72);
        
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
        CAUtils.CreatePenalizedModel(sb, "NULL", "NULL", false);
        CAUtils.PlotPenalizedCA(sb, sb.getCurrentImage("corr_penalized"), "png", 72);
        CAUtils.PlotPenalizedCVCA(sb, sb.getCurrentImage("corr_penalized2"), "png", 72);
    }

    private void doDefaultPolynomial() {
        CAUtils.CreatePolynomialModel(sb, "NULL", "NULL");
        CAUtils.PlotPolynomialCA(sb, "NULL", sb.getCurrentImage("corr_poly"), "png", 72);
        CAUtils.PlotPolynomialPredictCA(sb, 2, sb.getCurrentImage("corr_poly_pred"), "png", 72);
    }

    private void doDefaultMultivariate() {
        CAUtils.CreateMultivariateModel(sb);
        CAUtils.PlotMultivariateCA(sb, sb.getCurrentImage("corr_multivariate"), "png", 72);
        CAUtils.PlotMultivariateCoeffCA(sb, sb.getCurrentImage("corr_multivariate_coeff"), "png", 72);
        CAUtils.PlotMultivariateRelativeCA(sb, sb.getCurrentImage("corr_multivariate_relative"), "png", 72);
    }


    private void doDefaultSVM() {
        CAUtils.CreateSVMModel(sb, "NULL", "NULL");
        CAUtils.PlotSVMCA(sb, sb.getCurrentImage("corr_svm"), "png", 72);
    }    

    private void doDefaultRF() {
        CAUtils.CreateRFModel(sb, "NULL", "NULL");
        CAUtils.PlotRFCA(sb, sb.getCurrentImage("corr_rf"), "png", 72);
        CAUtils.PlotRFRelativeCA(sb, sb.getCurrentImage("corr_rf_relative"), "png", 72);
        CAUtils.PlotRFErrorCA(sb, sb.getCurrentImage("corr_rf_error"), "png", 72);
    }  
    
    private void doDefaultLogistic() {
        CAUtils.CreateLogisticModel(sb, "NULL", "NULL");
        CAUtils.PlotLogisticEffectCA(sb, "multinomial", sb.getCurrentImage("corr_logistic1"), "png", 72);
        CAUtils.PlotLogisticROCCA(sb, "multinomial", sb.getCurrentImage("corr_logisticROC"), "png", 72);
    } 
    
}
