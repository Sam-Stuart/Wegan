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
                    case "random forest":
                        doDefaultML(pageID);
                        break;
                }
            }

        }
    }
//weights <- .readDataTable("/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/target/MetaboAnalyst-4.34/resources/data/dune_weights.csv")
//  cat(paste0(mSetObj$dataSet$cmpd))

    private void doDefaultCorrelation() {
        System.out.println("HELLO RIGHT BEFORE PLOT Corr");
        UniVarTests.PlotCorrHeatMap(sb, sb.getCurrentImage("corr"), "png", 72, "col", "pearson", "bwm", "overview", "F", "F", "F", 100);
    }

    private void doDefaultLinear() {
        
        if (!CAUtils.CreateLinearModel(sb, "NULL", "NULL")){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC)); 
        }
        CAUtils.PlotLinearCA(sb, sb.getCurrentImage("corr_linear"), "png", 72);
    }

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
        // Don't need the plot  anymore 
        CAUtils.PlotMultivariateCA(sb, sb.getCurrentImage("corr_multivariate"), "png", 72);
    }

    private void doDefaultML(String modelType) {
        CAUtils.CreateMLModel(sb, modelType);
        if (modelType == "SVM") {
            CAUtils.PlotMLCA(sb, "NULL", sb.getCurrentImage("corr_svm"), "png", 72);
        } else {
            CAUtils.PlotMLCA(sb, "NULL", sb.getCurrentImage("corr_rf"), "png", 72);
        }

    }

    private void doDefaultSVM() {
        CAUtils.CreateSVMModel(sb);
        CAUtils.PlotSVMCA(sb, sb.getCurrentImage("corr_svm"), "png", 72);
    }    

    
}
