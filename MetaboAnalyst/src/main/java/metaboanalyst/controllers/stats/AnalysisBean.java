/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.ChemoMetrics;
import metaboanalyst.rwrappers.Classifying;
import metaboanalyst.rwrappers.Clustering;
import metaboanalyst.rwrappers.RCenter;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.SigVarSelect;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.utils.DataUtils;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "analBean")
public class AnalysisBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        if (!sb.isAnalInit(pageID)) {
            sb.registerPage(pageID);
            switch (pageID) {
                case "PCA":
                    doDefaultPCA();
                    break;
                case "ANOVA":
                    doDefaultANOVA();
                    break;
                case "Fold change":
                    doDefaultFC();
                    break;
                case "T-test":
                    doDefaultTT();
                    break;
                case "Volcano plot":
                    doDefaultVC();
                    break;
                case "Correlations":
                    doDefaultCorrelation();
                    break;
                case "PLSDA":
                    doDefaultPLSDA();
                    break;
                case "sPLSDA":
                    doDefaultSPLSDA();
                    break;
                case "OrthoPLSDA":
                    doDefaultOPLSDA();
                    break;
                case "SAM":
                    doDefaultSAM();
                    break;
                case "EBAM":
                    doDefaultEBAM();
                    break;
                case "Dendrogram":
                    doDefaultDendrogram();
                    break;
                case "Heatmap":
                    doDefaultHmClust();
                    break;
                case "K-means":
                    doDefaultKmeanClust();
                    break;
                case "SOM":
                    doDefaultSOMClust();
                    break;
                case "RandomForest":
                    doDefaultRF();
                    break;
                case "SVM":
                    doDefaultSVM();
                    break;
            }
        }
    }

    private void doDefaultANOVA() {
        int res = UniVarTests.performANOVA(sb, "F", 0.05, "fisher");
        if (res == 0) {
            sb.setAnovaSig(false);
        } else {
            sb.setAnovaSig(true);
        }
        UniVarTests.PlotAOV(sb, sb.getCurrentImage("aov"), "png", 72);
    }

    private void doDefaultFC() {
        UniVarTests.InitUnpairedFC(sb, 2, 0);
        UniVarTests.PlotFC(sb, sb.getCurrentImage("fc"), "png", 72);
    }

    private void doDefaultTT() {
        int res = UniVarTests.performTtests(sb, "F", 0.05, "FALSE", "TRUE");//default not paired and unequal variance
        if (res == 0) {
            sb.setTtSig(false);
        } else {
            sb.setTtSig(true);
        }
        UniVarTests.PlotT(sb, sb.getCurrentImage("tt"), "png", 72);
    }

    private void doDefaultVC() {
        UniVarTests.performVolcano(sb, "FALSE", 2, 0, 0.75, "F", 0.1, "TRUE", "raw");
        UniVarTests.PlotVolcano(sb, sb.getCurrentImage("volcano"), 1, "png", 72);
    }

    private void doDefaultCorrelation() {
        UniVarTests.PlotCorrHeatMap(sb, sb.getCurrentImage("corr"), "png", 72, "col", "pearson", "bwm", "overview", "F", "F", "F", 100);
    }

    private void doDefaultPCA() {
        ChemoMetrics.InitPCA(sb);
        ChemoMetrics.PlotPCAPairSummary(sb, sb.getCurrentImage("pca_pair"), "png", 72, 5);
        ChemoMetrics.PlotPCAScree(sb, sb.getCurrentImage("pca_scree"), "png", 72, 5);
        ChemoMetrics.PlotPCA2DScore(sb, sb.getCurrentImage("pca_score2d"), "png", 72, 1, 2, 0.95, 1, 0);
        ChemoMetrics.PlotPCALoading(sb, sb.getCurrentImage("pca_loading"), "png", 72, 1, 2, "scatter", 1);  // setLoadingTable(pcImpInx);
        ChemoMetrics.PlotPCABiplot(sb, sb.getCurrentImage("pca_biplot"), "png", 72, 1, 2);
        // ChemoMetrics.PlotPCA3DScore(sb, sb.getCurrentImage("pca_score3d"), "png", 72, 1, 2, 3, 40);
        ChemoMetrics.PlotPCA3DScore(sb, sb.getCurrentImage("pca_score3d"), "json", 72, 1, 2, 3);
    }

    private void doDefaultPLSDA() {
        ChemoMetrics.InitPLS(sb);
        ChemoMetrics.PlotPLSPairSummary(sb, sb.getCurrentImage("pls_pair"), "png", 72, ChemoMetrics.GetDefaultPLSPairNumber(sb));
        ChemoMetrics.PlotPLS2DScore(sb, sb.getCurrentImage("pls_score2d"), "png", 72, 1, 2, 0.95, 1, 0);
        // ChemoMetrics.PlotPLS3DScore(sb, sb.getCurrentImage("pls_score3d"), "png", 72, 1, 2, 3, 40);
        ChemoMetrics.PlotPLS3DScore(sb, sb.getCurrentImage("pls_score3d"), "json", 72, 1, 2, 3);
        ChemoMetrics.PlotPLSLoading(sb, sb.getCurrentImage("pls_loading"), "png", 72, 1, 2, "scatter", 1);

        String cvMethod = "T";
        int minSize = RDataUtils.getMinGroupSize(sb.getRConnection());
        if (minSize < 11) {
            cvMethod = "L";
        }
        ChemoMetrics.TrainPLSClassifier(sb, cvMethod, ChemoMetrics.GetDefaultPLSCVNumber(sb), "Q2");
        ChemoMetrics.PlotPLSClassification(sb, sb.getCurrentImage("pls_cv"), "png", 72);
        ChemoMetrics.PlotPLSImp(sb, sb.getCurrentImage("pls_imp"), "png", 72, "vip", "Comp. 1", 15, "FALSE");
    }

    private void doDefaultSPLSDA() {
        RCenter.loadRareFunctions(sb.getRConnection(), "spls");
        ChemoMetrics.InitSPLS(sb, 5, 10, "same");
        ChemoMetrics.PlotSPLSPairSummary(sb, sb.getCurrentImage("spls_pair"), "png", 72, ChemoMetrics.GetDefaultSPLSPairNumber(sb));
        ChemoMetrics.PlotSPLS2DScore(sb, sb.getCurrentImage("spls_score2d"), "png", 72, 1, 2, 0.95, 1, 0);
        // ChemoMetrics.PlotPLS3DScore(sb, sb.getCurrentImage("pls_score3d"), "png", 72, 1, 2, 3, 40);
        ChemoMetrics.PlotSPLS3DScore(sb, sb.getCurrentImage("spls_score3d"), "json", 72, 1, 2, 3);
        ChemoMetrics.PlotSPLSLoading(sb, sb.getCurrentImage("spls_loading"), "png", 72, 1, "overview");
        ChemoMetrics.PlotSPLSDAClassification(sb, sb.getCurrentImage("spls_cv"), "Mfold", "png", 72);
        //String cvMethod = "T";
        // int minSize = RDataUtils.getMinGroupSize(sb.getRConnection());
        //  if (minSize < 11) {
        //      cvMethod = "L";
        //  }
        // ChemoMetrics.TrainSPLSClassifier(sb, cvMethod, ChemoMetrics.GetDefaultPLSCVNumber(sb), "Q2");
        // ChemoMetrics.PlotSPLSClassification(sb, sb.getCurrentImage("spls_cv"), "png", 72);
        // ChemoMetrics.PlotSPLSImp(sb, sb.getCurrentImage("spls_imp"), "png", 72, "vip", "Comp. 1", 15, "FALSE");
    }

    private void doDefaultOPLSDA() {
        ChemoMetrics.InitOPLS(sb);
        ChemoMetrics.PlotOPLS2DScore(sb, sb.getCurrentImage("opls_score2d"), "png", 72, 1, 2, 0.95, 1, 0);
        ChemoMetrics.PlotOplsSplot(sb, sb.getCurrentImage("opls_splot"), "png", 72, "all");
        ChemoMetrics.PlotOplsMdlView(sb, sb.getCurrentImage("opls_mdl"), "png", 72);
    }

    private void doDefaultSAM() {
        SigVarSelect.InitSAM(sb, "d.stat", "FALSE", "TRUE");
        double delta = SigVarSelect.GetSAMSuggestedDelta(sb);
        SigVarSelect.PlotSAM_FDR(sb, delta, sb.getCurrentImage("sam_view"), "png", 72);
        SigVarSelect.PlotSAM_Cmpd(sb, sb.getCurrentImage("sam_imp"), "png", 72, delta);
    }

    private void doDefaultEBAM() {
        SigVarSelect.InitEBAM_A0(sb, "FALSE", "TRUE");
        SigVarSelect.PlotEBAM_A0(sb, sb.getCurrentImage("ebam_view"), "png", 72);
        double a0 = SigVarSelect.GetEBAMSuggestedA0(sb);
        SigVarSelect.InitEBAM_Cmpd(sb, "z.ebam", a0, "FALSE", "TRUE");
        SigVarSelect.PlotEBAM_Cmpd(sb, sb.getCurrentImage("ebam_imp"), "png", 72, 0.9);
    }

    private void doDefaultDendrogram() {
        Clustering.PlotClustTree(sb, sb.getCurrentImage("tree"), "png", 72, "euclidean", "ward.D");
    }

    private void doDefaultHmClust() {
        Clustering.PlotHeatMap(sb, sb.getCurrentImage("heatmap"), "png", 72, "norm", "row", "euclidean", "ward.D", "bwm", "overview", "T", "T", "T", "F");
    }

    private void doDefaultKmeanClust() {
        Clustering.PlotKmeans(sb, sb.getCurrentImage("km"), "png", 72, 3);
    }

    private void doDefaultSOMClust() {
        Clustering.PlotSOM(sb, sb.getCurrentImage("som"), "png", 72, 1, 3, "linear", "gaussian");
    }

    private void doDefaultRF() {
        Classifying.InitRF(sb, 500, 7, 1);
        Classifying.PlotRFClassication(sb, sb.getCurrentImage("rf_cls"), "png", 72);
        Classifying.PlotRFCmpd(sb, sb.getCurrentImage("rf_imp"), "png", 72);
        Classifying.PlotRFOutlier(sb, sb.getCurrentImage("rf_outlier"), "png", 72);
    }

    private void doDefaultSVM() {
        Classifying.InitSVMAnal(sb, "10");
        Classifying.PlotSVMClassification(sb, sb.getCurrentImage("svm_cls"), "png", 72);
        Classifying.PlotSVMSigCmpds(sb, sb.getCurrentImage("svm_imp"), "png", 72);
    }
}
