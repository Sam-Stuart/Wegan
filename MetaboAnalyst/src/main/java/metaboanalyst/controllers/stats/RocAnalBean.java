/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.FeatureBean;
import metaboanalyst.models.NameBean;
import metaboanalyst.rwrappers.RCenter;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.RocUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.event.TransferEvent;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "rocAnalBean")
@SessionScoped
public class RocAnalBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    private int kmClustNm = 5;

    public int getKmClustNm() {
        return kmClustNm;
    }

    public void setKmClustNm(int kmClustNm) {
        this.kmClustNm = kmClustNm;
    }

    private boolean partialRoc = false;

    public boolean isPartialRoc() {
        return partialRoc;
    }

    public void setPartialRoc(boolean partialRoc) {
        this.partialRoc = partialRoc;
    }

    private String univPerfOpt = "sp";

    public String getUnivPerfOpt() {
        return univPerfOpt;
    }

    public void setUnivPerfOpt(String univPerfOpt) {
        this.univPerfOpt = univPerfOpt;
    }

    private double univThresh = 0.2;

    public double getUnivThresh() {
        return univThresh;
    }

    public void setUnivThresh(double univThresh) {
        this.univThresh = univThresh;
    }

    private double rocCutOff = 0.2;

    public double getRocCutOff() {
        return rocCutOff;
    }

    public void setRocCutOff(double rocCutOff) {
        this.rocCutOff = rocCutOff;
    }

    private SelectItem[] mdlOpts = null;
    private SelectItem[] rocMdlOpts = null;

    public void setupMdlOptions() {
        String[] nms = RocUtils.getModelNames(sb.getRConnection());
        mdlOpts = new SelectItem[nms.length];
        rocMdlOpts = new SelectItem[nms.length + 1];
        rocMdlOpts[0] = new SelectItem(0, "Compare All Models");
        for (int i = 0; i < nms.length; i++) {
            mdlOpts[i] = new SelectItem(i + 1, nms[i]);
            rocMdlOpts[i + 1] = new SelectItem(i + 1, nms[i]);
        }
        rocMdlDD = 0;
    }

    public SelectItem[] getMdlOpts() {
        return mdlOpts;
    }

    public SelectItem[] getRocMdlOpts() {
        return rocMdlOpts;
    }

    private int rocMdlDD;

    public int getRocMdlDD() {
        return rocMdlDD;
    }

    public void setRocMdlDD(int rocMdlDD) {
        this.rocMdlDD = rocMdlDD;
    }

    private boolean showMisCls = false;

    public boolean isShowMisCls() {
        return showMisCls;
    }

    public void setShowMisCls(boolean showMisCls) {
        this.showMisCls = showMisCls;
    }

    //analysis mode : univ/explore/test
    private String analMode = "class";

    public String getAnalMode() {
        return analMode;
    }

    public void setAnalMode(String analMode) {
        this.analMode = analMode;
        RocUtils.setAnalysisMode(sb.getRConnection(), analMode);
    }

    //reset data beans after editing
    public void resetData() {
        featureBeans = null;
        sampleItems1 = null;
        univInit = false;
    }

    /**
     * a temp variable to store the current selected table row number this var
     * is shared by all the tables in the program since at any time only one
     * table can be selected upon; note the first row is 0; need to add 1 to
     * call R table row
     */
    private int tableRowInx;

    public int getTableRowInx() {
        return tableRowInx;
    }

    public void setTableRowInx(int tableRowInx) {
        this.tableRowInx = tableRowInx;
    }

    private ArrayList<FeatureBean> featureBeans;

    public ArrayList<FeatureBean> getFeatureBeans() {
        return featureBeans;
    }

    public void setFeatureBeans(ArrayList<FeatureBean> featureBeans) {
        this.featureBeans = featureBeans;
    }

    private NameBean[] smpl1Beans;
    private NameBean[] smpl2Beans;

    public NameBean[] getSmpl1Beans() {
        return smpl1Beans;
    }

    public void setSmpl1Beans(NameBean[] smplBeans) {
        this.smpl1Beans = smplBeans;
    }

    public NameBean[] getSmpl2Beans() {
        return smpl2Beans;
    }

    public void setSmpl2Beans(NameBean[] smplBeans) {
        this.smpl2Beans = smplBeans;
    }

    public void createSmplVec() {
        RConnection RC = sb.getRConnection();
        String[] names = RocUtils.getSample1Names(RC);
        smpl1Beans = new NameBean[names.length];
        for (int i = 0; i < smpl1Beans.length; i++) {
            smpl1Beans[i] = new NameBean(names[i]);
        }

        names = RocUtils.getSample2Names(RC);
        smpl2Beans = new NameBean[names.length];
        for (int i = 0; i < smpl2Beans.length; i++) {
            smpl2Beans[i] = new NameBean(names[i]);
        }
    }

    private String currentCmpd = null;

    public String getCurrentCmpd() {
        return currentCmpd;
    }

    public void setCurrentCmpd(String currentCmpd) {
        this.currentCmpd = currentCmpd;
    }

    private int lvNum = 2;

    public int getLvNum() {
        return lvNum;
    }

    public String getNewSampleNames() {
        return RDataUtils.getNewSampleNames(sb.getRConnection());
    }

    public void setLvNum(int lvlNum) {
        this.lvNum = lvlNum;
    }

    private String clsMethodOpt = "svm";

    public String getClsMethodOpt() {
        return clsMethodOpt;
    }

    public void setClsMethodOpt(String clsMethodOpt) {
        this.clsMethodOpt = clsMethodOpt;
    }

    public boolean getShowLRTap() {
        return (!this.clsMethodOpt.equals("lr"));
    }

    private String featRankOpt = "svm";

    public String getFeatRankOpt() {
        return featRankOpt;
    }

    public void setFeatRankOpt(String featRankOpt) {
        this.featRankOpt = featRankOpt;
    }

    public String getClsMethodLabel() {
        if (featRankOpt.equals("svm")) {
            return "SVM";
        }
        if (featRankOpt.equals("rf")) {
            return "RandomForest";
        }
        if (featRankOpt.equals("pls")) {
            return "PLS-DA";
        }
        if (featRankOpt.equals("lr")) {
            return "Logistic Regression";
        }
        return null;
    }

    public String getFeatRankLabel() {
        if (featRankOpt.equals("svm")) {
            return "SVM built-in";
        }
        if (featRankOpt.equals("rf")) {
            return "RandomForest built-in";
        }
        if (featRankOpt.equals("pls")) {
            return "PLSDA built-in";
        }
        if (featRankOpt.equals("auroc")) {
            return "Univariate AUROC";
        }
        if (featRankOpt.equals("fisher")) {
            return "Fisher's Exact";
        }
        if (featRankOpt.equals("tt")) {
            return "T-statistics";
        }
        return null;
    }

    public String performExploreAnalysis() {
        setAnalMode("explore");
        RConnection RC = sb.getRConnection();
        RocUtils.prepareROCData(RC);
        RocUtils.PerformRocCVExplorer(RC, clsMethodOpt, featRankOpt, lvNum);
        RocUtils.PlotProbView(sb, sb.getNewImage("cls_prob"), "png", 72, -1, 0, 0);
        RocUtils.PlotImpVar(sb, sb.getNewImage("cls_imp"), "png", 72, -1, "freq", 15);
        RocUtils.PlotAccuracies(sb, sb.getNewImage("cls_accu"), "png", 72);
        RocUtils.PlotROC(sb, sb.getNewImage("cls_roc"), "png", 72, 0, "threshold", 0, 0, "fpr", 0.5);
        setupMdlOptions();
        showConf = false;
        showMisCls = false;
        rankMeasure = "freq";
        return "Explorer";
    }

    private boolean testInit = false;

    public void setupFeatureTable() {
        setAnalMode("test");
        RConnection RC = sb.getRConnection();
        RocUtils.prepareROCData(RC);
        testInit = false;

        if (featureBeans == null) {
            RocUtils.ComputeUnivFeatureRanking(RC);
            prepareFeatureBean();
        }
        if (sampleItems1 == null) {
            prepareSampleBeans();
        }

        sb.registerPage("Builder");
    }

    private boolean univInit = false;

    public void performDefaultUnivAnalysis() {

        if (FacesContext.getCurrentInstance().getPartialViewContext().isAjaxRequest()) {
            return; // Skip ajax requests.
        }

        setAnalMode("univ");
        RConnection RC = sb.getRConnection();
        RocUtils.prepareROCData(RC);
        if (!univInit) {
            RocUtils.ComputeUnivFeatureRanking(RC);
            prepareFeatureBean();
            sb.registerPage("Univariate");
            univInit = true;
        }

    }

    public String getRocUnivImg() {
        String cmpdName2 = currentCmpd;
        if (currentCmpd != null) {
            cmpdName2 = currentCmpd.replaceAll("\\/", "_");
        }
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage(cmpdName2) + "dpi72.png";
    }

    public String getRocImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("cls_roc") + "dpi72.png";
    }

    public String getRocTestImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("cls_test_roc") + "dpi72.png";
    }

    public String getProbImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("cls_prob") + "dpi72.png";
    }

    public String getProbTestImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("cls_test_prob") + "dpi72.png";
    }

    public String getPredImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("cls_accu") + "dpi72.png";
    }

    public String getPredTestImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("cls_test_accu") + "dpi72.png";
    }

    public String getImpImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("cls_imp") + "dpi72.png";
    }

    public String getPermImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("roc_perm") + "dpi72.png";
    }

    public String getRocLRImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("cls_roc_lr") + "dpi72.png";
    }

    private void prepareFeatureBean() {
        //set up ranking and feature beans
        RConnection RC = sb.getRConnection();

        String[] rownames = RocUtils.getUnivRankedFeatureNames(RC);
        double[][] sigmat = RocUtils.getUnivFeatureRankingMat(RC);

        //set up content
        if (rownames == null || rownames.length == 0) {
            return;
        }

        int totLen = rownames.length;

        //now set up the feature beans
        featureBeans = new ArrayList();
        FeatureBean fb = null;

        for (int i = 0; i < totLen; i++) {
            fb = new FeatureBean();
            fb.addName(rownames[i]);
            fb.setUniqID(i);
            for (int m = 0; m < 4; m++) {
                fb.addValue(sigmat[i][m]);
            }
            featureBeans.add(fb);
        }
    }

    public List<FeatureBean> selectedFeatureBeans;

    public List<FeatureBean> getSelectedFeatureBeans() {
        return selectedFeatureBeans;
    }

    public void setSelectedFeatureBeans(List<FeatureBean> selectedFeatureBeans) {
        this.selectedFeatureBeans = selectedFeatureBeans;
    }

    public List<FeatureBean> lassoFeatureBeans = new ArrayList();

    public void computeFeatureBeans() {
        lassoFeatureBeans = new ArrayList();
        RConnection RC = sb.getRConnection();
        double[] freqs = RocUtils.getLassoFreqs(RC);
        String[] rownames = RocUtils.getLassoFreqNames(RC);
        int totLen = rownames.length;
        FeatureBean fb = null;
        for (int i = 0; i < totLen; i++) {
            fb = new FeatureBean();
            fb.addName(rownames[i]);
            fb.setUniqID(i);
            fb.addValue(freqs[i]);
            lassoFeatureBeans.add(fb);
        }
    }

    public List<FeatureBean> getLassoFeatureBeans() {
        return lassoFeatureBeans;
    }

    private DualListModel<String> sampleItems1, sampleItems2;
    private boolean smplHoldOut = false;

    public boolean isSmplHoldOut() {
        return smplHoldOut;
    }

    public void setSmplHoldOut(boolean smplHoldOut) {
        this.smplHoldOut = smplHoldOut;
    }

    public DualListModel<String> getSampleItems1() {
        return sampleItems1;
    }

    public void setSampleItems1(DualListModel<String> sampleItems1) {
        this.sampleItems1 = sampleItems1;
    }

    public DualListModel<String> getSampleItems2() {
        return sampleItems2;
    }

    public void setSampleItems2(DualListModel<String> sampleItems2) {
        this.sampleItems2 = sampleItems2;
    }

    public void prepareSampleBeans() {
        RConnection RC = sb.getRConnection();
        String[] names1 = RocUtils.getSample1Names(RC);
        String[] names2 = RocUtils.getSample2Names(RC);
        sampleItems1 = new DualListModel(Arrays.asList(names1), new ArrayList());
        sampleItems2 = new DualListModel(Arrays.asList(names2), new ArrayList());
    }

    public void doTransfer(TransferEvent event) {
        StringBuilder builder = new StringBuilder();
        for (Object item : event.getItems()) {
            builder.append(item.toString()).append("<br />");
        }
    }

    public String prepareTesterData() {
        if (setupVarTester()) {
            if (setupSmplTester()) {
                testInit = false;
                return "Evaluator";
            }
            return null;
        }
        return null;
    }

    List<String> selectedSmpl1, selectedSmpl2;

    public List<String> getSelectedSmpl1() {
        return selectedSmpl1;
    }

    public List<String> getSelectedSmpl2() {
        return selectedSmpl2;
    }

    private boolean setupVarTester() {
        if (selectedFeatureBeans.isEmpty()) {
            sb.updateMsg("Error", "No features were selected!");
            return false;
        }
        RConnection RC = sb.getRConnection();
        String cmd = "selected.cmpds <- c(\"";
        int count = 0;
        String nm;
        for (int i = 0; i < selectedFeatureBeans.size(); i++) {
            nm = selectedFeatureBeans.get(i).getName();
            if (count == 0) {
                cmd = cmd + nm;
            } else {
                cmd = cmd + "\", \"" + nm;
            }
            count++;
        }
        cmd = cmd + "\");";
        try {
            RCenter.recordRCommand(RC, cmd, true);
            RC.voidEval(cmd);
            sb.updateMsg("OK", "A total of " + count + " compounds are selected.");
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    private boolean setupSmplTester() {
        RConnection RC = sb.getRConnection();
        smplHoldOut = false;
        selectedSmpl1 = sampleItems1.getTarget();
        selectedSmpl2 = sampleItems2.getTarget();

        if (selectedSmpl1.isEmpty() && selectedSmpl2.isEmpty()) {
            String cmd = "selected.smpls <- c()";
            try {
                RCenter.recordRCommand(RC, cmd, true);
                RC.voidEval(cmd);
                sb.updateMsg("OK", "A total of 0 samples are selected.");
                return true;
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            if (selectedSmpl1.isEmpty()) {
                sb.updateMsg("Error", "No samples are selected for group 1!");
                return false;
            }
            if (selectedSmpl2.isEmpty()) {
                sb.updateMsg("Error", "No samples are selected for group 2!");
                return false;
            }
        }
        int count = 0;
        String nm;
        String cmd = "selected.smpls <- c(\"";
        for (int i = 0; i < selectedSmpl1.size(); i++) {
            nm = selectedSmpl1.get(i);
            if (count == 0) {
                cmd = cmd + nm;
            } else {
                cmd = cmd + "\", \"" + nm;
            }
            count++;
        }
        for (int m = 0; m < selectedSmpl2.size(); m++) {
            nm = selectedSmpl2.get(m);
            cmd = cmd + "\", \"" + nm;
            count++;
        }
        cmd = cmd + "\"); ";
        try {
            RCenter.recordRCommand(RC, cmd, true);
            RC.voidEval(cmd);
            smplHoldOut = true;
            sb.updateMsg("OK", "A total of " + count + " samples are selected.");
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    private boolean showOptPoint = true;
    private boolean showCI = false;

    public boolean isShowOptPoint() {
        return showOptPoint;
    }

    public void setShowOptPoint(boolean showOptPoint) {
        this.showOptPoint = showOptPoint;
    }

    public boolean isShowCI() {
        return showCI;
    }

    public void setShowCI(boolean showCI) {
        this.showCI = showCI;
    }

    private String optimalDD = "closest.topleft";

    public String getOptimalDD() {
        return optimalDD;
    }

    public void setOptimalDD(String optimalDD) {
        this.optimalDD = optimalDD;
    }

    public void updateROC() {
        if (currentCmpd == null) {
            sb.updateMsg("Error", "Please choose a compound first (clicking the corresponding View link)");
        } else {
            plotROC(currentCmpd);
        }
    }

    private String multPerfOpt = "fpr";

    public String getMultPerfOpt() {
        return multPerfOpt;
    }

    public void setMultPerfOpt(String multPerfOpt) {
        this.multPerfOpt = multPerfOpt;
    }

    //Multi ROC
    private boolean showConf = false;

    public boolean isShowConf() {
        return showConf;
    }

    public void setShowConf(boolean showConf) {
        this.showConf = showConf;
    }

    private boolean showConfLR = false;

    public boolean isShowConfLR() {
        return showConfLR;
    }

    public void setShowConfLR(boolean showConfLR) {
        this.showConfLR = showConfLR;
    }

    public void updateMultiROC() {
        RocUtils.PlotROC(sb, sb.getNewImage("cls_roc"), "png", 72, rocMdlDD, "threshold", showConf ? 1 : 0, 0, multPerfOpt, rocCutOff);
        RequestContext.getCurrentInstance().scrollTo("ac:form1:rocPane");
    }

    public void updateProbView() {
        RocUtils.PlotProbView(sb, sb.getNewImage("cls_prob"), "png", 72, rocMdlDD, showMisCls ? 1 : 0, 0);
        RequestContext.getCurrentInstance().scrollTo("ac:form2:screePane");
    }

    public String getConfMat() {
        return (RocUtils.getConfusionMatrix(sb.getRConnection()));
    }

    public String getTestConfMat() {
        if (showPredCls) {
            return (RocUtils.getConfusionMatrixTest(sb.getRConnection()));
        } else {
            return (RocUtils.getConfusionMatrix(sb.getRConnection()));
        }
    }

    public String getAccuText() {
        return RocUtils.getAccuSummary(sb.getRConnection());
    }

    private int featNum = 15;

    public int getFeatNum() {
        return featNum;
    }

    public void setFeatNum(int featNum) {
        this.featNum = featNum;
    }

    private String rankMeasure = "freq";

    public String getRankMeasure() {
        return rankMeasure;
    }

    public void setRankMeasure(String rankMeasure) {
        this.rankMeasure = rankMeasure;
    }

    public void updateImpView() {
        RocUtils.PlotImpVar(sb, sb.getNewImage("cls_imp"), "png", 72, rocMdlDD, rankMeasure, featNum);
        RequestContext.getCurrentInstance().scrollTo("ac:form4:impPane");
    }

    public void plotROC(String cmpdName) {

        String isPartial = partialRoc ? "T" : "F";
        String measure = univPerfOpt;
        double cutoff = univThresh;
        if (cutoff < 0 || cutoff > 1) {
            return;
        }

        String isAUC = showCI ? "T" : "F";
        String isOpt = showOptPoint ? "T" : "F";
        String optMtd = optimalDD;

        String cmpdName2 = cmpdName.replaceAll("\\/", "_");
        String imgName = sb.getNewImage(cmpdName2);

        RocUtils.PerformUnivROC(sb, cmpdName, imgName, "png", 72, isAUC, isOpt, optMtd, isPartial, measure, cutoff);
        currentCmpd = cmpdName;
        showCI = false;
    }

    public void performTestAnalysis() {

        //   multivInit = false; // b/c overwrite the explorer, force re-generate
        RConnection RC = sb.getRConnection();
        RocUtils.PerformRocCVTester(RC, clsMethodOpt, lvNum);
        RocUtils.PlotROC(sb, sb.getCurrentImage("cls_test_roc"), "png", 72, 0, avgMtd, 0, showHoldOut ? 1 : 0, "fpr", 0.5);
        RocUtils.PlotProbView(sb, sb.getCurrentImage("cls_test_prob"), "png", 72, -1, 0, 0);
        RocUtils.PlotTestAccuracies(sb, sb.getCurrentImage("cls_test_accu"), "png", 72);
        setupMdlOptions();
        setupSamplePredTable();
        showConf = false;
        if (clsMethodOpt.equals("lr")) {
            showConfLR = false;
            RocUtils.PlotROCLR(sb, sb.getNewImage("cls_roc_lr"), "png", 72, showConfLR ? 1 : 0);
        }
    }

    public String performDefaultTestAnalysis(boolean forceUpdate) {
        if (analMode.equals("explore")) {
            sb.updateMsg("Error", "You need to access this page from Builder!");
            return null;
        }
        if (!testInit | forceUpdate) {
            RConnection RC = sb.getRConnection();
            RocUtils.setCustomData(RC);
            RocUtils.PerformRocCVTester(RC, clsMethodOpt, lvNum);
            RocUtils.PlotROC(sb, sb.getCurrentImage("cls_test_roc"), "png", 72, 0, avgMtd, 0, 0, "fpr", 0.5);
            RocUtils.PlotProbView(sb, sb.getCurrentImage("cls_test_prob"), "png", 72, -1, 0, 0);
            RocUtils.PlotTestAccuracies(sb, sb.getCurrentImage("cls_test_accu"), "png", 72);
            setupMdlOptions();
            setupSamplePredTable();
            sb.registerPage("Evaluator");
            testInit = true;
            showConf = false;
            showConfLR = false;
            //  multivInit = false; // b/c overwrite the explorer, force re-generate
            if (clsMethodOpt.equals("lr")) {
                RocUtils.PlotROCLR(sb, sb.getNewImage("cls_roc_lr"), "png", 72, showConfLR ? 1 : 0);
            }
        }
        if (forceUpdate) {
            return "Builder";
        } else {
            return null;
        }
    }

    private String avgMtd = "threshold";

    public String getAvgMtd() {
        return avgMtd;
    }

    public void setAvgMtd(String avgMtd) {
        this.avgMtd = avgMtd;
    }

    private boolean showHoldOut = false;

    public boolean isShowHoldOut() {
        return showHoldOut;
    }

    public void setShowHoldOut(boolean showHoldOut) {
        this.showHoldOut = showHoldOut;
    }

    public String updateKmeans() {
        if (kmClustNm < 2) {
            sb.updateMsg("Error", "A meaningful cluster number should be 2 or higher");
            return null;
        }
        RocUtils.updateKmeans(sb.getRConnection(), kmClustNm);
        prepareFeatureBean();
        return null;
    }

    public void updateTestRoc() {
        RocUtils.PlotROC(sb, sb.getNewImage("cls_test_roc"), "png", 72, 0, avgMtd, showConf ? 1 : 0, showHoldOut ? 1 : 0, multPerfOpt, rocCutOff);
        RequestContext.getCurrentInstance().scrollTo("ac:form1:rocPane");
    }

    public void updateROCLRplot() {
        RocUtils.PlotROCLR(sb, sb.getNewImage("cls_roc_lr"), "png", 72, showConfLR ? 1 : 0);
        RequestContext.getCurrentInstance().scrollTo("ac:formLR:rocLRpanel");
    }

    private boolean showPredCls = false;

    public boolean isShowPredCls() {
        return showPredCls;
    }

    public void setShowPredCls(boolean showPredCls) {
        this.showPredCls = showPredCls;
    }

    public void updateTestProbView() {
        RocUtils.PlotProbView(sb, sb.getNewImage("cls_test_prob"), "png", 72, rocMdlDD, showMisCls ? 1 : 0, showPredCls ? 1 : 0);
        RequestContext.getCurrentInstance().scrollTo("ac:form2:screePane");
    }

    private String perfMeasure = "auroc";

    public String getPerfMeasure() {
        return perfMeasure;
    }

    public void setPerfMeasure(String perfMeasure) {
        this.perfMeasure = perfMeasure;
    }

    private int permNum = 0;

    public int getPermNum() {
        return permNum;
    }

    public void setPermNum(int permNum) {
        this.permNum = permNum;
    }

    public String performROCpermutation() {
        if (permNum == 0) {
            return null;
        }
        RocUtils.PerformPermut(sb.getRConnection(), perfMeasure, permNum);
        RocUtils.PlotPermut(sb, sb.getNewImage("roc_perm"), "png", 72);
        return null;
    }

    public void setupSamplePredTable() {

        RConnection RC = sb.getRConnection();
        if (RocUtils.containNewSamples(RC) == 1) {
            String[] nms = RDataUtils.getNewSampleNameVec(RC);
            double[] probs = RDataUtils.getNewSampleProbs(RC);
            String[] grps = RDataUtils.getNewSampleGrps(RC);
            smplPredBeans = new ArrayList<>();
            for (int i = 0; i < nms.length; i++) {
                NameBean nb = new NameBean(nms[i]);
                nb.setProb(probs[i]);
                nb.setCls(grps[i]);
                smplPredBeans.add(nb);
            }
        }
    }

    private ArrayList<NameBean> smplPredBeans;

    public ArrayList<NameBean> getSmplPredBeans() {
        return smplPredBeans;
    }

    public void setSmplPredBeans(ArrayList<NameBean> smplPredBeans) {
        this.smplPredBeans = smplPredBeans;
    }

    private boolean methodRocLR = false; // to show/hide Logistic Model Tab in RocTestView.xhtml

    public String getLRConvergence() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRConvergence(sb.getRConnection()));
        } else {
            return (null);
        }
    }

    public String getLREquation() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLREquation(sb.getRConnection()));
        } else {
            return (null);
        }
    }

    public String getLRmodelTable() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRmodelTable(sb.getRConnection()));
        } else {
            return (null);
        }
    }

    public String getLRperformTable() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRperformanceTable(sb.getRConnection()));
        } else {
            return (null);
        }
    }

    public String getLRclsLabel() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRclsLabel(sb.getRConnection()));
        } else {
            return (null);
        }
    }

    public String getLRclsLabelNew() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRclsLabelNew(sb.getRConnection()));
        } else {
            return (null);
        }
    }

    public String getLRthreshold() {
        if (clsMethodOpt.equals("lr")) {
            return (RocUtils.getLRthreshold(sb.getRConnection()));
        } else {
            return (null);
        }
    }

    // private boolean univInit = false;
    public String performDefaultDetailRocAnalysis(String cmpd) {
        if (currentCmpd == null || !currentCmpd.equals(cmpd)) {
            plotROC(cmpd);
        }
        prepareDetailROCBeans();
        return ("ROC detail");
    }

    private void prepareDetailROCBeans() {
        RConnection RC = sb.getRConnection();
        RocUtils.PrepareROCDetails(RC, currentCmpd);

        double[][] sigmat = RocUtils.GetRocValues(RC);

        //set up content
        if (sigmat == null || sigmat.length == 0) {
            return;
        }
        //now set up the feature beans
        rocDetailsBeans = new ArrayList();
        FeatureBean fb = null;

        for (int i = 0; i < sigmat.length; i++) {
            fb = new FeatureBean();
            fb.addName(sigmat[i][0] + "");
            fb.setUniqID(i);
            for (int m = 1; m < 6; m++) { //total six columns
                fb.addValue(sigmat[i][m]);
            }
            rocDetailsBeans.add(fb);
        }
        String cmpdName = currentCmpd.replaceAll("\\/", "_");
        downloadTxt = "<b>You can download the result table:</b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + "/" + cmpdName + "_roc.csv\"><b>" + "here" + "</b></a>";
    }

    private String downloadTxt = "";

    public String getDownloadTxt() {
        return downloadTxt;
    }

    public void setDownloadTxt(String downloadTxt) {
        this.downloadTxt = downloadTxt;
    }

    private ArrayList<FeatureBean> rocDetailsBeans;

    public ArrayList<FeatureBean> getRocDetailsBeans() {
        return rocDetailsBeans;
    }

    public void setRocDetailsBeans(ArrayList<FeatureBean> rocDetailsBeans) {
        this.rocDetailsBeans = rocDetailsBeans;
    }

    private String cutoff, sens, spec;

    public String getCutoff() {
        return cutoff;
    }

    public void setCutoff(String cutoff) {
        this.cutoff = cutoff;
    }

    public String getSens() {
        return sens;
    }

    public void setSens(String sens) {
        this.sens = sens;
    }

    public String getSpec() {
        return spec;
    }

    public void setSpec(String spec) {
        this.spec = spec;
    }

    private boolean canEdit = true;

    public boolean isCanEdit() {
        return canEdit;
    }

    public void setCanEdit(boolean canEdit) {
        this.canEdit = canEdit;
    }

    private int count = 0;

    public void threshBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.

        String fld = null;
        double val = 0;
        if (cutoff != null && cutoff.replaceAll("\\s+$", "").length() > 0) {
            fld = "threshold";
            val = Double.parseDouble(cutoff);
        } else if (sens != null && sens.replaceAll("\\s+$", "").length() > 0) {
            fld = "sensitivity";
            val = Double.parseDouble(sens);
            if (val < 0 || val > 1) {
                sb.updateMsg("Error", "Value must be between [0 1]");
                return;
            }
        } else if (spec != null && spec.replaceAll("\\s+$", "").length() > 0) {
            fld = "specificity";
            val = Double.parseDouble(spec);
            if (val < 0 || val > 1) {
                sb.updateMsg("Error", "Value must be between [0 1]");
                return;
            }
        } else {
            sb.updateMsg("Error", "No meaningful values were detected");
            return;
        }

        String cmpdName2 = currentCmpd.replaceAll("\\/", "_");
        count = count + 1;
        String imgNm = cmpdName2 + "_" + count;
        String[] res = RocUtils.GetROCcoords(sb.getRConnection(), fld, val, "TRUE", imgNm);

        cutoff = res[0];
        spec = res[1];
        sens = res[2];
        canEdit = false;
    }

    public void resetBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        cutoff = "";
        spec = "";
        sens = "";
        canEdit = true;
    }

    public String getRocDetailImg() {
        String cmpdName2 = currentCmpd;
        if (currentCmpd != null) {
            cmpdName2 = currentCmpd.replaceAll("\\/", "_");
        }
        if (count == 0) {
            return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage(cmpdName2) + "dpi72.png";
        } else {
            return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + cmpdName2 + "_" + count + "_dpi72.png";
        }
    }

    public String getProbDownloadLink() {
        return "<a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName() + "/roc_pred_prob.csv\"><b>" + "Details" + "</b></a>";
    }

    public String getProbDownloadLink2() {
        return "<a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName() + "/roc_pred_prob1.csv\"><b>" + "Details" + "</b></a>";
    }

}
