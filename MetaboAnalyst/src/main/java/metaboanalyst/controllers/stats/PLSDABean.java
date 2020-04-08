/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.File;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.ChemoMetrics;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.chart.Axis;
import org.primefaces.model.chart.AxisType;
import org.primefaces.model.chart.LineChartModel;
import org.primefaces.model.chart.LineChartSeries;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "plsBean")
public class PLSDABean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    private int plsPairNum = 5;
    private int plsScore2dX = 1;
    private int plsScore2dY = 2;
    private boolean displayConfs = true;
    private boolean displayNames = true;
    private boolean displayFeatNames = true;

    private int plsScore3dX = 1;
    private int plsScore3dY = 2;
    private int plsScore3dZ = 3;
    private int rotationAngle = 40;
    private int plsLoadX = 1;
    private int plsLoadY = 2;
    private String loadOpt = "scatter";
    private String impOpt = "vip";
    private int searchCompNum = 0;
    private String cvType = "NA";//10 fold CV
    private String perfMeasure = "Q2";
    private String vipOpt;
    private String coefOpt;
    private int impFeatNum = 15;
    private boolean grayScale = false; // for VIP
    private boolean greyScale = false;  //for Score plot
    private String permStat = "bw";
    private int permNum = 0;
    private String plsVarOpt = "xvar";

    public String getPlsVarOpt() {
        return plsVarOpt;
    }

    public void setPlsVarOpt(String plsVarOpt) {
        this.plsVarOpt = plsVarOpt;
    }
    

    public SelectItem[] getPlsPCs() {
        int pcNums = ChemoMetrics.GetMaxPCACompNumber(sb) - 2;
        SelectItem[] items = new SelectItem[pcNums];
        for (int i = 0; i < pcNums; i++) {
            int pcNum = i + 2;
            items[i] = new SelectItem(pcNum, pcNum + "");
        }
        return items;
    }

    public SelectItem[] getPlsAllPCs() {
        int pcNums = ChemoMetrics.GetMaxPCACompNumber(sb) - 1;
        SelectItem[] items = new SelectItem[pcNums];
        for (int i = 0; i < pcNums; i++) {
            int pcNum = i + 1;
            items[i] = new SelectItem(pcNum, pcNum + "");
        }
        return items;
    }

    public boolean isGrayScale() {
        return grayScale;
    }

    public void setGrayScale(boolean grayScale) {
        this.grayScale = grayScale;
    }

    public boolean isGreyScale() {
        return greyScale;
    }

    public void setGreyScale(boolean greyScale) {
        this.greyScale = greyScale;
    }

    public int getPlsPairNum() {
        return plsPairNum;
    }

    public void setPlsPairNum(int plsPairNum) {
        this.plsPairNum = plsPairNum;
    }

    public int getPlsScore2dX() {
        return plsScore2dX;
    }

    public void setPlsScore2dX(int plsScore2dX) {
        this.plsScore2dX = plsScore2dX;
    }

    public int getPlsScore2dY() {
        return plsScore2dY;
    }

    public void setPlsScore2dY(int plsScore2dY) {
        this.plsScore2dY = plsScore2dY;
    }

    public boolean isDisplayConfs() {
        return displayConfs;
    }

    public void setDisplayConfs(boolean displayConfs) {
        this.displayConfs = displayConfs;
    }

    public boolean isDisplayNames() {
        return displayNames;
    }

    public void setDisplayNames(boolean displayNames) {
        this.displayNames = displayNames;
    }

    public int getPlsScore3dX() {
        return plsScore3dX;
    }

    public void setPlsScore3dX(int plsScore3dX) {
        this.plsScore3dX = plsScore3dX;
    }

    public int getPlsScore3dY() {
        return plsScore3dY;
    }

    public void setPlsScore3dY(int plsScore3dY) {
        this.plsScore3dY = plsScore3dY;
    }

    public int getPlsScore3dZ() {
        return plsScore3dZ;
    }

    public void setPlsScore3dZ(int plsScore3dZ) {
        this.plsScore3dZ = plsScore3dZ;
    }

    public int getRotationAngle() {
        return rotationAngle;
    }

    public void setRotationAngle(int plsRotationAngle) {
        this.rotationAngle = plsRotationAngle;
    }

    public int getPlsLoadX() {
        return plsLoadX;
    }

    public void setPlsLoadX(int plsLoadX) {
        this.plsLoadX = plsLoadX;
    }

    public int getPlsLoadY() {
        return plsLoadY;
    }

    public void setPlsLoadY(int plsLoadY) {
        this.plsLoadY = plsLoadY;
    }

    public String getLoadOpt() {
        return loadOpt;
    }

    public void setLoadOpt(String plsLoadOpt) {
        this.loadOpt = plsLoadOpt;
    }

    public boolean isDisplayFeatNames() {
        return displayFeatNames;
    }

    public void setDisplayFeatNames(boolean displayFeatNames) {
        this.displayFeatNames = displayFeatNames;
    }

    public String getImpOpt() {
        return impOpt;
    }

    public void setImpOpt(String impOpt) {
        this.impOpt = impOpt;
    }

    public int getSearchCompNum() {
        if (searchCompNum == 0) {
            searchCompNum = ChemoMetrics.GetDefaultPLSCVNumber(sb);
        }
        return searchCompNum;
    }

    public void setSearchCompNum(int searchCompNum) {
        this.searchCompNum = searchCompNum;
    }

    public String getCvType() {
        if (cvType.equals("NA")) {
            cvType = "T";
            int minSize = RDataUtils.getMinGroupSize(sb.getRConnection());
            if (minSize < 11) {
                cvType = "L";
            }
        }
        return cvType;
    }

    public void setCvType(String cvType) {
        this.cvType = cvType;
    }

    public String getPerfMeasure() {
        return perfMeasure;
    }

    public void setPerfMeasure(String perfMeasure) {
        this.perfMeasure = perfMeasure;
    }

    public String getVipOpt() {
        return vipOpt;
    }

    public void setVipOpt(String vipOpt) {
        this.vipOpt = vipOpt;
    }

    public String getCoefOpt() {
        return coefOpt;
    }

    public void setCoefOpt(String coefOpt) {
        this.coefOpt = coefOpt;
    }

    public int getImpFeatNum() {
        return impFeatNum;
    }

    public void setImpFeatNum(int vipFeatNum) {
        this.impFeatNum = vipFeatNum;
    }

    public String getPermStat() {
        return permStat;
    }

    public void setPermStat(String permStat) {
        this.permStat = permStat;
    }

    public int getPermNum() {
        return permNum;
    }

    public void setPermNum(int permNum) {
        this.permNum = permNum;
    }

    private String permMsg = "";

    public String getPermMsg() {
        return permMsg;
    }

    public void updatePLSDA() {
        ChemoMetrics.InitPLS(sb);
        ChemoMetrics.PlotPLSPairSummary(sb, sb.getNewImage("pls_pair"), "png", 72, plsPairNum);
        ChemoMetrics.PlotPLS2DScore(sb, sb.getNewImage("pls_score2d"), "png", 72, 1, 2, 0.95, 1, 0);
        // ChemoMetrics.PlotPLS3DScore(sb, sb.getCurrentImage("pls_score3d"), "png", 72, 1, 2, 3, 40);
        ChemoMetrics.PlotPLS3DScore(sb, sb.getNewImage("pls_score3d"), "json", 72, 1, 2, 3);
        ChemoMetrics.PlotPLSLoading(sb, sb.getNewImage("pls_loading"), "png", 72, 1, 2, "scatter", 1);

        String cvMethod = "T";
        int minSize = RDataUtils.getMinGroupSize(sb.getRConnection());
        if (minSize < 11) {
            cvMethod = "L";
        }
        ChemoMetrics.TrainPLSClassifier(sb, cvMethod, searchCompNum, "Q2");
        ChemoMetrics.PlotPLSClassification(sb, sb.getNewImage("pls_cv"), "png", 72);
        ChemoMetrics.PlotPLSImp(sb, sb.getNewImage("pls_imp"), "png", 72, "vip", "Comp. 1", 15, "FALSE");
    }

    public String plsPairBtn_action() {
        ChemoMetrics.PlotPLSPairSummary(sb, sb.getNewImage("pls_pair"), "png", 72, plsPairNum);
        RequestContext.getCurrentInstance().scrollTo("ac:form1:pairPane");
        return null;
    }

    public String plsScore2dBtn_action() {
        if (plsScore2dX == plsScore2dY) {
            sb.updateMsg("Error", "X and Y axes are of the same PC");
        } else {
            double conf = 0.95;
            if (!displayConfs) {
                conf = 0;
            }

            int useGreyScale = 0;
            if (greyScale) {
                useGreyScale = 1;
            }
            ChemoMetrics.PlotPLS2DScore(sb, sb.getNewImage("pls_score2d"), "png", 72, plsScore2dX, plsScore2dY, conf, displayNames ? 1 : 0, useGreyScale);
            RequestContext.getCurrentInstance().scrollTo("ac:form3:score2dPane");
        }
        return null;
    }

    private int activeTab = 0;

    public int getActiveTab() {
        return activeTab;
    }

    public void setActiveTab(int activeTab) {
        this.activeTab = activeTab;
    }

    public String plsScore3dBtn_action() {
        if (plsScore3dX == plsScore3dY || plsScore3dX == plsScore3dZ || plsScore3dY == plsScore3dZ) {
            sb.updateMsg("Error", "Detected the same PC on two axes!");
        } else {
            ChemoMetrics.PlotPLS3DScore(sb, sb.getNewImage("pls_score3d"), "json", 72, plsScore3dX, plsScore3dY, plsScore3dZ);
            activeTab = 2;
        }
        return null;
    }

    public void plsLoadBtn_action() {
        if (plsLoadX == plsLoadY) {
            sb.updateMsg("Error", "Detected the same PC on two axes!");
        } else {
            ChemoMetrics.PlotPLSLoading(sb, sb.getNewImage("pls_loading"), "png", 72, plsLoadX, plsLoadY, loadOpt, displayFeatNames ? 1 : 0);
            updateLoadModel();
        }
    }

    public String plsCVBtn_action() {
        int res = ChemoMetrics.TrainPLSClassifier(sb, cvType, searchCompNum, perfMeasure);
        if (res == 1) {
            ChemoMetrics.PlotPLSClassification(sb, sb.getNewImage("pls_cv"), "png", 72);
            RequestContext.getCurrentInstance().scrollTo("ac:form2:cvPane");
        } else {
            sb.updateMsg("Error", "The parameters cause errors in computing. ");
        }
        return null;
    }

    public String getPlsCVImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("pls_cv") + "dpi72.png";
    }

    public String plsImpBtn_action() {
        String imp = impOpt.equals("vip") ? vipOpt : coefOpt;
        if (imp.equalsIgnoreCase("NA")) {
            sb.updateMsg("Error", "This feature is not available for your data!");
            return null;
        }
        ChemoMetrics.PlotPLSImp(sb, sb.getNewImage("pls_imp"), "png", 72,
                impOpt, imp, impFeatNum, grayScale ? "TRUE" : "FALSE");
        RequestContext.getCurrentInstance().scrollTo("ac:form6:impPane");
        return null;
    }

    public String plsPermBtn_action() {
        if (permNum == 0) {
            sb.updateMsg("Error", "Please specify the number of permutations!");
            return null;
        }
        permMsg = ChemoMetrics.PLSPermute(sb, permNum, permStat);
        ChemoMetrics.PlotPLSPermutation(sb, sb.getNewImage("pls_perm"), "png", 72);
        RequestContext.getCurrentInstance().scrollTo("ac:form7:permPane");
        return null;
    }

    public String getPerfTxt() {
        double[][] vals = ChemoMetrics.GetPLS_CVMat(sb);
        String[] rownames = ChemoMetrics.GetPLSCVRowNames(sb);
        String[] colnames = ChemoMetrics.GetPLSCVColNames(sb);
        String str = "<h2>PLS-DA cross validation details: </h2><br/>";
        str = str + DataUtils.setupTable("Measure", vals, rownames, colnames);
        return str;
    }

    public String impDetailsLnk_action() {
        if (impOpt.equals("vip")) {
            return sb.detailsLnk_action("pls.vip");
        } else {
            return sb.detailsLnk_action("pls.coef");
        }
    }

    /*
     Methods for interactive scatter plotting 
     */
    private double loadMinX, loadMaxX, loadMinY, loadMaxY;

    public double getLoadMinX() {
        return loadMinX;
    }

    public void setLoadMinX(double loadMinX) {
        this.loadMinX = loadMinX;
    }

    public double getLoadMaxX() {
        return loadMaxX;
    }

    public void setLoadMaxX(double loadMaxX) {
        this.loadMaxX = loadMaxX;
    }

    public double getLoadMinY() {
        return loadMinY;
    }

    public void setLoadMinY(double loadMinY) {
        this.loadMinY = loadMinY;
    }

    public double getLoadMaxY() {
        return loadMaxY;
    }

    public void setLoadMaxY(double loadMaxY) {
        this.loadMaxY = loadMaxY;
    }

    public LineChartModel getLdModel() {
        if (ldModel == null) {
            updateLoadModel();
        }
        return ldModel;
    }

    public void setLdModel(LineChartModel ldModel) {
        this.ldModel = ldModel;
    }

    public void setLoadMaxX(int loadMaxX) {
        this.loadMaxX = loadMaxX;
    }

    private LineChartModel ldModel;

    public void updateLoadModel() {
        RConnection RC = sb.getRConnection();
        ldModel = new LineChartModel();
        LineChartSeries dots = new LineChartSeries();
        double[][] mat = RDataUtils.getPLSLoadMat(RC); //must be called first
        for (double[] row : mat) {
            dots.set(row[0], row[1]);
        }

        sb.setFeatureLabels(RDataUtils.getPLSLoadCmpds(RC));
        sb.setFeatureInx(RDataUtils.getPLSLoadCmpdInxs(RC));
        double[] rgx = RDataUtils.getPLSLoadAxesSpec(RC);
        loadMinX = rgx[0];
        loadMaxX = rgx[1];
        loadMinY = rgx[2];
        loadMaxY = rgx[3];

        dots.setShowLine(false);
        dots.setMarkerStyle("filledCircle', size:'9.0");
        ldModel.addSeries(dots);
        ldModel.setZoom(true);
        ldModel.setExtender("ext");
        Axis xAxis = ldModel.getAxis(AxisType.X);
        xAxis.setLabel("Loadings " + plsLoadX);
        xAxis.setMin(loadMinX);
        xAxis.setMax(loadMaxX);

        Axis yAxis = ldModel.getAxis(AxisType.Y);
        yAxis.setLabel("Loadings " + plsLoadY);
        yAxis.setMin(loadMinY);
        yAxis.setMax(loadMaxY);
    }

    private SelectItem[] vipItems = null;
    private SelectItem[] coefItems = null;

    public SelectItem[] getVipItems() {
        if (vipItems == null) {
            vipItems = RDataUtils.createSelectItems(sb, ChemoMetrics.GetPLSSigColNames(sb, "vip"));
        }
        return vipItems;
    }

    public void setVipItems(SelectItem[] vipItems) {
        this.vipItems = vipItems;
    }

    public SelectItem[] getCoefItems() {
        if (coefItems == null) {
            coefItems = RDataUtils.createSelectItems(sb, ChemoMetrics.GetPLSSigColNames(sb, "coef"));
        }
        return coefItems;
    }

    public void setCoefItems(SelectItem[] coefItems) {
        this.coefItems = coefItems;
    }
}