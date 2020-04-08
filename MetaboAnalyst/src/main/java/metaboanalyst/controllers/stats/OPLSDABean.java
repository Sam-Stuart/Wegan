/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.Serializable;
import javax.faces.bean.ManagedBean;
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
@ManagedBean(name = "oplsBean")
public class OPLSDABean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private boolean displayConfs = true;
    private boolean displayNames = true;
    private boolean displayFeatNames = true;

    private String loadOpt = "all";
    private boolean grayScale = false; // for VIP
    private boolean greyScale = false;  //for Score plot
    private String permStat = "bw";
    private int permNum = 20;

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

    //loadOpt can be "all", "none", "custom"
    public String getLoadOpt() {
        return loadOpt;
    }

    public void setLoadOpt(String loadOpt) {
        this.loadOpt = loadOpt;
    }

    public boolean isDisplayFeatNames() {
        return displayFeatNames;
    }

    public void setDisplayFeatNames(boolean displayFeatNames) {
        this.displayFeatNames = displayFeatNames;
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

    public String oplsScore2dBtn_action() {

        double conf = 0.95;
        if (!displayConfs) {
            conf = 0;
        }

        int useGreyScale = 0;
        if (greyScale) {
            useGreyScale = 1;
        }
        ChemoMetrics.PlotOPLS2DScore(sb, sb.getNewImage("opls_score2d"), "png", 72, 1, 2, conf, displayNames ? 1 : 0, useGreyScale);
        RequestContext.getCurrentInstance().scrollTo("ac:form3:score2dPane");

        return null;
    }

    private int activeTab = 0;

    public int getActiveTab() {
        return activeTab;
    }

    public void setActiveTab(int activeTab) {
        this.activeTab = activeTab;
    }

    public void oplsLoadBtn_action() {
        ChemoMetrics.PlotOplsSplot(sb, sb.getNewImage("opls_splot"), "png", 72, loadOpt);
    }

    public String oplsPermBtn_action() {
        //permMsg = ChemoMetrics.OPLSPermute(sb, permNum, permStat);
        permMsg = ChemoMetrics.PlotOPLSPermutation(sb, sb.getNewImage("opls_perm"), "png", 72, permNum);
        RequestContext.getCurrentInstance().scrollTo("ac:form7:permPane");
        return null;
    }

    public void updateOrthoPLSDA() {
        ChemoMetrics.InitOPLS(sb);
        ChemoMetrics.PlotOPLS2DScore(sb, sb.getNewImage("opls_score2d"), "png", 72, 1, 2, 0.95, 1, 0);
        ChemoMetrics.PlotOplsSplot(sb, sb.getNewImage("opls_splot"), "png", 72, "all");
        ChemoMetrics.PlotOplsMdlView(sb, sb.getNewImage("opls_mdl"), "png", 72);
    }

    public void updateSplot() {
        ChemoMetrics.PlotOplsSplot(sb, sb.getCurrentImage("opls_splot"), "png", 72, loadOpt);
        if (loadOpt.equals("custom")) {
            sb.updateMsg("OK","Please first click the points of interest and then re-gerenate the Splot in Image Center");
        } else {
            sb.updateMsg("OK", "You can now re-generate the plot using the Image Center");
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
        double[][] mat = RDataUtils.getOPLSLoadMat(RC); //must be called first
        for (double[] row : mat) {
            dots.set(row[0], row[1]);
        }

        sb.setFeatureLabels(RDataUtils.getOPLSLoadCmpds(RC));
        sb.setFeatureInx(RDataUtils.getOPLSLoadCmpdInxs(RC));
        double[] rgx = RDataUtils.getOPLSLoadAxesSpec(RC);
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
        //xAxis.setLabel("Loadings " + plsLoadX);
        xAxis.setMin(loadMinX);
        xAxis.setMax(loadMaxX);

        Axis yAxis = ldModel.getAxis(AxisType.Y);
        //yAxis.setLabel("Loadings " + plsLoadY);
        yAxis.setMin(loadMinY);
        yAxis.setMax(loadMaxY);
    }

    public String getDownloadLink() {
        return "<a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName() + "/oplsda_model.csv\"><b>" + "Details" + "</b></a>";
    }

}