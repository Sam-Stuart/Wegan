/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.File;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.UniVarTests;
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
@ManagedBean(name = "univBean")
@ViewScoped
public class UnivBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private String pairedFcAnal = "FALSE";

    public String getPairedFcAnal() {
        return pairedFcAnal;
    }

    public void setPairedFcAnal(String pairedAnal) {
        this.pairedFcAnal = pairedAnal;
    }

    private SelectItem[] cmpOpts = null;

    public SelectItem[] getCmpOpts() {
        if (cmpOpts == null) {
            String[] grpNms = RDataUtils.getNormGroupNames(sb.getRConnection());
            int grpLen = grpNms.length;
            if (grpLen == 2) {
                cmpOpts = new SelectItem[grpLen];
                cmpOpts[0] = new SelectItem(0, grpNms[0] + "/" + grpNms[1]);
                cmpOpts[1] = new SelectItem(1, grpNms[1] + "/" + grpNms[0]);
            } else {
                cmpOpts = new SelectItem[]{new SelectItem("NULL", "<Not set>")};
            }
        }
        return cmpOpts;
    }

    private int cmpType = 0;

    public int getCmpType() {
        return cmpType;
    }

    public void setCmpType(int cmpType) {
        this.cmpType = cmpType;
    }

    private String viewOpt = "overview";

    public String getViewOpt() {
        return viewOpt;
    }

    public void setViewOpt(String viewOpt) {
        this.viewOpt = viewOpt;
    }

    private String fcThresh = "2";
    private String countThresh = "75";
    private int plotLbl = 1;

    public int getPlotLbl() {
        return plotLbl;
    }

    public void setPlotLbl(int plotLbl) {
        this.plotLbl = plotLbl;
    }

    public String getFcThresh() {
        return fcThresh;
    }

    public void setFcThresh(String fcThresh) {
        this.fcThresh = fcThresh;
    }

    public String getCountThresh() {
        return countThresh;
    }

    public void setCountThresh(String countThresh) {
        this.countThresh = countThresh;
    }

    public String fcButton_action() {
        double fc = Double.parseDouble(fcThresh);
        if (pairedFcAnal.equals("TRUE")) {
            double pairThresh = Double.parseDouble(countThresh) / 100;
            UniVarTests.InitPairedFC(sb, fc, pairThresh, cmpType);
        } else {
            UniVarTests.InitUnpairedFC(sb, fc, cmpType);
        }
        UniVarTests.PlotFC(sb, sb.getNewImage("fc"), "png", 72);
        RequestContext.getCurrentInstance().scrollTo("form1:fcPane");
        return null;
    }

    private String ttPThresh = "0.05";

    public String getTtPThresh() {
        return ttPThresh;
    }

    public void setTtPThresh(String ttPThresh) {
        this.ttPThresh = ttPThresh;
    }

    private String pairedTtAnal = "FALSE";

    public String getPairedTtAnal() {
        return pairedTtAnal;
    }

    public void setPairedTtAnal(String pairedAnal) {
        this.pairedTtAnal = pairedAnal;
    }

    private String equalVar = "TRUE";

    public String getEqualVar() {
        return equalVar;
    }

    public void setEqualVar(String equalVar) {
        this.equalVar = equalVar;
    }

    private boolean nonParTt = false;

    public boolean isNonParTt() {
        return nonParTt;
    }

    public void setNonParTt(boolean nonParTt) {
        this.nonParTt = nonParTt;
    }

    public void ttButton_action() {
        double thresh = Double.parseDouble(ttPThresh);
        String nonpar = "F";
        if (nonParTt) {
            nonpar = "T";
        }
        int res = UniVarTests.performTtests(sb, nonpar, thresh, pairedTtAnal, equalVar);
        if (res == 0) {
            sb.setTtSig(false);
        } else {
            sb.setTtSig(true);
        }
        String msg = RDataUtils.getCurrentMsg(sb.getRConnection());
        updateLineModel("tt");
        UniVarTests.PlotT(sb, sb.getNewImage("tt"), "png", 72);
        sb.updateMsg("OK", msg);
    }

    private String corDirection = "col";

    public String getCorDirection() {
        return corDirection;
    }

    public void setCorDirection(String corDirection) {
        this.corDirection = corDirection;
    }
    
    private String pairedVC = "FALSE";

    public String getPairedVC() {
        return pairedVC;
    }

    public void setPairedVC(String pairedVC) {
        this.pairedVC = pairedVC;
    }

    private double vcPThresh = 0.1;
    private double vcFcThresh = 2;
    private double vcCountThresh = 75;

    public double getVcPThresh() {
        return vcPThresh;
    }

    public void setVcPThresh(double vcPThresh) {
        this.vcPThresh = vcPThresh;
    }

    public double getVcFcThresh() {
        return vcFcThresh;
    }

    public void setVcFcThresh(double vcFcThresh) {
        this.vcFcThresh = vcFcThresh;
    }

    public double getVcCountThresh() {
        return vcCountThresh;
    }

    public void setVcCountThresh(double vcCountThresh) {
        this.vcCountThresh = vcCountThresh;
    }

    private boolean nonParVcTt = false;

    public boolean isNonParVcTt() {
        return nonParVcTt;
    }

    public void setNonParVcTt(boolean nonParVcTt) {
        this.nonParVcTt = nonParVcTt;
    }

    private SelectItem[] normVarNmOpts = null;

    public SelectItem[] getNormVarNmOpts() {
        if (normVarNmOpts == null) {
            String[] varNms = RDataUtils.getNormFeatureNames(sb.getRConnection());
            int colLen = varNms.length;
            SelectItem[] varOpts = new SelectItem[colLen];
            for (int i = 0; i < colLen; i++) {
                varOpts[i] = new SelectItem(varNms[i], varNms[i]);
            }
            normVarNmOpts = varOpts;
        }
        return normVarNmOpts;
    }

    public boolean isVcPair() {
        if (pairedVC.equals("FALSE")) {
            return false;
        }
        return true;
    }

    public String vcButton_action() {
        String nonpar = "F";
        if (nonParVcTt) {
            nonpar = "T";
        }

        double countVal = vcCountThresh / 100;
        UniVarTests.performVolcano(sb, pairedVC, vcFcThresh, cmpType, countVal, nonpar, vcPThresh, equalVar, vcPvalType);
        if (pairedVC.equals("FALSE")) {
            updateVolcanoModel();
        }
        UniVarTests.PlotVolcano(sb, sb.getNewImage("volcano"), plotLbl, "png", 72);
        return null;
    }
    
    private String aovPThresh = "0.05";

    public String getAovPThresh() {
        return aovPThresh;
    }

    public void setAovPThresh(String aovPThresh) {
        this.aovPThresh = aovPThresh;
    }

    private boolean nonParam = false;

    public boolean isNonParam() {
        return nonParam;
    }

    public void setNonParam(boolean nonParam) {
        this.nonParam = nonParam;
    }

    private String posthocType = "fisher";

    public String getPosthocType() {
        return posthocType;
    }

    public void setPosthocType(String posthocType) {
        this.posthocType = posthocType;
    }

    public void aovButton_action() {
        // TODO: Replace with your code
        double sigThresh = Double.parseDouble(aovPThresh);
        String nonPar = "F";
        if (nonParam) {
            nonPar = "T";
        }
        int res = UniVarTests.performANOVA(sb, nonPar, sigThresh, posthocType);
        if (res == 0) {
            sb.setAnovaSig(false);
        } else {
            sb.setAnovaSig(true);
        }
        updateLineModel("aov");
        UniVarTests.PlotAOV(sb, sb.getNewImage("aov"), "png", 72);
        String msg = RDataUtils.getCurrentMsg(sb.getRConnection());
        sb.updateMsg("OK", msg);
    }

    public String getAovImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("aov") + "dpi72.png";
    }

    private boolean fixRange = false;

    public boolean isFixRange() {
        return fixRange;
    }

    public void setFixRange(boolean fixRange) {
        this.fixRange = fixRange;
    }

    private String vcPvalType = "raw";

    public String getVcPvalType() {
        return vcPvalType;
    }

    public void setVcPvalType(String volcanoPvalType) {
        this.vcPvalType = volcanoPvalType;
    }

    private boolean noClust = false;

    public boolean isNoClust() {
        return noClust;
    }

    public void setNoClust(boolean noClust) {
        this.noClust = noClust;
    }

    private String hmDistMeasure = "pearson";

    public String getHmDistMeasure() {
        return hmDistMeasure;
    }

    public void setHmDistMeasure(String hmDistMeasure) {
        this.hmDistMeasure = hmDistMeasure;
    }

    private String colContrast = "bwm";

    public String getColContrast() {
        return colContrast;
    }

    public void setColContrast(String colContrast) {
        this.colContrast = colContrast;
    }

    private String corrDownloadTxt = "<a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
            + "/correlation_table.csv\"><b>" + "Correlation Matrix" + "</b></a>";

    public String getCorrDownloadTxt() {
        return corrDownloadTxt;
    }

    public void setCorrDownloadTxt(String corrDownloadTxt) {
        this.corrDownloadTxt = corrDownloadTxt;
    }
    
    private String pvalDownloadTxt = "<a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
            + "/pval_corr_table.csv\"><b>" + "P-Value Matrix" + "</b></a>";
    
    public String getPvalDownloadTxt() {
        return pvalDownloadTxt;
    }

    public void setPvalDownloadTxt(String pvalDownloadTxt) {
        this.pvalDownloadTxt = pvalDownloadTxt;
    }
    
    public void corrBtn_action() {

        String fix = fixRange ? "T" : "F";
        String clst = noClust ? "T" : "F";
        //String top = topCB.isChecked() ? "T" : "F";
        //int topNum= Integer.parseInt(topFld.getText().toString());
        UniVarTests.PlotCorrHeatMap(sb, sb.getNewImage("corr"), "png", 72, corDirection, hmDistMeasure, colContrast, viewOpt, fix, clst, "F", 999);
        RequestContext.getCurrentInstance().scrollTo("form1:corrPane");
    }

    private String ptnDistMeasure = "pearson";

    public String getPtnDistMeasure() {
        return ptnDistMeasure;
    }

    public void setPtnDistMeasure(String ptnDistMeasure) {
        this.ptnDistMeasure = ptnDistMeasure;
    }

    private String ptnFeature = "NA";

    public String getPtnFeature() {
        return ptnFeature;
    }

    public void setPtnFeature(String ptnFeature) {
        this.ptnFeature = ptnFeature;
    }

    private String ptnTemplate;

    public String getPtnTemplate() {
        return ptnTemplate;
    }

    public void setPtnTemplate(String ptnTemplate) {
        this.ptnTemplate = ptnTemplate;
    }

    public SelectItem[] getTemplatePthOpts() {
        String[] templates = UniVarTests.getTempateNames(sb);
        SelectItem[] opts = new SelectItem[templates.length];

        opts[0] = new SelectItem("na", templates[0]);
        for (int i = 1; i < templates.length; i++) {
            opts[i] = new SelectItem(templates[i], templates[i]);
        }
        return opts;
    }

    private String ptnType;

    public String getPtnType() {
        return ptnType;
    }

    public void setPtnType(String ptnType) {
        this.ptnType = ptnType;
    }

    private String usrPtn;

    public String getUsrPtn() {
        return usrPtn;
    }

    public void setUsrPtn(String usrPtn) {
        this.usrPtn = usrPtn;
    }

    public String ptnBtn_action() {
        String imgName = sb.getNewImage("ptn");
        RConnection RC = sb.getRConnection();
        if (ptnType.equals("featptn")) {
            if (UniVarTests.matchFeaturePattern(sb, ptnDistMeasure, ptnFeature)) {
                UniVarTests.plotMatchedFeatures(sb, imgName, "png", 72);
            } else {
                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        } else if (ptnType.equals("preptn")) {
            if (ptnTemplate.equals("na")) {
                sb.updateMsg("Error", "The first item is group label!");
                return null;
            } else if (UniVarTests.matchPattern(sb, ptnDistMeasure, ptnTemplate)) {
                UniVarTests.plotMatchedFeatures(sb, imgName, "png", 72);
            } else {
                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        } else { //self defined
            if (usrPtn == null || usrPtn.trim().length() == 0) {
                sb.updateMsg("Error", "Please define a pattern first!");
                return null;
            }
            if (UniVarTests.matchPattern(sb, ptnDistMeasure, usrPtn)) {
                UniVarTests.plotMatchedFeatures(sb, imgName, "png", 72);
            } else {
                sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                return null;
            }
        }
        RequestContext.getCurrentInstance().scrollTo("form2:ptnPane");
        return null;
    }

    private LineChartModel lineModel, vcModel;

    public LineChartModel getLineModel() {
        if (lineModel == null) {
            if (sb.isMultiGroup()) {
                updateLineModel("aov");
            } else {
                updateLineModel("tt");
            }
        }
        return lineModel;
    }

    public LineChartModel getVolcanoModel() {
        if (vcModel == null) {
            updateVolcanoModel();
        }
        return vcModel;
    }

    private double maxX = 0;

    public double getMaxX() {
        return maxX;
    }

    private double minX = 0;

    public double getMinX() {
        return minX;
    }

    public void updateLineModel(String type) {
        RConnection RC = sb.getRConnection();
        lineModel = new LineChartModel();
        LineChartSeries up_dots = new LineChartSeries();
        //LineChartSeries ln_dots = new LineChartSeries();
        LineChartSeries dn_dots = new LineChartSeries();
        double[][] upMat;
        double[][] dnMat; //lnMat;

        String[] nodeIDs;
        double[] nodeInx;

        if (type.equals("aov")) {
            upMat = RDataUtils.getAnovaUpMat(RC); //must be called first
            dnMat = RDataUtils.getAnovaDnMat(RC); //must be called first
            nodeIDs = RDataUtils.getAnovaCmpds(RC);
            nodeInx = RDataUtils.getAnovaCmpdInxs(RC);
        } else {
            upMat = RDataUtils.getTtUpMat(RC); //must be called first
            dnMat = RDataUtils.getTtDnMat(RC); //must be called first
            nodeIDs = RDataUtils.getTtCmpds(RC);
            nodeInx = RDataUtils.getTtCmpdInxs(RC);
        }
        sb.setFeatureLabels(nodeIDs);
        sb.setFeatureInx(nodeInx);
        maxX = nodeIDs.length + 1;

        String seriesCols = "c40000, 0085cc, 6e20c1";
        if (upMat[0][0] != -1) {
            for (double[] row : upMat) {
                up_dots.set(row[0], row[1]);
            }
            up_dots.setShowLine(false);
            up_dots.setMarkerStyle("filledCircle', size:'7.0");
            lineModel.addSeries(up_dots);
        } else {
            seriesCols = "0085cc, 6e20c1";
        }

        if (dnMat[0][0] != -1) {
            for (double[] row : dnMat) {
                dn_dots.set(row[0], row[1]);
            }
            dn_dots.setShowLine(false);
            dn_dots.setMarkerStyle("filledCircle', size:'6.0");
            lineModel.addSeries(dn_dots);
        } else {
            seriesCols = "c40000, 6e20c1";
        }

        lineModel.setZoom(true);
        lineModel.setSeriesColors(seriesCols);
        lineModel.setExtender("ext");
        Axis xAxis = lineModel.getAxis(AxisType.X);
        xAxis.setLabel("Compounds");
        xAxis.setMin(0);
        xAxis.setMax(maxX);

        Axis yAxis = lineModel.getAxis(AxisType.Y);
        yAxis.setLabel("-log10(raw P-value)");
        yAxis.setMin(-1);

        //plotCmpd(maxInx);
    }

    private int aovMinX, aovMaxX;

    public int getAovMinX() {
        return aovMinX;
    }

    public void setAovMinX(int aovMinX) {
        this.aovMinX = aovMinX;
    }

    public int getAovMaxX() {
        return aovMaxX;
    }

    public void setAovMaxX(int aovMaxX) {
        this.aovMaxX = aovMaxX;
    }

    public void updateVolcanoModel() {
        RConnection RC = sb.getRConnection();
        vcModel = new LineChartModel();
        LineChartSeries up_dots = new LineChartSeries();
        LineChartSeries hl_dots = new LineChartSeries();
        LineChartSeries vl_dots = new LineChartSeries();
        LineChartSeries vr_dots = new LineChartSeries();
        LineChartSeries dn_dots = new LineChartSeries();
        double[][] upMat, dnMat, hlMat, vlMat, vrMat;

        upMat = RDataUtils.getVolcanoUpMat(RC); //must be called first
        dnMat = RDataUtils.getVolcanoDnMat(RC); //must be called first
        hlMat = RDataUtils.getVolcanoHlMat(RC); //must be called first
        vlMat = RDataUtils.getVolcanoVlMat(RC); //must be called first
        vrMat = RDataUtils.getVolcanoVrMat(RC); //must be called first

        sb.setFeatureLabels(RDataUtils.getVolcanoCmpds(RC));
        sb.setFeatureInx(RDataUtils.getVolcanoCmpdInxs(RC));

        String seriesCols = "c40000, 0085cc, 6e20c1, 6e20c1, 6e20c1";
        if (upMat[0][0] != -1) {
            for (double[] row : upMat) {
                up_dots.set(row[0], row[1]);
            }
            up_dots.setShowLine(false);
            up_dots.setMarkerStyle("filledCircle', size:'7.0");
            vcModel.addSeries(up_dots);
        } else {
            seriesCols = "0085cc, 6e20c1, 6e20c1, 6e20c1";
        }

        if (dnMat[0][0] != -1) {
            for (double[] row : dnMat) {
                dn_dots.set(row[0], row[1]);
            }
            dn_dots.setShowLine(false);
            dn_dots.setMarkerStyle("filledCircle', size:'6.0");
            vcModel.addSeries(dn_dots);
        } else {
            seriesCols = "c40000, 6e20c1, 6e20c1, 6e20c1";
        }

        for (double[] row : hlMat) {
            hl_dots.set(row[0], row[1]);
        }

        double[] rgx = RDataUtils.getVolcanoRangeX(RC);
        minX = rgx[0];
        maxX = rgx[1];
        int maxNum = 0;
        if (Math.abs(minX) > Math.abs(maxX)) {
            maxNum = (int) Math.abs(minX);
        } else {
            maxNum = (int) Math.abs(maxX);
        }
        aovMaxX = maxNum + 1;
        aovMinX = 0 - maxNum - 1;

        for (double[] row : vlMat) {
            vl_dots.set(row[0], row[1]);
        }
        for (double[] row : vrMat) {
            vr_dots.set(row[0], row[1]);
        }

        hl_dots.setShowLine(true);
        hl_dots.setMarkerStyle("dash', size:'0.0");
        vr_dots.setShowLine(true);
        vr_dots.setMarkerStyle("dash', size:'0.0");
        vl_dots.setShowLine(true);
        vl_dots.setMarkerStyle("dash', size:'0.0");

        vcModel.setZoom(true);
        vcModel.setSeriesColors(seriesCols);
        vcModel.setExtender("ext");
        Axis xAxis = vcModel.getAxis(AxisType.X);
        xAxis.setLabel("log2(FC)");
        xAxis.setMin(minX);
        xAxis.setMax(maxX);

        Axis yAxis = vcModel.getAxis(AxisType.Y);
        yAxis.setLabel("-log10(p)");
        yAxis.setMin(-1);
    }

    public void setupVolcano() {
        if (!sb.isAnalInit("Volcano plot")) {
            sb.registerPage("Volcano plot");
            UniVarTests.performVolcano(sb, "FALSE", 2, 0, 0.75, "F", 0.1, "TRUE", "raw");
            UniVarTests.PlotVolcano(sb, sb.getCurrentImage("volcano"), 1, "png", 72);
        }
        updateVolcanoModel();
    }

    public void generateUnivReport() {
        UniVarTests.ComputeUnivReport(sb.getRConnection());
    }

    public String getCurrentUserRelativeDir(String downloadFileName) {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + downloadFileName;
    }
}
