/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.File;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.FeatureBean;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.TimeSeries;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.UploadedFile;
import org.primefaces.model.chart.Axis;
import org.primefaces.model.chart.AxisType;
import org.primefaces.model.chart.LineChartModel;
import org.primefaces.model.chart.LineChartSeries;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "timeBean")
@SessionScoped
public class TimeBean implements Serializable{

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private UploadedFile tsFile;

    public UploadedFile getTsFile() {
        return tsFile;
    }

    public void setTsFile(UploadedFile file) {
        this.tsFile = file;
    }

    public boolean isTimeOnly() {
        return tsDesign.equals("time0");
    }
    
    public boolean isAovOptDisabled() {
        return true;
    }

    private String timeDataOpt = "time2";

    public String getTimeDataOpt() {
        return timeDataOpt;
    }

    private boolean useInteract = true;

    public boolean isUseInteract() {
        return useInteract;
    }

    public void setUseInteract(boolean useInteract) {
        this.useInteract = useInteract;
    }
    
    public void setTimeDataOpt(String timeDataOpt) {
        this.timeDataOpt = timeDataOpt;
    }

    private int aovOpt = 1;

    public int getAovOpt() {
        return aovOpt;
    }

    public void setAovOpt(int aovOpt) {
        this.aovOpt = aovOpt;
    }
    
    private String tsDataType = "conc";

    public String getTsDataType() {
        return tsDataType;
    }

    public void setTsDataType(String tsDataType) {
        this.tsDataType = tsDataType;
    }

    private String tsDesign = "time";

    private UploadedFile csvFile;

    public UploadedFile getCsvFile() {
        return csvFile;
    }

    public void setCsvFile(UploadedFile csvFile) {
        this.csvFile = csvFile;
    }

    public String getTsDesign() {
        return tsDesign;
    }

    public void setTsDesign(String tsDesign) {
        this.tsDesign = tsDesign;
    }

    private String tsFormat = "rowts";

    public String getTsFormat() {
        return tsFormat;
    }

    public void setTsFormat(String tsFormat) {
        this.tsFormat = tsFormat;
    }

    private String viewOpt = "overview";

    public String getViewOpt() {
        return viewOpt;
    }

    public void setViewOpt(String viewOpt) {
        this.viewOpt = viewOpt;
    }

    private boolean drawBorders = false;

    public boolean isDrawBorders() {
        return drawBorders;
    }

    public void setDrawBorders(boolean drawBorders) {
        this.drawBorders = drawBorders;
    }

        
    private String dataNames = "colOnly";

    public String getDataNames() {
        return dataNames;
    }

    public void setDataNames(String dataNames) {
        this.dataNames = dataNames;
    }
    
    public String handleTsDataUpload() {

        if (sb.doLogin(tsDataType, "ts", false, false)) {
            try {
                RConnection RC = sb.getRConnection();
                RDataUtils.setDesignType(RC, tsDesign);
                String fileName = DataUtils.uploadFile(csvFile, sb, null, ab.isOnPublicServer());

                if (RDataUtils.readTextData(RC, fileName, tsFormat, "disc", dataNames)) {
                    sb.setDataUploaded(true);
                    return "Data check";
                } else {
                    sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                    return null;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
        return null;
    }

    public String handleTestDataUpload() {
        String fileName;
        if (timeDataOpt.equals("time2")) {
            tsDataType = "pktable";
            tsDesign = "time";
            tsFormat = "colts";
            fileName = ab.getTestTimeSeriesData();
        } else {
            tsDataType = "pktable";
            tsDesign = "time0";
            tsFormat = "rowts";
            fileName = ab.getTimeOnlyPath();
        }
        if (sb.doLogin(tsDataType, "ts", false, false)) {
            try {
                RConnection RC = sb.getRConnection();
                RDataUtils.setDesignType(RC, tsDesign);
                if (RDataUtils.readTextData(RC, fileName, tsFormat, "disc", dataNames)) {
                    sb.setDataUploaded(true);
                    return "Data check";
                } else {
                    sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
                    return null;
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
        return null;
    }

    public void doDefaultAnalysis(String pageID) {

        if (!sb.isAnalInit(pageID)) {
            if (pageID.equals("iPCA")) {
                initPCA3D();
            } else if (pageID.equals("Heatmap2")) {
                doDefaultHeatmap2();
            } else if (pageID.equals("ANOVA2")) {
                doDefaultANOVA2();
            } else if (pageID.equals("ASCA")) {
                doDefaultASCA();
            } else if (pageID.equals("MEBA")) {
                doDefaultMEBA();
            }
            sb.registerPage(pageID);
        }
    }

    private void doDefaultHeatmap2() {
        TimeSeries.plotHeatMap2(sb, sb.getCurrentImage("heatmap2"), "png", 72, "euclidean", "ward.D", "bwm", "overview", 1, "F", "F");
    }

    private void doDefaultANOVA2() {
        int res = TimeSeries.InitANOVA2(sb.getRConnection(), 0.05, "fdr", tsDesign, 1, 1);
        if (res == 0) {
            sb.updateMsg("Error", RDataUtils.getErrMsg(sb.getRConnection()));
        } else {
            TimeSeries.PlotAOV2(sb, sb.getNewImage("aov2"), "png", 72);
        }
    }

    private void doDefaultASCA() {
        RConnection RC = sb.getRConnection();
        TimeSeries.performASCA(RC, 1, 1, 2, 2);

        TimeSeries.plotASCAscree(sb, sb.getCurrentImage("asca_scree"), "png", 72);

        String colorBW = "FALSE";
        TimeSeries.plotASCAModels(sb, sb.getCurrentImage("asca_fa"), "png", 72, "a", colorBW);
        TimeSeries.plotASCAModels(sb, sb.getCurrentImage("asca_fb"), "png", 72, "b", colorBW);
        TimeSeries.plotASCAInteraction(sb, sb.getCurrentImage("asca_fab"), "png", 72, colorBW);

        TimeSeries.performASCAPermutation(RC, 20);
        TimeSeries.plotASCAPermSummary(sb, sb.getCurrentImage("asca_perm"), "png", 72);

        TimeSeries.performASCAVarSelection(RC, 0.05, 0.90);
        TimeSeries.plotASCAImpVar(sb, sb.getCurrentImage("asca_impa"), "png", 72, "a");
        TimeSeries.plotASCAImpVar(sb, sb.getCurrentImage("asca_impb"), "png", 72, "b");
        TimeSeries.plotASCAImpVar(sb, sb.getCurrentImage("asca_impab"), "png", 72, "ab");
    }
    /*
     * PCA 3D visualization 
     */
    private String colOpt = "facA";

    public String getColOpt() {
        return colOpt;
    }

    public void setColOpt(String colOpt) {
        this.colOpt = colOpt;
    }

    public void initPCA3D() {
        TimeSeries.initIPCA(sb.getRConnection(), sb.getNewImage("ipca_3d") + ".json");
    }

    /**
     * Heatmap2
     */
    private String distOpt;

    public String getDistOpt() {
        return distOpt;
    }

    public void setDistOpt(String distOpt) {
        this.distOpt = distOpt;
    }

    private String clusterOpt;

    public String getClusterOpt() {
        return clusterOpt;
    }

    public void setClusterOpt(String clusterOpt) {
        this.clusterOpt = clusterOpt;
    }

    private String colorOpt;

    public String getColorOpt() {
        return colorOpt;
    }

    public void setColorOpt(String colorOpt) {
        this.colorOpt = colorOpt;
    }

    private int smplSortOpt = 1;

    public int getSmplSortOpt() {
        return smplSortOpt;
    }

    public void setSmplSortOpt(int smplSortOpt) {
        this.smplSortOpt = smplSortOpt;
    }

    private boolean useAnovaFeature = false;

    public boolean isUseAnovaFeature() {
        return useAnovaFeature;
    }

    public void setUseAnovaFeature(boolean useAnovaFeature) {
        this.useAnovaFeature = useAnovaFeature;
    }

    public void hm2Bn_action() {
        if (viewOpt.equals("detail")) {
            if (RDataUtils.getNormFeatureNumber(sb.getRConnection()) > 2000) {
                viewOpt = "overview";
                sb.updateMsg("Warning", "Too many features (over 2000) - reset to overview.");
            }
        }
        if (useAnovaFeature) {
            if (!sb.isAnalInit("ANOVA2")) {
                sb.updateMsg("Error", "You need to perform ANOVA2 analysis first!");
                useAnovaFeature = false;
                return;
            }
        }
        TimeSeries.plotHeatMap2(sb, sb.getNewImage("heatmap2"), "png", 72, distOpt, clusterOpt, colorOpt, viewOpt, smplSortOpt, useAnovaFeature ? "T" : "F", drawBorders ? "T" : "F");
    }

    public String getHm2Img() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("heatmap2") + "dpi72.png";
    }

    /**
     * ANOVA2
     */
    private double pThresh = 0.05;

    public double getPthresh() {
        return pThresh;
    }

    public void setPthresh(double pThresh) {
        this.pThresh = pThresh;
    }

    private String pvalOpt = "fdr";

    public String getPvalOpt() {
        return pvalOpt;
    }

    public void setPvalOpt(String pvalOpt) {
        this.pvalOpt = pvalOpt;
    }

    public void aov2Bn_action() {
        int useInteraciton = useInteract? 1:0;
        int res = TimeSeries.InitANOVA2(sb.getRConnection(), pThresh, pvalOpt, tsDesign, aovOpt, useInteraciton);
        if (res == 0) {
            sb.updateMsg("Error", RDataUtils.getErrMsg(sb.getRConnection()));
        } else {
            TimeSeries.PlotAOV2(sb, sb.getNewImage("aov2"), "png", 72);
            updateLineModel();
        }
    }

    public String getAov2Img() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("aov2") + "dpi72.png";
    }

    /**
     * ASCA
     */
    private int mdlANum = 1;
    private int mdlBNum = 1;
    private int mdlABNum = 2;
    private int mdlResNum = 2;

    public int getMdlANum() {
        return mdlANum;
    }

    public void setMdlANum(int mdlANum) {
        this.mdlANum = mdlANum;
    }

    public int getMdlBNum() {
        return mdlBNum;
    }

    public void setMdlBNum(int mdlBNum) {
        this.mdlBNum = mdlBNum;
    }

    public int getMdlABNum() {
        return mdlABNum;
    }

    public void setMdlABNum(int mdlABNum) {
        this.mdlABNum = mdlABNum;
    }

    public int getMdlResNum() {
        return mdlResNum;
    }

    public void setMdlResNum(int mdlRes) {
        this.mdlResNum = mdlRes;
    }

    private boolean useGreyCol = false;

    public boolean isUseGreyCol() {
        return useGreyCol;
    }

    public void setUseGreyCol(boolean useGreyCol) {
        this.useGreyCol = useGreyCol;
    }

    private int permNum = 20;

    public int getPermNum() {
        return permNum;
    }

    public void setPermNum(int perNum) {
        this.permNum = perNum;
    }

    private double lvlThresh = 0.90;
    private double alphaThresh = 0.05;

    public double getLvlThresh() {
        return lvlThresh;
    }

    public void setLvlThresh(double lvlThresh) {
        this.lvlThresh = lvlThresh;
    }

    public double getAlphaThresh() {
        return alphaThresh;
    }

    public void setAlphaThresh(double alphaThresh) {
        this.alphaThresh = alphaThresh;
    }

    public void mdlBtn_action() {
        try {
            TimeSeries.performASCA(sb.getRConnection(), mdlANum, mdlBNum, mdlABNum, mdlResNum);

            // BHan: for the black/white code
            String colorBW = useGreyCol ? "TRUE" : "FALSE";

            TimeSeries.plotASCAModels(sb, sb.getNewImage("asca_fa"), "png", 72, "a", colorBW);
            TimeSeries.plotASCAModels(sb, sb.getNewImage("asca_fb"), "png", 72, "b", colorBW);
            TimeSeries.plotASCAInteraction(sb, sb.getNewImage("asca_fab"), "png", 72, colorBW);

        } catch (NumberFormatException e) {
            e.printStackTrace();
        }
    }

    public void permBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        TimeSeries.performASCAPermutation(sb.getRConnection(), permNum);
        TimeSeries.plotASCAPermSummary(sb, sb.getNewImage("asca_perm"), "png", 72);
    }

    public String sigBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        try {
            TimeSeries.performASCAVarSelection(sb.getRConnection(), alphaThresh, lvlThresh);

            TimeSeries.plotASCAImpVar(sb, sb.getNewImage("asca_impa"), "png", 72, "a");
            TimeSeries.plotASCAImpVar(sb, sb.getNewImage("asca_impb"), "png", 72, "b");
            TimeSeries.plotASCAImpVar(sb, sb.getNewImage("asca_impab"), "png", 72, "ab");

        } catch (NumberFormatException e) {
            e.printStackTrace();
            return null;
        }
        return null;
    }

    /**
     * MEBA
     *
     * @return
     */
    private FeatureBean[] featureBeans = null;
    private String downloadTxt;
    private String cmpdName;

    public String getCmpdName() {
        return cmpdName;
    }

    public void setCmpdName(String cmpdName) {
        this.cmpdName = cmpdName;
    }

    public FeatureBean[] getFeatureBeans() {
        return featureBeans;
    }

    public void setFeatureBeans(FeatureBean[] featureBeans) {
        this.featureBeans = featureBeans;
    }

    public String getDownloadTxt() {
        return downloadTxt;
    }

    private void doDefaultMEBA() {
        RConnection RC = sb.getRConnection();
        int res = TimeSeries.performMB(RC, 10);
        if (res == 0) {
            String msg = "Please make sure data are balanced for time-series analysis. In particular, "
                    + "for each time point, all experiments must exist and cannot be missing!";
            sb.updateMsg("Error", msg);
            featureBeans = null;
            downloadTxt = "No results";
            return;
        }
        String[] rownames = TimeSeries.getMBSigRowNames(RC);
        String[] colnames = TimeSeries.getMBSigColNames(RC);
        double[][] sigmat = TimeSeries.getMBSigMat(RC);

        //set up content
        if (rownames == null || rownames.length == 0) {
            featureBeans = null;
            downloadTxt = "No results";
            sb.updateMsg("Error", "No result data was found!");
        } else {
            //now set up the feature beans
            featureBeans = new FeatureBean[rownames.length];
            FeatureBean fb;

            for (int i = 0; i < rownames.length; i++) {
                fb = new FeatureBean();
                fb.addName(rownames[i]);

                for (int m = 0; m < colnames.length; m++) {
                    fb.addValue(sigmat[i][m]);
                }
                featureBeans[i] = fb;
            }

            downloadTxt = "<b>You can download the table:</b> <a href = \"/MetaboAnalyst/resources/users/"
                    + sb.getCurrentUser().getName()
                    + File.separator + "meba_sig_features.csv\" target=\"_blank\"><b>" + "here" + "</b></a>";
        }
    }

    public void cmpdLnk_action() {
        TimeSeries.plotMBProfile(sb, cmpdName, "png", 72 + "");
        sb.setCurrentMBname(cmpdName);
        cmpdName = cmpdName.replaceAll("\\/", "_");
    }

    public String getAscaScreeImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("asca_scree") + "dpi72.png";
    }

    public String getAscaFacAImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("asca_fa") + "dpi72.png";
    }

    public String getAscaFacBImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("asca_fb") + "dpi72.png";
    }

    public String getAscaFacABImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("asca_fab") + "dpi72.png";
    }

    public String getAscaPermImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("asca_perm") + "dpi72.png";
    }

    public String getAscaImpAImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("asca_impa") + "dpi72.png";
    }

    public String getAscaImpBImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("asca_impb") + "dpi72.png";
    }

    public String getAscaImpABImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("asca_impab") + "dpi72.png";
    }

    public String getMEBACmpdImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + cmpdName + "_dpi72.png";
    }

    private String cmpdImg = null;
    private LineChartModel lineModel;
    private String[] nodeIDs;

    public String[] getNodeIDs() {
        return nodeIDs;
    }

    public void setNodeIDs(String[] nodeIDs) {
        this.nodeIDs = nodeIDs;
    }

    private double maxX = 0;

    public double getMaxX() {
        return maxX;
    }

    public LineChartModel getLineModel() {
        if (lineModel == null) {
            updateLineModel();
        }
        return lineModel;
    }

    public void updateLineModel() {
        RConnection RC = sb.getRConnection();
        lineModel = new LineChartModel();
        LineChartSeries up_dots = new LineChartSeries();
        LineChartSeries ln_dots = new LineChartSeries();
        LineChartSeries dn_dots = new LineChartSeries();
        double[][] upMat, dnMat, lnMat;
        int maxInx;

        upMat = RDataUtils.getAnova2UpMat(RC); //must be called first
        dnMat = RDataUtils.getAnova2DnMat(RC); //must be called first
        lnMat = RDataUtils.getAnova2LnMat(RC); //must be called first
        nodeIDs = RDataUtils.getAnova2Cmpds(RC);
        maxInx = RDataUtils.getMaxAov2Inx(RC) - 1;

        maxX = nodeIDs.length + 1;

        for (double[] row : upMat) {
            up_dots.set(row[0], row[1]);
        }
        for (double[] row : dnMat) {
            dn_dots.set(row[0], row[1]);
        }
        for (double[] row : lnMat) {
            ln_dots.set(row[0], row[1]);
        }

        up_dots.setShowLine(false);
        up_dots.setMarkerStyle("filledCircle', size:'7.0");
        dn_dots.setShowLine(false);
        dn_dots.setMarkerStyle("filledCircle', size:'6.0");
        ln_dots.setShowLine(true);
        ln_dots.setMarkerStyle("dash', size:'0.0"); //single quote is important to get there!

        lineModel.addSeries(up_dots);
        lineModel.addSeries(ln_dots);
        lineModel.addSeries(dn_dots);
        lineModel.setZoom(true);
        lineModel.setExtender("ext");
        Axis xAxis = lineModel.getAxis(AxisType.X);
        xAxis.setLabel("Compounds");
        xAxis.setMin(0);
        xAxis.setMax(maxX);

        Axis yAxis = lineModel.getAxis(AxisType.Y);
        yAxis.setLabel("-log10(p)");
        yAxis.setMin(-1);
    }

}
