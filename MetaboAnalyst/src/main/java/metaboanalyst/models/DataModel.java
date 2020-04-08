/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.models;

import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.utils.DataUtils;
import metaboanalyst.rwrappers.RAnalUtils;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import metaboanalyst.rwrappers.RDataUtils;
import org.primefaces.model.chart.PieChartModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
public class DataModel implements Serializable {

    private RConnection RC;
    private String name;
    private String dataName;
    private String groupInfo;
    private int smplNum;
    private int geneNum;
    private boolean uploaded = false;
    private boolean processed = false; // normalization
    private boolean normed = false;
    private boolean analDone = false;
    private boolean allDone = false;
    private boolean include = true;
    private String featureType = "NA"; //entrez/refseq as well as built-in affy platform ID, "probe" indicated custome platform
    private boolean autoscaleOpt = false;
    private double sigLevel = 0.1;
    private double fcLevel = 0.0;

    public double getFcLevel() {
        return fcLevel;
    }

    public void setFcLevel(double fcLevel) {
        this.fcLevel = fcLevel;
    }

    public RConnection getRC() {
        return RC;
    }

    public void setRC(RConnection RC) {
        this.RC = RC;
    }

    public int getSmplNum() {
        return smplNum;
    }

    public int getGeneNum() {
        return geneNum;
    }

    public boolean isAllDone() {
        return allDone;
    }

    public void setAllDone() {
        this.allDone = true;
    }

    public boolean isAutoscaleOpt() {
        return autoscaleOpt;
    }

    public void setAutoscaleOpt(boolean autoscaleOpt) {
        this.autoscaleOpt = autoscaleOpt;
    }

    public boolean isAnalDone() {
        return analDone;
    }

    public void setAnalDone(boolean analDone) {
        this.analDone = analDone;
    }

    public DataModel(RConnection RC, String name) {
        this.RC = RC;
        this.name = name;
    }

    public String getDataName() {
        return dataName;
    }

    public String getGroupInfo() {
        return groupInfo;
    }

    public void setDataName(String dataName) {
        this.dataName = dataName;
    }

    public boolean isUploaded() {
        return uploaded;
    }

    public void setUploaded(boolean uploaded) {
        this.uploaded = uploaded;
    }

    public boolean isProcessed() {
        return processed;
    }

    public void setProcessed(boolean processed) {
        this.processed = processed;
    }

    public boolean isNormed() {
        return normed;
    }

    public void setNormed(boolean normed) {
        this.normed = normed;
    }

    public boolean isInclude() {
        return include;
    }

    public void setInclude(boolean include) {
        if (this.include != include) {
            String msg = "Dataset: " + name + " is included";
            if (!include) {
                msg = "Dataset: " + name + " is excluded";
            }
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_INFO, "OK", msg));
            this.include = include;
        }
    }

    private void updateDataInfo() {
        int[] dims = RDataUtils.getDataDims(RC, name);
        geneNum = dims[1];
        smplNum = dims[0];
        //int naNum = dims[2];
        //missingInfo = naNum + " (" + (double) Math.round(100 * naNum / (geneNum * smplNum)) + "%)";
        //int zoNum = dims[3];
        //zeroInfo = zoNum + " (" + (double) Math.round(100 * zoNum / (geneNum * smplNum)) + "%)";

        String[] nms = RDataUtils.getMetaGroupNames(RC, name);
        groupInfo = nms[0] + " vs. " + nms[1];
        /*
        String[] info = RDataUtils.getSampleInfo(RC, name, clsLabel);
        groupInfo = info[0];
        sampleInfo = info[1];
        String[] nms = RDataUtils.getGroupNames(RC, name);
        //int totNum = nms.length * (nms.length - 1) / 2;
        grpOpts = new SelectItem[nms.length];
        for (int m = 0; m < nms.length; m++) {
            String grpOpt = nms[m];
            //String target = grpA + " vs. " + grpB;
            grpOpts[m] = new SelectItem(grpOpt, grpOpt);
        }
        //set up default
        selectedGrp1 = grpOpts[0].getValue().toString();
        selectedGrp2 = grpOpts[1].getValue().toString();
         */
    }

    public String getName() {
        if (name.endsWith(".txt") | name.endsWith(".csv")) {
            return name.substring(0, name.length() - 4);
        }

        return name;
    }

    public String getFullName() {
        return name;
    }

    public void setName(String model) {
        this.name = model;
    }

    public String getUploadIcon() {
        if (uploaded) {
            return "ui-icon-check";
        }
        return "ui-icon ui-icon-arrowthickstop-1-n";
    }

    public String getProcessIcon() {
        if (processed) {
            return "ui-icon-check";
        }
        return "ui-icon ui-icon-wrench";
    }

    public String getNormIcon() {
        if (normed) {
            return "ui-icon-check";
        }
        return "ui-icon ui-icon-gear";
    }

    public String getAnalIcon() {
        if (analDone) {
            return "ui-icon-check";
        }
        return "ui-icon ui-icon-search";
    }

    public String getSummaryIcon() {
        if (allDone) {
            return "ui-icon-check";
        }
        return "ui-icon ui-icon-info";
    }

    /**
     * The methods below are data-specific information Should be handled by the
     * data itself
     */
    public void processMetaData() {

        processed = false;

        ArrayList<String> msgVec = new ArrayList();
        String[] msgArray = null;
        try {
            if (RDataUtils.sanityCheckMetaData(RC, name)) {
                msgVec.add("Checking data content ...passed ");
                msgArray = RDataUtils.getMetaSanityCheckMessage(RC, name);
                processed = true;
                updateDataInfo();
            } else {
                msgVec.add("Checking data content ...failed ");
                msgArray = RDataUtils.getMetaCheckMsg(RC);
            }
        } catch (Exception e) {
            msgVec.add("Checking data content ...failed ");
            e.printStackTrace();
        }

        msgVec.addAll(Arrays.asList(msgArray));

        String msg = "<table face=\"times\" size = \"3\">";
        msg = msg + "<tr><th> Data processing information: " + "</th></tr>";
        for (int i = 0; i < msgVec.size(); i++) {
            msg = msg + "<tr><td align=\"left\">" + msgVec.get(i) + "</td></tr>";
        }
        msg = msg + "</table>";
        msgText = msg;
    }

    private String missingInfo;

    public String getMissingInfo() {
        return missingInfo;
    }

    public String getNormInfo() {
        if (normOpt.equals("none")) {
            if (autoscaleOpt) {
                return ("Autoscale only");
            } else {
                return ("None performed");
            }
        } else {
            if (autoscaleOpt) {
                return ("Log2 transform followed by autoscale");
            } else {
                return ("Log2 transform only");
            }
        }
    }

    private String zeroInfo;

    public String getZeroInfo() {
        return zeroInfo;
    }

    public void plotDataProfile() {
        SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
        RAnalUtils.plotDataProfiles(RC, name, sb.getNewImage("qc_boxplot"), sb.getNewImage("qc_pca"));
    }

    public String getFeatureType() {
        return featureType;
    }

    public void setFeatureType(String featureType) {
        this.featureType = featureType;
    }

    private String normOpt = "none";

    public void setNormOpt(String normOpt) {
        this.normOpt = normOpt;
    }

    public String getNormOpt() {
        return normOpt;
    }

    private String msgText;

    public String getMsgText() {
        return msgText;
    }

    public void performNormalization() {
        normed = false;
        int res = RAnalUtils.performIndNormalization(RC, name, normOpt, (autoscaleOpt) ? 1 : 0);
        if (res == 1) {
            String idOptMsg = RDataUtils.getCurrentMsg(RC);
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_INFO, "Normalization - OK", idOptMsg));
            normed = true;
        } else {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "Failed to perform normalization!"));
        }
    }

    public double getSigLevel() {
        return sigLevel;
    }

    public void setSigLevel(double sigLevel) {
        this.sigLevel = sigLevel;
    }
    private PieChartModel pieModel;

    public PieChartModel getPieModel() {
        return pieModel;
    }

    private void updatePieModel(int count1, int count2) {
        if (pieModel == null) {
            pieModel = new PieChartModel();
        } else {
            pieModel.getData().clear();
        }
        pieModel.getData().put("Sig [" + count1 + "]", count1);
        pieModel.getData().put("Non-Sig [" + count2 + "]", count2);
        pieModel.setLegendPosition("w");
    }
    //number of significant features in individual analysis
    private int deNum;

    public int getDeNum() {
        return deNum;
    }

    public void setDeNum(int num) {
        this.deNum = num;
        if(deNum > 0){
            vennInclude = true;
        }
    }

    //can be used for venn diagram? 
    private boolean vennInclude = false;

    public boolean isVennInclude() {
        return vennInclude;
    }

    public void setVennInclude(boolean vennInclude) {
        if (deNum == 0) {
            if (vennInclude) {
                FacesContext.getCurrentInstance().addMessage(null,
                        new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "Cannot include data without sig. hits!"));
            }
            this.vennInclude = false;
        } else {
            this.vennInclude = vennInclude;
        }
    }

    public void performDEAnalysis() {
        int[] res = RAnalUtils.performLimmaDE(RC, name, sigLevel, fcLevel);
        if (res[0] == 1) {
            setDeNum(res[1]); 
            setAnalDone(true);
            updatePieModel(res[1], res[2]);
        }
    }
}
