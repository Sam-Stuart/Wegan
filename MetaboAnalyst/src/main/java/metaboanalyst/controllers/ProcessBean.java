/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers;

import metaboanalyst.utils.DataUtils;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import javax.faces.bean.ManagedBean;
import metaboanalyst.rwrappers.RDataUtils;
import org.rosuda.REngine.Rserve.RConnection;

@ManagedBean(name = "procBean")
public class ProcessBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private String msgText;

    public String getMsgText() {
        return msgText;
    }

    private boolean bnDisabled = false;

    public boolean isBnDisabled() {
        return bnDisabled;
    }

    public void setBnDisabled(boolean bnDisabled) {
        this.bnDisabled = bnDisabled;
    }

    private boolean doQCFiltering = false;

    public boolean isDoQCFiltering() {
        return doQCFiltering;
    }

    public void setDoQCFiltering(boolean doQCFiltering) {
        this.doQCFiltering = doQCFiltering;
    }

    private int qcCutoff = 25;

    public int getQcCutoff() {
        return qcCutoff;
    }

    public void setQcCutoff(int qcCutoff) {
        this.qcCutoff = qcCutoff;
    }

    public void performSanityCheck() {
        RConnection RC = sb.getRConnection();
        ArrayList<String> msgVec = new ArrayList();
        String[] msgArray = null;
        try {
            if (sb.getAnalType().equals("mummichog")) {

                if (RDataUtils.sanityCheckMummichogData(RC)) {
                    msgVec.add("Checking data content ...passed ");
                    msgArray = RDataUtils.getSanityCheckMessage(RC);
                    bnDisabled = false;
                } else {
                    msgVec.add("Checking data content ...failed ");
                    msgArray = RDataUtils.getErrorMsgs(RC);
                    bnDisabled = true;
                }

            } else {
                System.out.println("WE ARE HERE YES SIR");
                System.out.println(sb.getAnalType());
                if (RDataUtils.sanityCheckData(RC)) {
                    msgVec.add("Checking data content ...passed ");
                    msgArray = RDataUtils.getSanityCheckMessage(RC);
                    bnDisabled = false;
                    System.out.println(sb.getAnalType());
                } else {
                    msgVec.add("Checking data content ...failed ");
                    msgArray = RDataUtils.getErrorMsgs(RC);
                    bnDisabled = true;
                }
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
        System.out.println(sb.getAnalType());
    }

    public String imputeButton_action() {
        sb.setMultipleGroup(RDataUtils.getGroupNumber(sb.getRConnection()) > 2);
        return "Missing value";
    }

    public String skipButton_action() {

        if (sb.getDataType().equals("mass_sig") || sb.getDataType().equals("mass_all")) {
            return "mzlibview";
        }

        RConnection RC = sb.getRConnection();
        RDataUtils.replaceMin(RC);
        if (!sb.getAnalType().equals("ts")) {
            sb.setMultipleGroup(RDataUtils.getGroupNumber(RC) > 2);
        }
        //sb.setPageInit("sanity");
        sb.setIntegChecked(true);
        sb.setSmallSmplSize(RDataUtils.isSmallSampleSize(RC));
        //sb.setupDataOpts();

        if (RDataUtils.getProcFeatureNumber(RC) > 250) {
            return "Data filter";
        } else if (!sb.getDataType().equalsIgnoreCase("nmds")) {
            return "Normalization";
        } else if (!sb.getDataType().equalsIgnoreCase("conc")) {
            return "Data filter";
        } else {
            return "Normalization";
        }
    }

    private String filterOpt = "iqr";

    public String getFilterOpt() {
        return filterOpt;
    }

    public void setFilterOpt(String filterOpt) {
        this.filterOpt = filterOpt;
    }

    public void filterButton_action() {
        RConnection RC = sb.getRConnection();
        String doQC = "F";
        if (doQCFiltering) {
            doQC = "T";
        }
        int res = RDataUtils.filterVariable(RC, filterOpt, doQC, qcCutoff);
        String msg = RDataUtils.getCurrentMsg(RC);
        if (res == 0) {
            sb.updateMsg("Error", msg);
        } else {
            sb.updateMsg("OK", msg);
        }
    }

    private String nmrAlignText = "";

    public String getNmrAlignText() {
        return nmrAlignText;
    }

    public void setNmrAlignText(String nmrAlignText) {
        this.nmrAlignText = nmrAlignText;
    }

    public String performNMRPeakAlignment() {
        RConnection RC = sb.getRConnection();
        RDataUtils.processPeakList(RC, 0.03);
        setNMRpeakProcTable();
        return null;
    }

    private void setNMRpeakProcTable() {
        String[] msgArray = RDataUtils.getPeaklistProcMessage(sb.getRConnection());
        String msg = "<table face=\"times\" size = \"3\">"
                + "<tr><th> NMR peak processing information </th></tr>";
        for (String msgArray1 : msgArray) {
            msg = msg + "<tr><td align=\"left\">" + msgArray1 + "</td></tr>";
        }
        msg = msg + "</table>";
        nmrAlignText = msg;
    }

    public String nmrNextBn_action() {
        sb.setDataProcessed(true);
        return "Data check";
    }

    private double mzThresh = 0.025;
    private double rtThresh = 30;

    public double getMzThresh() {
        return mzThresh;
    }

    public void setMzThresh(double mzThresh) {
        this.mzThresh = mzThresh;
    }

    public double getRtThresh() {
        return rtThresh;
    }

    public void setRtThresh(double rtThresh) {
        this.rtThresh = rtThresh;
    }

    private String msPeakText = "";

    public String getMsPeakText() {
        return msPeakText;
    }

    public void setMsPeakText(String msPeakText) {
        this.msPeakText = msPeakText;
    }

    private void setMSpeakProcTable() {
        String[] msgArray = RDataUtils.getPeaklistProcMessage(sb.getRConnection());
        String msg = "<table face=\"times\" size = \"3\">"
                + "<tr><th> MS peak processing information </th></tr>";
        for (int i = 0; i < msgArray.length; i++) {
            msg = msg + "<tr><td align=\"left\">" + msgArray[i] + "</td></tr>";
        }
        msg = msg + "</table>";
        msPeakText = msg;
    }

    public String msPeakAlignBn_action() {
        RDataUtils.processPeakList(sb.getRConnection(), mzThresh, rtThresh);
        setMSpeakProcTable();
        sb.setMsPeakAligned(true);
        return null;
    }

    public String msPeakNextBn_action() {
        sb.setDataProcessed(true);
        return "Data check";
    }

    private double fwhmThresh = 30;

    public double getFwhmThresh() {
        return fwhmThresh;
    }

    public void setFwhmThresh(double fwhmThresh) {
        this.fwhmThresh = fwhmThresh;
    }

    private double bwThresh = 20;

    public double getBwThresh() {
        return bwThresh;
    }

    public void setBwThresh(double bwThresh) {
        this.bwThresh = bwThresh;
    }

    private String profOpt = "bin";

    public String getProfOpt() {
        return profOpt;
    }

    public void setProfOpt(String profOpt) {
        this.profOpt = profOpt;
    }

    private String peakIntOpt = "into";

    public String getPeakIntOpt() {
        return peakIntOpt;
    }

    public void setPeakIntOpt(String peakIntOpt) {
        this.peakIntOpt = peakIntOpt;
    }

    public String smplSpecNorm_action() {
        return "Normalization";
    }

    public String msSpecBn_action() {
        //so far let bwThresh same as fewh
        bwThresh = fwhmThresh;
        if (RDataUtils.readMSspec(sb.getRConnection(), "upload", profOpt, fwhmThresh, bwThresh)) {
            RDataUtils.processMSspec(sb.getRConnection(), fwhmThresh, peakIntOpt);
            RDataUtils.plotMSspecSummaryGraph(sb, sb.getNewImage("msrt"), "png", 72);
            setSpecResultInfo();
            sb.setMsSpecAligned(true);
        }
        return null;
    }

    private String msRtText = "";

    public String getMsRtText() {
        return msRtText;
    }

    public void setMsRtText(String msRtText) {
        this.msRtText = msRtText;
    }

    private void setSpecResultInfo() {
        RConnection RC = sb.getRConnection();
        boolean processed = RDataUtils.isSpectraProcessingOK(RC);
        String[] msgArray = RDataUtils.getMSSpectraProcessingMessage(RC);
        String msg = "<table face=\"times\" size = \"3\">"
                + "<tr><th> Summary of spectra processing : </th></tr>";
        if (msgArray == null || msgArray.length == 0) {
            msg = msg + "<tr><td align=\"left\">No peak groups found for retention time correction</td></tr>"
                    + "<tr><td align=\"left\">It is possible that fwhm value is incorrect</td></tr>";
        } else {
            for (int i = 0; i < msgArray.length; i++) {
                msg = msg + "<tr><td align=\"left\">" + msgArray[i] + "</td></tr>";
            }
        }

        msg = msg + "</table>";
        msRtText = msg;
    }

    public String getRtCorImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("msrt") + "dpi72.png";
    }

    public String msSpecNextBn_action() {
        sb.setDataProcessed(true);
        return "Data check";
    }

    private boolean removeMissing = true;

    public boolean isRemoveMissing() {
        return removeMissing;
    }

    public void setRemoveMissing(boolean removeMissing) {
        this.removeMissing = removeMissing;
    }

    private int missingPercent = 50;

    public int getMissingPercent() {
        return missingPercent;
    }

    public void setMissingPercent(int missingPercent) {
        this.missingPercent = missingPercent;
    }

    private String missingImputeOpt = "min";

    public String getMissingImputeOpt() {
        return missingImputeOpt;
    }

    public void setMissingImputeOpt(String missingImputeOpt) {
        this.missingImputeOpt = missingImputeOpt;
    }

    private String replaceVarOpt;

    public String getReplaceVarOpt() {
        return replaceVarOpt;
    }

    public void setReplaceVarOpt(String replaceVarOpt) {
        this.replaceVarOpt = replaceVarOpt;
    }

    private String imputeAlgOpt;

    public String getImputeAlgOpt() {
        return imputeAlgOpt;
    }

    public void setImputeAlgOpt(String imputeAlgOpt) {
        this.imputeAlgOpt = imputeAlgOpt;
    }

    public String performMissingImpute() {
        RConnection RC = sb.getRConnection();
        if (removeMissing) {
            double percent = missingPercent / 100.0;
            RDataUtils.removeVariable4Percent(RC, percent);
        }

        String method = missingImputeOpt;
        switch (missingImputeOpt) {
            case "replaceCol":
                method = replaceVarOpt;
                break;
            case "impute":
                method = imputeAlgOpt;
                break;
        }
        RDataUtils.imputeVariable(RC, method);
        sb.setIntegChecked(true);
        sb.setSmallSmplSize(RDataUtils.isSmallSampleSize(RC));
        //sb.setupDataOpts();
        if (RDataUtils.getProcFeatureNumber(RC) > 250) {
            return "Data filter";
        } else if (!sb.getDataType().equalsIgnoreCase("conc")) {
            return "Data filter";
        } else {
            return "Normalization";
        }
    }

}
