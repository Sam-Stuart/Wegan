/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.Serializable;
import java.util.ArrayList;
import javax.faces.bean.ManagedBean;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.Classifying;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "classBean")
public class ClassificationBean implements Serializable{

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    private int treeNum = 500;
    private int tryNum = 7;
    private int rfRandom = 1;

    public int getRfRandom() {
        return rfRandom;
    }

    public void setRfRandom(int rfRandom) {
        this.rfRandom = rfRandom;
    }
    
    public int getTryNum() {
        return tryNum;
    }

    public void setTryNum(int tryNum) {
        this.tryNum = tryNum;
    }

    public int getTreeNum() {
        return treeNum;
    }

    public void setTreeNum(int treeNum) {
        this.treeNum = treeNum;
    }

    public String rfBn_action() {
        try {

            // index cannot be more than total number of cluster
            if (treeNum < 1 || tryNum > RDataUtils.getNormFeatureNames(sb.getRConnection()).length) {
                return null;
            }
            Classifying.InitRF(sb, treeNum, tryNum, rfRandom);
            Classifying.PlotRFClassication(sb, sb.getNewImage("rf_cls"), "png", 72);
            Classifying.PlotRFCmpd(sb, sb.getNewImage("rf_imp"), "png", 72);
            Classifying.PlotRFOutlier(sb, sb.getNewImage("rf_outlier"), "png", 72);

        } catch (Exception e) {
            e.printStackTrace();
        }
        RequestContext.getCurrentInstance().scrollTo("ac:form1:sumPane");
        return null;
    }

    public String getConfText() {
        return setupConfusionTable();
    }

    private String setupConfusionTable() {
        double[][] confmat = Classifying.GetRFConfusionMat(sb);
        String[] rownames = Classifying.GetRFConfRowNames(sb);
        String[] colnames = Classifying.GetRFConfColNames(sb);
        Double rfOOB = Classifying.GetRFOOB(sb);
        ArrayList<Integer> inxVec = new ArrayList();
        inxVec.add(0);
        inxVec.add(1);
        if (rownames == null || rownames.length == 0) {
            return ("No report was generated.");
        }
        return (setConfTable(" ", confmat, rownames, colnames, inxVec, rfOOB));
    }

    //inx indicate which col of sigmat is int, since default all double
    private static String setConfTable(String lbl, double[][] sigmat, String[] rownames, String[] colnames, ArrayList<Integer> inxs, double err) {
        if (rownames == null || rownames.length == 0) {
            return ("The analysis has not performed yet.");
        } else {
            String str = "<b>The OOB error is " + err + "</b></br>";
            str = str + "<table border=\"1\" cellpadding=\"5\">";
            str = str + "<tr><th>" + lbl + "</th>";
            for (int i = 0; i < colnames.length; i++) {
                str = str + "<th>" + colnames[i] + "</th>";
            }
            str = str + "</tr>";
            for (int i = 0; i < rownames.length; i++) {
                str = str + "<tr><td>" + rownames[i] + "</td>";
                for (int j = 0; j < colnames.length; j++) {
                    if (inxs.contains(j)) {
                        str = str + "<td>" + Math.round(sigmat[i][j]) + "</td>";
                    } else {
                        str = str + "<td>" + sigmat[i][j] + "</td>";
                    }
                }
                str = str + "</tr>";
            }
            str = str + "</table>";
            return str;
        }
    }

    private String validationOpt = "10";

    public String getValidationOpt() {
        return validationOpt;
    }

    public void setValidationOpt(String validationOpt) {
        this.validationOpt = validationOpt;
    }

    public String svmBn_action() {
        Classifying.InitSVMAnal(sb, validationOpt);
        Classifying.PlotSVMClassification(sb, sb.getCurrentImage("svm_cls"), "png", 72);
        Classifying.PlotSVMSigCmpds(sb, sb.getCurrentImage("svm_imp"), "png", 72);
        RequestContext.getCurrentInstance().scrollTo("ac:form1:sumPane");
        return null;
    }

}
