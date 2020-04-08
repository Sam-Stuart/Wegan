/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.ComponentBean;
import metaboanalyst.rwrappers.ChemoMetrics;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "splsBean")
@ViewScoped
public class SPLSDABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    private int compNum = 5;
    private int varNum = 10;
    private String compVarOpt = "same";
    private int splsPairNum = 5;
    private int splsScore2dX = 1;
    private int splsScore2dY = 2;
    private boolean displayConfs = true;
    private boolean displayNames = true;
    private boolean displayFeatNames = true;

    private int splsScore3dX = 1;
    private int splsScore3dY = 2;
    private int splsScore3dZ = 3;
    private int rotationAngle = 40;
    private int splsLoadX = 1;
    private int splsLoadY = 2;
    private String viewOpt = "overview";
    private boolean grayScale = false; // for VIP
    private boolean greyScale = false;  //for Score plot
    private String validationOpt = "Mfold";

    public String getValidationOpt() {
        return validationOpt;
    }

    public void setValidationOpt(String validationOpt) {
        this.validationOpt = validationOpt;
    }

    public void onSplsCompNumChange() {
        updateComponentBeans();
    }

    public SelectItem[] getSplsComps() {
        int pcNums = ChemoMetrics.GetMaxPCACompNumber(sb) - 2;
        SelectItem[] items = new SelectItem[pcNums];
        for (int i = 0; i < pcNums; i++) {
            int pcNum = i + 2;
            items[i] = new SelectItem(pcNum, pcNum + "");
        }
        return items;
    }

    public SelectItem[] getSplsAvailableComps() {
        SelectItem[] items = new SelectItem[compNum];
        for (int i = 0; i < compNum; i++) {
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

    public int getSplsPairNum() {
        return splsPairNum;
    }

    public void setSplsPairNum(int splsPairNum) {
        this.splsPairNum = splsPairNum;
    }

    public int getSplsScore2dX() {
        return splsScore2dX;
    }

    public void setSplsScore2dX(int splsScore2dX) {
        this.splsScore2dX = splsScore2dX;
    }

    public int getSplsScore2dY() {
        return splsScore2dY;
    }

    public void setSplsScore2dY(int splsScore2dY) {
        this.splsScore2dY = splsScore2dY;
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

    public int getSplsScore3dX() {
        return splsScore3dX;
    }

    public void setSplsScore3dX(int splsScore3dX) {
        this.splsScore3dX = splsScore3dX;
    }

    public int getSplsScore3dY() {
        return splsScore3dY;
    }

    public void setSplsScore3dY(int splsScore3dY) {
        this.splsScore3dY = splsScore3dY;
    }

    public int getSplsScore3dZ() {
        return splsScore3dZ;
    }

    public void setSplsScore3dZ(int splsScore3dZ) {
        this.splsScore3dZ = splsScore3dZ;
    }

    public int getRotationAngle() {
        return rotationAngle;
    }

    public void setRotationAngle(int splsRotationAngle) {
        this.rotationAngle = splsRotationAngle;
    }

    public int getSplsLoadX() {
        return splsLoadX;
    }

    public void setSplsLoadX(int splsLoadX) {
        this.splsLoadX = splsLoadX;
    }

    public int getSplsLoadY() {
        return splsLoadY;
    }

    public void setSplsLoadY(int splsLoadY) {
        this.splsLoadY = splsLoadY;
    }

    public String getViewOpt() {
        return viewOpt;
    }

    public void setViewOpt(String splsLoadOpt) {
        this.viewOpt = splsLoadOpt;
    }

    public boolean isDisplayFeatNames() {
        return displayFeatNames;
    }

    public void setDisplayFeatNames(boolean displayFeatNames) {
        this.displayFeatNames = displayFeatNames;
    }

    private List<ComponentBean> compBeans = null;

    private void updateComponentBeans() {
        if (compBeans == null) {
            compBeans = new ArrayList();
            for (int i = 0; i < compNum; i++) {
                compBeans.add(new ComponentBean("Component" + (i + 1), varNum));
            }
        } else if (compBeans.size() > compNum) {
            List<ComponentBean> ncompBeans = new ArrayList();
            for (int i = 0; i < compNum; i++) {
                ncompBeans.add(compBeans.get(i));
            }
            compBeans = ncompBeans;
        } else if (compBeans.size() < compNum) {
            for (int i = compBeans.size(); i < compNum; i++) {
                compBeans.add(new ComponentBean("Component" + (i + 1), varNum));
            }
        }
    }

    public List<ComponentBean> getCompBeans() {
        if (compBeans == null) {
            compBeans = new ArrayList();
            for (int i = 0; i < compNum; i++) {
                compBeans.add(new ComponentBean("Component" + (i + 1), varNum));
            }
        }
        return compBeans;
    }

    public void setCompVarNums() {
        RDataUtils.setCompVarNumbers(sb.getRConnection(), compBeans);
        compVarOpt = "specific";
    }

    public String getCompVarOpt() {
        //System.out.println("=======" + compVarOpt + "========");
        return compVarOpt;
    }

    public void setCompVarOpt(String compVarOpt) {
        this.compVarOpt = compVarOpt;
    }

    public int getCompNum() {
        return compNum;
    }

    public void setCompNum(int compNum) {
        this.compNum = compNum;
    }

    public int getVarNum() {
        return varNum;
    }

    public void setVarNum(int varNum) {
        this.varNum = varNum;
    }

    public void updateSPLSDA() {
        ChemoMetrics.InitSPLS(sb, compNum, varNum, compVarOpt);
        ChemoMetrics.PlotSPLSPairSummary(sb, sb.getNewImage("spls_pair"), "png", 72, compNum);
        ChemoMetrics.PlotSPLS2DScore(sb, sb.getNewImage("spls_score2d"), "png", 72, 1, 2, 0.95, 1, 0);

        ChemoMetrics.PlotSPLS3DScore(sb, sb.getNewImage("spls_score3d"), "json", 72, 1, 2, 3);

        ChemoMetrics.PlotSPLSLoading(sb, sb.getNewImage("spls_loading"), "png", 72, 1, viewOpt);
        ChemoMetrics.PlotSPLSDAClassification(sb, sb.getNewImage("spls_cv"), "Mfold", "png", 72);
    }

    public String splsPairBtn_action() {
        ChemoMetrics.PlotSPLSPairSummary(sb, sb.getNewImage("spls_pair"), "png", 72, splsPairNum);
        RequestContext.getCurrentInstance().scrollTo("ac:form1:pairPane");
        return null;
    }

    public String splsScore2dBtn_action() {
        if (splsScore2dX == splsScore2dY) {
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
            ChemoMetrics.PlotSPLS2DScore(sb, sb.getNewImage("spls_score2d"), "png", 72, splsScore2dX, splsScore2dY, conf, displayNames ? 1 : 0, useGreyScale);
            RequestContext.getCurrentInstance().scrollTo("ac:form3:score2dPane");
        }
        return null;
    }

    public void splsScore3dBtn_action() {
        if (compNum > 2) {
            ChemoMetrics.PlotSPLS3DScore(sb, sb.getNewImage("spls_score3d"), "json", 72, 1, 2, 3);
        } else {
            sb.updateMsg("Error", "The current model contains less than 3 components!");
        }
    }

    public void splsLoadBtn_action() {
        ChemoMetrics.PlotSPLSLoading(sb, sb.getNewImage("spls_loading"), "png", 72, splsLoadX, viewOpt);
    }

    public String splsCVBtn_action() {
        ChemoMetrics.PlotSPLSDAClassification(sb, sb.getNewImage("spls_cv"), validationOpt, "png", 72);
        return null;
    }
}
