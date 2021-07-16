/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.diversity;

import metaboanalyst.controllers.diversity.*;
import java.io.File;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.DownloadBean;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.DiversityUtils;
import metaboanalyst.utils.DataUtils;

/**
 *
 * @author violet
 */
public class RarefactionBean implements Serializable {
    
    public static final String PROP_SAMPLE_PROPERTY = "sampleProperty";
    
    // check box
    private boolean doOriginal = false; 
    
    public boolean isdoOriginal() {
        return doOriginal;
    }
    
    public void setDoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
     
    
    private Boolean doRareSE = false;
    
    public Boolean isdoRareSE() {
        return doRareSE;
    }

    public void setdoRareSE(Boolean doRareSE) {
        this.doRareSE = doRareSE;
    }
    
    // Textbox 
    private String RareSample = "";
    
    public String getRareSample() {
        return RareSample;
    }

    public void setRareSample(String RareSample) {
        this.RareSample = RareSample;
    }
    
    
    private String RareStep = "";
    
    public String getRareStep() {
        return RareStep;
    }

    public void setRareStep(String RareStep) {
        this.RareStep = RareStep;
    }
    
    
    
    private String RareTypeOpts = "NULL";
    
    public String getRareTypeOpts() {
        return RareTypeOpts;
    }

    public void setRareTypeOpts(String RareTypeOpts) {
        this.RareTypeOpts = RareTypeOpts;
    }
    
    
    private String RareMarginOpts = "NULL";
    
    public String getRareMarginOpts() {
        return RareMarginOpts;
    }

    public void setrareMARGINOpts(String rareMARGINOpts) {
        this.RareMarginOpts = RareMarginOpts;
    }
    
    
    private String RareColorOpts = "NULL";
    
    public String getRareColorOpts() {
        return RareColorOpts;
    }

    public void setRareColorOpts(String RareColorOpts) {
        this.RareColorOpts = RareColorOpts;
    }
    
    
    private String RareColTextOpts = "NULL";
    
    public String getRareColTextOpts() {
        return RareColTextOpts;
    }

    public void setRareColTextOpts(String RareColTextOpts) {
        this.RareColTextOpts = RareColTextOpts;
    }
    
    
    // ACTION BUTTON // 
    public void rareUpdate_action() {
        DiversityUtils.CreateRarefactionDiv(sb, RareTypeOpts, RareSample, doRareSE, RareMarginOpts); 
        DiversityUtils.PlotRarefactionCurveDiversity(sb, RareStep, RareColorOpts, RareColTextOpts, sb.getNewImage("Rarefaction_Curve"), png, 72);
    }
    
}