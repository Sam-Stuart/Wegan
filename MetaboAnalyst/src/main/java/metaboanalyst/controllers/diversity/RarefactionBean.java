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
import metaboanalyst.controllers.ApplicationBean1;
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
    
   private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
   private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
   
   //public static final String PROP_SAMPLE_PROPERTY = "sampleProperty";
  // check box
    private boolean doOriginal = false; 
    
    public boolean isdoOriginal() {
        return doOriginal;
    }
    
    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
     
   // check box 
    private Boolean doSe = false;
    
    public Boolean isdoSe() {
        return doSe;
    }

    public void setdoSe(Boolean doSe) {
        this.doSe = doSe;
    }
    
    // textbox 
    private String Sample = " ";
    
    public String setSample() {
        return Sample;
    }

    public void setSample(String Sample) {
        this.Sample = Sample;
    }
    
    // static dropdown
    private String Type = "NULL"; // in the view, need to present the options //application bean 
    
    public String getType() {
        return Type;
    }

    public void setType(String Type) {
        this.Type = Type;
    }
    
    
    private String Margin = "NULL";
    
    public String getMargin() {
        return Margin;
    }

    public void setMargin(String Margin) {
        this.Margin = Margin;
    }
    
    private String Step = " ";
    
    public String getStep() {
        return Step;
    }

    public void setStep(String Step) {
        this.Step = Step;
    }
    
    
    private String Color = "NULL";
    
    public String getColor() {
        return Color;
    }

    public void setColor(String Color) {
        this.Color = Color;
    }
    
    
    private String Colortext = " ";
    
    public String getColortext() {
        return Colortext;
    }

    public void setColortext(String Colortext) {
        this.Colortext = Colortext;
    }
    
    
    // ACTION BUTTON // 
    public void rareUpdate_action() {
        DiversityUtils.CreateRarefactionDiv(sb, doOriginal, Type, Sample, doSe, Margin);       
        DiversityUtils.PlotRarefactionCurveDiversity(sb, Step, Color, Colortext, sb.getNewImage("Rarefaction_Curve"), "png", 72, "false");
    }
    
}