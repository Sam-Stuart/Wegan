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
@ManagedBean(name="accummodelBean") //
public class AccummodelBean implements Serializable {
    
   private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
   private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
   
   private User usr = sb.getCurrentUser();
   private String usrName = usr.getName();
   
 
  
    // check box
    private boolean doOriginal = false; 
    
    public boolean isdoOriginal() {
        return doOriginal;
    }
    
    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
    
    
    private boolean doConditioned = false; 
    
    public boolean isdoConditioned() {
        return doConditioned;
    }
    
    public void setdoConditioned(boolean doConditioned) {
        this.doConditioned = doConditioned;
    }
        
    
    // textbox 
    private String permutations = "100";
    
    public String getPermutations() {
        return permutations;
    }

    public void setPermutations(String permutations) {
        this.permutations = permutations;
    }
    
    
    private String pch = "";
    
    public String getPch() {
        return pch;
    }

    public void setPch(String pch) {
        this.pch = pch;
    }
    
    
    // static dropdown
    private final SelectItem[] gamma; // in the view, need to present the options //application bean 
    private String gammachosen = "NULL";
            
    public SelectItem[] getGamma() {
        return gamma;
    }
    
    public String getGammachosen() {
        return gammachosen;
    } 

    public void setGammachosen(String gammachosen) {
        this.gammachosen = gammachosen;
    }
    
    
    private final SelectItem[] models; // in the view, need to present the options //application bean 
    private String modelschosen = "NULL";
            
    public SelectItem[] getModels() {
        return models;
    }
    
    public String getModelschosen() {
        return modelschosen;
    } 

    public void setModelschosen(String modelschosen) {
        this.modelschosen = modelschosen;
    }
    
    
    private final SelectItem[] object; // in the view, need to present the options //application bean 
    private String objectchosen = "NULL";
            
    public SelectItem[] getObject() {
        return object;
    }
    
    public String getObjectchosen() {
        return objectchosen;
    } 

    public void setObjectchosen(String objectchosen) {
        this.objectchosen = objectchosen;
    }
    
    
    private final SelectItem[] interval; // in the view, need to present the options //application bean 
    private String intervalchosen = "NULL";
            
    public SelectItem[] getInterval() {
        return interval;
    }
    
    public String getIntervalchosen() {
        return intervalchosen;
    } 

    public void setIntervalchosen(String intervalchosen) {
        this.intervalchosen = intervalchosen;
    }
    
    
    private final SelectItem[] type; // in the view, need to present the options //application bean 
    private String typechosen = "NULL";
            
    public SelectItem[] getType() {
        return type;
    }
    
    public String getTypechosen() {
        return typechosen;
    } 

    public void setTypechosen(String typechosen) {
        this.typechosen = typechosen;
    }
    
    
    private final SelectItem[] color; // in the view, need to present the options //application bean 
    private String colorchosen = "NULL";
            
    public SelectItem[] getColor() {
        return color;
    }
    
    public String getColorchosen() {
        return colorchosen;
    } 

    public void setColorchosen(String colorchosen) {
        this.colorchosen = colorchosen;
    }      
            
    
    private final SelectItem[] ci_color; // in the view, need to present the options //application bean 
    private String ci_colorchosen = "NULL";
            
    public SelectItem[] getCi_color() {
        return ci_color;
    }
    
    public String getCi_colorchosen() {
        return ci_colorchosen;
    } 

    public void setCi_colorchosen(String ci_colorchosen) {
        this.ci_colorchosen = ci_colorchosen;
    }
    
    
    private final SelectItem[] ci_type; // in the view, need to present the options //application bean 
    private String ci_typechosen = "NULL";
            
    public SelectItem[] getCi_type() {
        return ci_type;
    }
    
    public String getCi_typechosen() {
        return ci_typechosen;
    } 

    public void setCi_typechosen(String ci_typechosen) {
        this.ci_typechosen = ci_typechosen;
    }
    
    
    private final SelectItem[] box_color; // in the view, need to present the options //application bean 
    private String box_colorchosen = "NULL";
            
    public SelectItem[] getBox_color() {
        return box_color;
    }
    
    public String getBox_colorchosen() {
        return box_colorchosen;
    } 

    public void setBox_colorchosen(String box_colorchosen) {
        this.box_colorchosen = box_colorchosen;
    }
    
    
    private final SelectItem[] line_color;
    private String line_colorchosen = "NULL";
    
    public SelectItem[] getLine_color() {
        return line_color;
    }
    
    public String getLine_colorchosen() {
        return line_colorchosen;
    } 

    public void setLine_colorchosen(String line_colorchosen) {
        this.line_colorchosen = line_colorchosen;
    }
    
    
    private final SelectItem[] pred_color;
    private String pred_colorchosen = "NULL";
    
    public SelectItem[] getPred_color() {
        return pred_color;
    }
    
    public String getPred_colorchosen() {
        return pred_colorchosen;
    } 

    public void setPred_colorchosen(String pred_colorchosen) {
        this.pred_colorchosen = pred_colorchosen;
    }
    
    private String fileexactfreqresult = "Exact Species Accumulation Curve_Frequency.csv";
    private String fileexactfreqresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileexactfreqresult + "\">" + fileexactfreqresult + "</a>";
    
    public String getFileexactfreqresultpath() {
        return fileexactfreqresultpath;
    }

    public void setFileexactfreqresultpath(String fileexactfreqresultpath) {
        this.fileexactfreqresultpath = fileexactfreqresultpath;
    }
    
    private String fileexactcurveresult = "Exact Species Accumulation Curve_Data.csv";
    private String fileexactcurveresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileexactcurveresult + "\">" + fileexactcurveresult + "</a>";
    
    public String getFileexactcurveresultpath() {
        return fileexactcurveresultpath;
    }

    public void setFileexactcurveresultpath(String fileexactcurveresultpath) {
        this.fileexactcurveresultpath = fileexactcurveresultpath;
    }
    
    private String filecollectorcurveresult = "Collector Species Accumulation Curve_Data";
    private String filecollectorcurveresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filecollectorcurveresult + "\">" + filecollectorcurveresult + "</a>";
    
    public String getFilecollectorcurveresultpath() {
        return filecollectorcurveresultpath;
    }

    public void setFilecollectorcurveresultpath(String filecollectorcurveresultpath) {
        this.filecollectorcurveresultpath = filecollectorcurveresultpath;
    }
    
    private String filerandomfreqresult = "Random Species Accumulation Curve_Frequency";
    private String filerandomfreqresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filerandomfreqresult + "\">" + filerandomfreqresult + "</a>";
    
    public String getFilerandomfreqresultpath() {
        return filerandomfreqresultpath;
    }

    public void setFilerandomfreqresultpath(String filerandomfreqresultpath) {
        this.filerandomfreqresultpath = filerandomfreqresultpath;
    }
    
    private String filerandomcurveresult = "Random Species Accumulation Curve_Data.csv";
    private String filerandomcurveresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filerandomcurveresult + "\">" + filerandomcurveresult + "</a>";
    
    public String getFilerandomcurveresultpath() {
        return filerandomcurveresultpath;
    }

    public void setFilerandomcurveresultpath(String filerandomcurveresultpath) {
        this.filerandomcurveresultpath = filerandomcurveresultpath;
    }
    
    private String filecolemancurveresult = "Coleman Species Accumulation Curve_Data.csv";
    private String filecolemancurveresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filecolemancurveresult + "\">" + filecolemancurveresult + "</a>";
    
    public String getFilecolemancurveresultpath() {
        return filecolemancurveresultpath;
    }

    public void setFilecolemancurveresultpath(String filecolemancurveresultpath) {
        this.filecolemancurveresultpath = filecolemancurveresultpath;
    }
    
    private String filerarefreqresult = "Rarefaction Species Accumulation Curve_Frequency.csv";
    private String filerarefreqresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filerarefreqresult + "\">" + filerarefreqresult + "</a>";
    
    public String getFilerarefreqresultpath() {
        return filerarefreqresultpath;
    }

    public void setFilerarefreqresultpath(String filerarefreqresultpath) {
        this.filerarefreqresultpath = filerarefreqresultpath;
    }
    
    private String filerareacmcurveresult = "Rarefaction Species Accumulation Curve_Data.csv";
    private String filerareacmcurveresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filerareacmcurveresult + "\">" + filerareacmcurveresult + "</a>";
    
    public String getFilerareacmcurveresultpath() {
        return filerareacmcurveresultpath;
    }

    public void setFilerareacmcurveresultpath(String filerareacmcurveresultpath) {
        this.filerareacmcurveresultpath = filerareacmcurveresultpath;
    }
    
    private String filenlmfreqresult = "Nonlinear Selfstarting Species Accumulation Curve_Data.csv";
    private String filenlmfreqresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filenlmfreqresult + "\">" + filenlmfreqresult + "</a>";
    
    public String getFilenlmfreqresultpath() {
        return filenlmfreqresultpath;
    }

    public void setFilenlmfreqresultpath(String filenlmfreqresultpath) {
        this.filenlmfreqresultpath = filenlmfreqresultpath;
    }
    
    private String filenlmresdresult = "Nonlinear Selfstarting Species Accumulation Curve_Residuals.csv";
    private String filenlmresdresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filenlmresdresult + "\">" + filenlmresdresult + "</a>";
    
    public String getFilenlmresdresultpath() {
        return filenlmresdresultpath;
    }

    public void setFilenlmresdresultpath(String filenlmresdresultpath) {
        this.filenlmresdresultpath = filenlmresdresultpath;
    }
    
    private String filenlmfitresult = "Nonlinear Selfstarting Species Accumulation Curve_Fitted.csv";
    private String filenlmfitresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filenlmfitresult + "\">" + filenlmfitresult + "</a>";
    
    public String getFilenlmfitresultpath() {
        return filenlmfitresultpath;
    }

    public void setFilenlmfitresultpath(String filenlmfitresultpath) {
        this.filenlmfitresultpath = filenlmfitresultpath;
    }
    
    
    public AccummodelBean() {
        gamma = new SelectItem[5];
        gamma[0] = new SelectItem("NULL", "Jack1");
        gamma[1] = new SelectItem("jack2", "Jack2");
        gamma[2] = new SelectItem("chao", "Chao");
        gamma[3] = new SelectItem("boot", "Boot");
        gamma[4] = new SelectItem("Species", "Species");
        
        models = new SelectItem[4];
        models[0] = new SelectItem("NULL", "Arrhenius");
        models[1] = new SelectItem("gleason", "Gleason");
        models[2] = new SelectItem("gitay", "Gitay");
        models[3] = new SelectItem("lomolino", "Lomolino");
        
        object = new SelectItem[6];
        object[0] = new SelectItem("NULL", "Exact");
        object[1] = new SelectItem("random", "Random");
        object[2] = new SelectItem("coleman", "Coleman");
        object[3] = new SelectItem("collector", "Collector");
        object[4] = new SelectItem("rarefaction", "Rarefaction");
        object[5] = new SelectItem("input_data", "Uploaded data");
        
        interval = new SelectItem[3];
        interval[0] = new SelectItem("NULL", "None");
        interval[1] = new SelectItem("confidence", "Confidence");
        interval[2] = new SelectItem("prediction", "Prediction");
        
        type = new SelectItem[5];
        type[0] = new SelectItem("NULL", "Exact");        
        type[2] = new SelectItem("coleman", "Coleman");
        type[1] = new SelectItem("random", "Random");
        type[3] = new SelectItem("collector", "Collector");
        type[4] = new SelectItem("rarefaction", "Rarefaction");
        
        ci_type = new SelectItem[3];
        ci_type[0] = new SelectItem("NULL", "Polygon");        
        ci_type[2] = new SelectItem("bar", "Bar");
        ci_type[1] = new SelectItem("line", "Line");
        
        color = new SelectItem[6];
        color[0] = new SelectItem("NULL", "Black");
        color[1] = new SelectItem("slateblue", "Slateblue");
        color[2] = new SelectItem("steelblue", "Steelblue");
        color[3] = new SelectItem("royalblue", "Royalblue");
        color[4] = new SelectItem("navyblue", "Navyblue");
        color[5] = new SelectItem("darkslategray", "Darkslategray");
        
        box_color = new SelectItem[6];
        box_color[0] = new SelectItem("NULL", "Skyblue");
        box_color[1] = new SelectItem("palegreen", "Palegreen");
        box_color[2] = new SelectItem("turquoise", "Turquoise");
        box_color[3] = new SelectItem("lightsteelblue", "Lightsteelblue");
        box_color[4] = new SelectItem("peachpuff", "Peachpuff");
        box_color[5] = new SelectItem("wheat", "Wheat");
        
        ci_color = new SelectItem[6];
        ci_color[0] = new SelectItem("NULL", "Gray88");
        ci_color[1] = new SelectItem("honeydew", "Honeydew");
        ci_color[2] = new SelectItem("lightcyan", "Lightcyan");
        ci_color[3] = new SelectItem("thistle", "Thistle");
        ci_color[4] = new SelectItem("ivory", "Ivory");
        ci_color[5] = new SelectItem("azure", "Azure");
                   
        line_color = new SelectItem[6];
        line_color[0] = new SelectItem("NULL", "Red");
        line_color[1] = new SelectItem("coral", "Coral");
        line_color[2] = new SelectItem("brown", "Brown");
        line_color[3] = new SelectItem("salmon", "Salmon");
        line_color[4] = new SelectItem("tomato", "Tomato");
        line_color[5] = new SelectItem("sienna", "Sienna");
       
        pred_color = new SelectItem[6];
        pred_color[0] = new SelectItem("NULL", "Purple");
        pred_color[1] = new SelectItem("violetred", "Violetred");
        pred_color[2] = new SelectItem("orchid", "Orchid");
        pred_color[3] = new SelectItem("maroon", "Maroon");
        pred_color[4] = new SelectItem("hotpink", "Hotpink");
        pred_color[5] = new SelectItem("deeppink", "Deeppink");
    }
   
    
    // ACTION BUTTON // 
    public void AccummodelUpdate_action() {
        DiversityUtils.CreateAccumModelDiv(sb, doOriginal, permutations, doConditioned, gammachosen, modelschosen, objectchosen, intervalchosen);       
        DiversityUtils.PlotAccumCurveDiv(sb, typechosen, colorchosen, ci_colorchosen, ci_typechosen, box_colorchosen, line_colorchosen, pred_colorchosen, pch, sb.getNewImage("Species_Accumulation_Model"), "png", 72, "false");
    } 
    
}