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
@ManagedBean(name="fddivBean") //
public class FddivBean implements Serializable {
    
   private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
   private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
   
   private User usr = sb.getCurrentUser();
   private String usrName = usr.getName();

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
    private boolean doW_abun = false;
    
    public boolean isdoW_abun() {
        return doW_abun;
    }

    public void setdoW_abun(boolean doW_abun) {
        this.doW_abun = doW_abun;
    }
    
    
    private boolean doStand_x = false;
    
    public boolean isdoStand_x() {
        return doStand_x;
    }

    public void setdoStand_x(boolean doStand_x) {
        this.doStand_x = doStand_x;
    }
 
    
//    private boolean doCalc_FDiv = false;
//    
//    public boolean isdoCalc_FDiv() {
//        return doCalc_FDiv;
//    }
//
//    public void setdoCalc_FDiv(boolean doCalc_FDiv) {
//        this.doCalc_FDiv = doCalc_FDiv;
//    }
//
//    
//    private boolean doCalc_FRic = false;
//    
//    public boolean isdoCalc_FRic() {
//        return doCalc_FRic;
//    }
//
//    public void setdoCalc_FRic(boolean doCalc_FRic) {
//        this.doCalc_FRic = doCalc_FRic;
//    }
    
    
    private boolean doStand_FRic = false;
    
    public boolean isdoStand_FRic() {
        return doStand_FRic;
    }

    public void setdoStand_FRic(boolean doStand_FRic) {
        this.doStand_FRic = doStand_FRic;
    }
    
 
//    private boolean doCalc_CWM = false;
//    
//    public boolean isdoCalc_CWM() {
//        return doCalc_CWM;
//    }
//
//    public void setdoCalc_CWM(boolean doCalc_CWM) {
//        this.doCalc_CWM = doCalc_CWM;
//    }
    
    
    private boolean doPrint_pco = false;
    
    public boolean isdoPrint_pco() {
        return doPrint_pco;
    }

    public void setdoPrint_pco(boolean doPrint_pco) {
        this.doPrint_pco = doPrint_pco;
    }
    
    
    private boolean doMessages = false;
    
    public boolean isdoMessages() {
        return doMessages;
    }

    public void setdoMessages(boolean doMessages) {
        this.doMessages = doMessages;
    }
    
          
    // static dropdown
    private final SelectItem[] corr; // in the view, need to present the options //application bean 
    private String corrchosen = "NULL";
            
    public SelectItem[] getCorr() {
        return corr;
    }
    
    public String getCorrchosen() {
        return corrchosen;
    } 

    public void setCorrchosen(String corrchosen) {
        this.corrchosen = corrchosen;
    }
    
    private final SelectItem[] ord;
    private String ordchosen = "NULL";
    
    public SelectItem[] getOrd() {
        return ord;
    }
    
    public String getOrdchosen() {
        return ordchosen;
    } 

    public void setOrdchosen(String ordchosen) {
        this.ordchosen = ordchosen;
    }
          
    
    private final SelectItem[] color;
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
        

    // textbox
    private String w = "";
    
    public String getW() {
        return w;
    }

    public void setW(String w) {
        this.w = w;
    }
    
    
    private String m = "";
    
    public String getM() {
        return m;
    }

    public void setM(String m) {
        this.m = m;
    }
    

    private String asym_bin = "";
    
    public String getAsym_bin() {
        return asym_bin;
    }

    public void setAsym_bin(String asym_bin) {
        this.asym_bin = asym_bin;
    }

    
    private String filefdrichresult = "Functional richness.csv";
    private String filefdrichresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filefdrichresult + "\">" + filefdrichresult + "</a>";
    
    public String getFilefdrichresultpath() {
        return filefdrichresultpath;
    }

    public void setFilefdrichresultpath(String filefdrichresultpath) {
        this.filefdrichresultpath = filefdrichresultpath;
    }
    
    private String filefdevenresult = "Functional evenness.csv";
    private String filefdevenresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filefdevenresult + "\">" + filefdevenresult + "</a>";
    
    public String getFilefdevenresultpath() {
        return filefdevenresultpath;
    }

    public void setFilefdevenresultpath(String filefdevenresultpath) {
        this.filefdevenresultpath = filefdevenresultpath;
    }
    
    private String filefddigresult = "Functional divergence.csv";
    private String filefddigresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filefddigresult + "\">" + filefddigresult + "</a>";
    
    public String getFilefddigresultpath() {
        return filefddigresultpath;
    }

    public void setFilefddigresultpath(String filefddigresultpath) {
        this.filefddigresultpath = filefddigresultpath;
    }
    
    private String filefddisresult = "Functional dispersion.csv";
    private String filefddisresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filefddisresult + "\">" + filefddisresult + "</a>";
    
    public String getFilefddisresultpath() {
        return filefddisresultpath;
    }

    public void setFilefddisresultpath(String filefddisresultpath) {
        this.filefddisresultpath = filefddisresultpath;
    }
    
    private String filefdraoresult = "Rao's quadratic entropy.csv";
    private String filefdraoresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filefdraoresult + "\">" + filefdraoresult + "</a>";
    
    public String getFilefdraoresultpath() {
        return filefdraoresultpath;
    }

    public void setFilefdraoresultpath(String filefdraoresultpath) {
        this.filefdraoresultpath = filefdraoresultpath;
    }
    
    private String filefdcomresult = "Community-level weighted means of trait values.csv";
    private String filefdcomresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filefdcomresult + "\">" + filefdcomresult + "</a>";
    
    public String getFilefdcomresultpath() {
        return filefdcomresultpath;
    }

    public void setFilefdcomresultpath(String filefdcomresultpath) {
        this.filefdcomresultpath = filefdcomresultpath;
    }
    
    private String filefdgowresult = "Gower dissimilarity.csv";
    private String filefdgowresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filefdgowresult + "\">" + filefdgowresult + "</a>";
    
    public String getFilefdgowresultpath() {
        return filefdgowresultpath;
    }

    public void setFilefdgowresultpath(String filefdgowresultpath) {
        this.filefdgowresultpath = filefdgowresultpath;
    }
    
    //options are "podani", "metric", "classic"
    public FddivBean() {
        corr = new SelectItem[4];
        corr[0] = new SelectItem("NULL", "Cailliez");
        corr[1] = new SelectItem("sqrt", "Sqrt");
        corr[2] = new SelectItem("lingoes", "Lingoes");
        corr[3] = new SelectItem("none", "None");
        
        ord = new SelectItem[3];
        ord[0] = new SelectItem("NULL", "Podani");
        ord[1] = new SelectItem("metric", "Metric");
        ord[2] = new SelectItem("classic", "Classic");
        
        color = new SelectItem[3];
        color[0] = new SelectItem("NULL", "Grayscale");
        color[1] = new SelectItem("blue", "Bluescale");
        color[2] = new SelectItem("red", "Redscale");          
        
    }
    
    
    // ACTION BUTTON // 
    public void fddivUpdate_action() {
        DiversityUtils.CreateFdDiv(sb, doOriginal, w, corrchosen, doW_abun, doStand_x, m, doStand_FRic, doPrint_pco, doMessages, asym_bin, ordchosen);       
        DiversityUtils.PlotFdTree(sb, colorchosen, sb.getNewImage("Cluster_Plot"), "png", 72, "false");
    }
    
}//doCalc_FDiv, doCalc_FRic, doCalc_CWM, 