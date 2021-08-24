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
@ManagedBean(name="abunddistBean") //
public class AbunddistBean implements Serializable {
    
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
    private boolean doTiesplit = false;
    
    public boolean isdoTiesplit() {
        return doTiesplit;
    }

    public void setdoTiesplit(boolean doTiesplit) {
        this.doTiesplit = doTiesplit;
    }
    
    
    // textbox 
    private String community = "";
    
    public String getCommunity() {
        return community;
    }

    public void setCommunity(String community) {
        this.community = community;
    }
    
    
    private String truncate = "";
    
    public String getTruncate() {
        return truncate;
    }

    public void setTruncate(String truncate) {
        this.truncate = truncate;
    }
    
    // static dropdown
    private final SelectItem[] bar_color; // in the view, need to present the options //application bean 
    private String bar_colorchosen = "NULL";
            
    public SelectItem[] getBar_color() {
        return bar_color;
    }
    
    public String getBar_colorchosen() {
        return bar_colorchosen;
    } 

    public void setBar_colorchosen(String bar_colorchosen) {
        this.bar_colorchosen = bar_colorchosen;
    }
    
    
    private final SelectItem[] line_color_addFit; // in the view, need to present the options //application bean 
    private String line_color_addFitchosen = "NULL";
            
    public SelectItem[] getLine_color_addFit() {
        return line_color_addFit;
    }
    
    public String getLine_color_addFitchosen() {
        return line_color_addFitchosen;
    } 

    public void setLine_color_addFitchosen(String line_color_addFitchosen) {
        this.line_color_addFitchosen = line_color_addFitchosen;
    }
     
    
    private final SelectItem[] line_color_addPoi;
    private String line_color_addPoichosen = "NULL";
    
    public SelectItem[] getLine_color_addPoi() {
        return line_color_addPoi;
    }
    
    public String getLine_color_addPoichosen() {
        return line_color_addPoichosen;
    } 

    public void setLine_color_addPoichosen(String line_color_addPoichosen) {
        this.line_color_addPoichosen = line_color_addPoichosen;
    }
    
    
    private final SelectItem[] line_color_addMax;
    private String line_color_addMaxchosen = "NULL";
    
    public SelectItem[] getLine_color_addMax() {
        return line_color_addMax;
    }
    
    public String getLine_color_addMaxchosen() {
        return line_color_addMaxchosen;
    } 

    public void setLine_color_addMaxchosen(String line_color_addMaxchosen) {
        this.line_color_addMaxchosen = line_color_addMaxchosen;
    }
    
    
//    private String colortext = " ";
//    
//    public String getColortext() {
//        return colortext;
//    }
//
//    public void setColortext(String colortext) {
//        this.colortext = colortext;
//    }
    
    private String filefisherfreqresult = "Fisher frequency table.csv";
    private String filefisherfreqresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filefisherfreqresult + "\">" + filefisherfreqresult + "</a>";
    
    public String getFilefisherfreqresultpath() {
        return filefisherfreqresultpath;
    }

    public void setFilefisherfreqresultpath(String filefisherfreqresultpath) {
        this.filefisherfreqresultpath = filefisherfreqresultpath;
    }
    
    private String filefisheroutputresult = "Fisher log series model output.csv";
    private String filefisheroutputresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filefisheroutputresult + "\">" + filefisheroutputresult + "</a>";
    
    public String getFilefisheroutputresultpath() {
        return filefisheroutputresultpath;
    }

    public void setFilefisheroutputresultpath(String filefisheroutputresultpath) {
        this.filefisheroutputresultpath = filefisheroutputresultpath;
    }
    
    private String filepoissoncoefresult = "Poisson coefficients.csv";
    private String filepoissoncoefresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filepoissoncoefresult + "\">" + filepoissoncoefresult + "</a>";
    
    public String getFilepoissoncoefresultpath() {
        return filepoissoncoefresultpath;
    }

    public void setFilepoissoncoefresultpath(String filepoissoncoefresultpath) {
        this.filepoissoncoefresultpath = filepoissoncoefresultpath;
    }
    
    private String filepoissonfreqresult = "Poisson frequencies by Octave.csv";
    private String filepoissonfreqresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filepoissonfreqresult + "\">" + filepoissonfreqresult + "</a>";
    
    public String getFilepoissonfreqresultpath() {
        return filepoissonfreqresultpath;
    }

    public void setFilepoissonfreqresultpath(String filepoissonfreqresultpath) {
        this.filepoissonfreqresultpath = filepoissonfreqresultpath;
    }
    
    private String filepoissonextraresult = "Poisson_Total extrapolated richness.csv";
    private String filepoissonextraresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filepoissonextraresult + "\">" + filepoissonextraresult + "</a>";
    
    public String getFilepoissonextraresultpath() {
        return filepoissonextraresultpath;
    }

    public void setFilepoissonextraresultpath(String filepoissonextraresultpath) {
        this.filepoissonextraresultpath = filepoissonextraresultpath;
    }
    
    private String filemaxcoefresult = "Max_likelihood coefficients.csv";
    private String filemaxcoefresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filemaxcoefresult + "\">" + filemaxcoefresult + "</a>";
    
    public String getFilemaxcoefresultpath() {
        return filemaxcoefresultpath;
    }

    public void setFilemaxcoefresultpath(String filemaxcoefresultpath) {
        this.filemaxcoefresultpath = filemaxcoefresultpath;
    }
    
    private String filemaxfreqresult = "Max_likelihood frequencies by Octave.csv";
    private String filemaxfreqresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filemaxfreqresult + "\">" + filemaxfreqresult + "</a>";
    
    public String getFilemaxfreqresultpath() {
        return filemaxfreqresultpath;
    }

    public void setFilemaxfreqresultpath(String filemaxfreqresultpath) {
        this.filemaxfreqresultpath = filemaxfreqresultpath;
    }
    
    private String filemaxextraresult = "Max_likelihood_Total extrapolated richness.csv";
    private String filemaxextraresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filemaxextraresult + "\">" + filemaxextraresult + "</a>";
    
    public String getFilemaxextraresultpath() {
        return filemaxextraresultpath;
    }

    public void setFilemaxextraresultpath(String filemaxextraresultpath) {
        this.filemaxextraresultpath = filemaxextraresultpath;
    }
    
    
    public AbunddistBean() {
        bar_color = new SelectItem[6];
        bar_color[0] = new SelectItem("NULL", "skyblue");
        bar_color[1] = new SelectItem("gray", "gray");
        bar_color[2] = new SelectItem("turquoise", "turquoise");
        bar_color[3] = new SelectItem("slateblue", "slateblue");
        bar_color[4] = new SelectItem("seagreen", "seagreen");
        bar_color[5] = new SelectItem("wheat", "wheat");
                   
        line_color_addFit = new SelectItem[6];
        line_color_addFit[0] = new SelectItem("NULL", "red");
        line_color_addFit[1] = new SelectItem("coral", "coral");
        line_color_addFit[2] = new SelectItem("brown", "brown");
        line_color_addFit[3] = new SelectItem("salmon", "salmon");
        line_color_addFit[4] = new SelectItem("tomato", "tomato");
        line_color_addFit[5] = new SelectItem("sienna", "sienna");
       
        line_color_addPoi = new SelectItem[6];
        line_color_addPoi[0] = new SelectItem("NULL", "green");
        line_color_addPoi[1] = new SelectItem("olive", "olive");
        line_color_addPoi[2] = new SelectItem("springgreen", "spring green");
        line_color_addPoi[3] = new SelectItem("yellowgreen", "yellow green");
        line_color_addPoi[4] = new SelectItem("gold", "gold");
        line_color_addPoi[5] = new SelectItem("orange", "orange");
       
        line_color_addMax = new SelectItem[6];
        line_color_addMax[0] = new SelectItem("NULL", "purple");
        line_color_addMax[1] = new SelectItem("violetred", "violetred");
        line_color_addMax[2] = new SelectItem("orchid", "orchid");
        line_color_addMax[3] = new SelectItem("maroon", "maroon");
        line_color_addMax[4] = new SelectItem("hotpink", "hotpink");
        line_color_addMax[5] = new SelectItem("deeppink", "deeppink");
    }
   
    
    // ACTION BUTTON // 
    public void AbunddistUpdate_action() {
        DiversityUtils.CreateAbundDistDiv(sb, doOriginal, community, doTiesplit, truncate);       
        DiversityUtils.PlotAbundFisherPlotDiv(sb, bar_colorchosen, line_color_addFitchosen, sb.getNewImage("Abundance_Fisher_Dist_Plot"), "png", 72, "false");
        DiversityUtils.PlotAbundPrestPlotDiv(sb, bar_colorchosen, line_color_addPoichosen, line_color_addMaxchosen, sb.getNewImage("Abundance_Prest_Dist_Plot"), "png", 72, "false");
    } 
    
}