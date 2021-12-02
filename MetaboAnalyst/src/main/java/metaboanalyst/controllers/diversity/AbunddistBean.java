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
    
    private String filenullresult = "Perimeters of brokenstick models of species abundance.csv";
    private String filenullresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filenullresult + "\">" + filenullresult + "</a>";
    
    public String getFilenullresultpath() {
        return filenullresultpath;
    }

    public void setFilenullresultpath(String filenullresultpath) {
        this.filenullresultpath = filenullresultpath;
    }
    
    private String fileperiresult = "Perimeters of preemption models of species abundance.csv";
    private String fileperiresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileperiresult + "\">" + fileperiresult + "</a>";
    
    public String getFileperiresultpath() {
        return fileperiresultpath;
    }

    public void setFileperiresultpath(String fileperiresultpath) {
        this.fileperiresultpath = fileperiresultpath;
    }
    
    private String filelogresult = "Perimeters of log-Normal models of species abundance.csv";
    private String filelogresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filelogresult + "\">" + filelogresult + "</a>";
    
    public String getFilelogresultpath() {
        return filelogresultpath;
    }

    public void setFilelogresultpath(String filelogresultpath) {
        this.filelogresultpath = filelogresultpath;
    }
    
    private String filezipfresult = "Perimeters of Zipf models of species abundance.csv";
    private String filezipfresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filezipfresult + "\">" + filezipfresult + "</a>";
    
    public String getFilezipfresultpath() {
        return filezipfresultpath;
    }

    public void setFilezipfresultpath(String filezipfresultpath) {
        this.filezipfresultpath = filezipfresultpath;
    }
    
    private String filemanresult = "Perimeters of Zipf-Mandelbrot models of species abundance.csv";
    private String filemanresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filemanresult + "\">" + filemanresult + "</a>";
    
    public String getFilemanresultpath() {
        return filemanresultpath;
    }

    public void setFilemanresultpath(String filemanresultpath) {
        this.filemanresultpath = filemanresultpath;
    }
    
    
    public AbunddistBean() {
        bar_color = new SelectItem[6];
        bar_color[0] = new SelectItem("NULL", "Skyblue");
        bar_color[1] = new SelectItem("gray", "Gray");
        bar_color[2] = new SelectItem("turquoise", "Turquoise");
        bar_color[3] = new SelectItem("slateblue", "Slateblue");
        bar_color[4] = new SelectItem("seagreen", "Seagreen");
        bar_color[5] = new SelectItem("wheat", "Wheat");
                   
        line_color_addFit = new SelectItem[6];
        line_color_addFit[0] = new SelectItem("NULL", "Red");
        line_color_addFit[1] = new SelectItem("coral", "Coral");
        line_color_addFit[2] = new SelectItem("brown", "Brown");
        line_color_addFit[3] = new SelectItem("salmon", "Salmon");
        line_color_addFit[4] = new SelectItem("tomato", "Tomato");
        line_color_addFit[5] = new SelectItem("sienna", "Sienna");
       
        line_color_addPoi = new SelectItem[6];
        line_color_addPoi[0] = new SelectItem("NULL", "Green");
        line_color_addPoi[1] = new SelectItem("olive", "Olive");
        line_color_addPoi[2] = new SelectItem("springgreen", "Spring green");
        line_color_addPoi[3] = new SelectItem("yellowgreen", "Yellow green");
        line_color_addPoi[4] = new SelectItem("gold", "Gold");
        line_color_addPoi[5] = new SelectItem("orange", "Orange");
       
        line_color_addMax = new SelectItem[6];
        line_color_addMax[0] = new SelectItem("NULL", "Purple");
        line_color_addMax[1] = new SelectItem("violetred", "Violetred");
        line_color_addMax[2] = new SelectItem("orchid", "Orchid");
        line_color_addMax[3] = new SelectItem("maroon", "Maroon");
        line_color_addMax[4] = new SelectItem("hotpink", "Hotpink");
        line_color_addMax[5] = new SelectItem("deeppink", "Deeppink");
    }
   
    
    // ACTION BUTTON // 
    public void AbunddistFisherUpdate_action() {
        DiversityUtils.CreateAbundDistDiv(sb, doOriginal, community, doTiesplit, truncate);       
        DiversityUtils.PlotAbundFisherPlotDiv(sb, bar_colorchosen, line_color_addFitchosen, sb.getNewImage("Abundance_Fisher_Dist_Plot"), "png", 72, "false");
    } 
    
    public void AbunddistPrestUpdate_action() {
        DiversityUtils.CreateAbundDistDiv(sb, doOriginal, community, doTiesplit, truncate);       
        DiversityUtils.PlotAbundPrestPlotDiv(sb, bar_colorchosen, line_color_addPoichosen, line_color_addMaxchosen, sb.getNewImage("Abundance_Prest_Dist_Plot"), "png", 72, "false");
    } 
    
    public void AbunddistRankUpdate_action() {
        DiversityUtils.CreateAbundDistDiv(sb, doOriginal, community, doTiesplit, truncate);       
        DiversityUtils.PlotAbundRankPlotDiv(sb, sb.getNewImage("Abundance_Rank_Dist_Plot"), "png", 72, "false");
    } 
    
}