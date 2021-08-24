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
@ManagedBean(name="taxodivBean") //
public class TaxodivBean implements Serializable {
    
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
    private boolean doMatch_force = false;
    
    public boolean isdoMatch_force() {
        return doMatch_force;
    }

    public void setdoMatch_force(boolean doMatch_force) {
        this.doMatch_force = doMatch_force;
    }
    
    
    private boolean doVarstep = false;
    
    public boolean isdoVarstep() {
        return doVarstep;
    }

    public void setdoVarstep(boolean doVarstep) {
        this.doVarstep = doVarstep;
    }

    
    private boolean doCheck = false;
    
    public boolean isdoCheck() {
        return doCheck;
    }

    public void setdoCheck(boolean doCheck) {
        this.doCheck = doCheck;
    }
    
            
    // static dropdown
    private final SelectItem[] dis; // in the view, need to present the options //application bean 
    private String dischosen;
            
    public SelectItem[] getDis() {
        return dis;
    }
    
    public String getDischosen() {
        return dischosen;
    } 

    public void setDischosen(String dischosen) {
        this.dischosen = dischosen;
    }
    
    private final SelectItem[] method_hc;
    private String method_hcchosen;
    
    public SelectItem[] getMethod_hc() {
        return method_hc;
    }
    
    public String getMethod_hcchosen() {
        return method_hcchosen;
    } 

    public void setMethod_hcchosen(String method_hcchosen) {
        this.method_hcchosen = method_hcchosen;
    }
          
    
    private final SelectItem[] color;
    private String colorchosen;
    
    public SelectItem[] getColor() {
        return color;
    }
    
    public String getColorchosen() {
        return colorchosen;
    } 

    public void setColorchosen(String colorchosen) {
        this.colorchosen = colorchosen;
    }
        
    
    private final SelectItem[] colorc;
    private String colorcchosen;
    
    public SelectItem[] getColorc() {
        return colorc;
    }
    
    public String getColorcchosen() {
        return colorcchosen;
    } 

    public void setColorcchosen(String colorcchosen) {
        this.colorcchosen = colorcchosen;
    }
    
    private final SelectItem[] colord;
    private String colordchosen;
    
    public SelectItem[] getColord() {
        return colord;
    }
    
    public String getColordchosen() {
        return colordchosen;
    } 

    public void setColordchosen(String colordchosen) {
        this.colordchosen = colordchosen;
    }

    
    private String filetaxadistresult = "Taxanomy Distance.csv";
    private String filetaxadistresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filetaxadistresult + "\">" + filetaxadistresult + "</a>";
    
    public String getFiletaxadistresultpath() {
        return filetaxadistresultpath;
    }

    public void setFiletaxadistresultpath(String filetaxadistresultpath) {
        this.filetaxadistresultpath = filetaxadistresultpath;
    }
    
    private String filetaxaindresult = "Taxanomy Indices.csv";
    private String filetaxaindresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filetaxaindresult + "\">" + filetaxaindresult + "</a>";
    
    public String getFiletaxaindresultpath() {
        return filetaxaindresultpath;
    }

    public void setFiletaxaindresultpath(String filetaxaindresultpath) {
        this.filetaxaindresultpath = filetaxaindresultpath;
    }
    
    private String filedissiresult = "Dissimilarities among trees.csv";
    private String filedissiresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filedissiresult + "\">" + filedissiresult + "</a>";
    
    public String getFiledissiresultpath() {
        return filetaxaindresultpath;
    }

    public void setFiledissiresultpath(String filedissiresultpath) {
        this.filedissiresultpath = filedissiresultpath;
    }
    
    private String filecommatrixresult = "Community Matrix.csv";
    private String filecommatrixresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filecommatrixresult + "\">" + filecommatrixresult + "</a>";
    
    public String getFilecommatrixresultpath() {
        return filecommatrixresultpath;
    }

    public void setFilecommatrixresultpath(String filecommatrixresultpath) {
        this.filecommatrixresultpath = filecommatrixresultpath;
    }
    
    public TaxodivBean() {
        dis = new SelectItem[7];
        dis[0] = new SelectItem("NULL", "Taxonomic hierarchies");
        dis[1] = new SelectItem("euclidean", "Euclidean");
        dis[2] = new SelectItem("maximum", "maximum");
        dis[3] = new SelectItem("manhattan", "Manhattan");
        dis[4] = new SelectItem("canberra", "canberra");
        dis[5] = new SelectItem("binary", "binary");
        dis[6] = new SelectItem("minkowski", "minkowski");
        
        method_hc = new SelectItem[8];
        method_hc[0] = new SelectItem("NULL", "Average");
        method_hc[1] = new SelectItem("ward.D", "Ward.D");
        method_hc[2] = new SelectItem("ward.D2", "Ward.D2");
        method_hc[3] = new SelectItem("single", "Single");
        method_hc[4] = new SelectItem("complete", "Complete");
        method_hc[5] = new SelectItem("mcquitty", "Mcquitty");
        method_hc[6] = new SelectItem("median", "Median");
        method_hc[7] = new SelectItem("centroid", "Centroid");
        
        color = new SelectItem[3];
        color[0] = new SelectItem("NULL", "Grayscale");
        color[1] = new SelectItem("blue", "Bluescale");
        color[2] = new SelectItem("red", "Redscale");    
        
        colorc = new SelectItem[3];
        colorc[0] = new SelectItem("NULL", "Black");
        colorc[1] = new SelectItem("blue", "Blue");
        colorc[2] = new SelectItem("red", "Red");
        
        colord = new SelectItem[3];
        colord[0] = new SelectItem("NULL", "cm.colors(256)");
        colord[1] = new SelectItem("rainbow", "rainbow(256)");
        colord[2] = new SelectItem("brewer", "brewer.pal(8)");
        
        
    }
    
//taxa_tree <- function(mSetObj=NA, color="NULL", imgName, format="png", dpi=72, width=NA) {
    //    Taxonomic_div <- function(mSetObj = NA, data = "false", dis = "NULL", match.force = "false", varstep = "false", method.hc = "NULL", check = "false") {

    // ACTION BUTTON // 
    public void taxodivUpdate_action() {
        DiversityUtils.CreateTaxoDiv(sb, doOriginal, dischosen, doMatch_force, doVarstep, method_hcchosen, doCheck);       
        DiversityUtils.PlotTaxaTree(sb, colorchosen, sb.getNewImage("Taxa_Tree_Plot"), "png", 72, "false");
        DiversityUtils.PlotTaxonScatter(sb, colorcchosen, sb.getNewImage("Taxa_Scatter_Plot"), "png", 72, "false");
        DiversityUtils.PlotTaxonHeatmap(sb, colordchosen, sb.getNewImage("Taxa_Heatmap_Plot"), "png", 72, "false");
    }
    
}