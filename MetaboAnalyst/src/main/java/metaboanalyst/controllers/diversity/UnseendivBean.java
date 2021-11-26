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
@ManagedBean(name="unseendivBean") //
public class UnseendivBean implements Serializable {
    
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
    private boolean doSmallsample = false;
    
    public boolean isdoSmallsample() {
        return doSmallsample;
    }

    public void setdoSmallsample(boolean doSmallsample) {
        this.doSmallsample = doSmallsample;
    }
 
    // static dropdown
    private final SelectItem[] index; // in the view, need to present the options //application bean 
    private String indexchosen = "NULL";
            
    public SelectItem[] getIndex() {
        return index;
    }
    
    public String getIndexchosen() {
        return indexchosen;
    } 

    public void setIndexchosen(String indexchosen) {
        this.indexchosen = indexchosen;
    }
 
    
    private final SelectItem[] plotdata;
    private String plotdatachosen = "NULL";
    
    public SelectItem[] getPlotdata() {
        return plotdata;
    }
    
    public String getPlotdatachosen() {
        return plotdatachosen;
    } 

    public void setPlotdatachosen(String plotdatachosen) {
        this.plotdatachosen = plotdatachosen;
    }
    
    
//    private final SelectItem[] color;
//    private String colorchosen = "NULL";
//    
//    public SelectItem[] getColor() {
//        return color;
//    }
//    
//    public String getColorchosen() {
//        return colorchosen;
//    } 
//
//    public void setColorchosen(String colorchosen) {
//        this.colorchosen = colorchosen;
//    }
 
    
    private final SelectItem[] box_color;
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
    
    
    private final SelectItem[] border_col;
    private String border_colchosen = "NULL";
    
    public SelectItem[] getBorder_col() {
        return border_col;
    }
    
    public String getBorder_colchosen() {
        return border_colchosen;
    } 

    public void setBorder_colchosen(String border_colchosen) {
        this.border_colchosen = border_colchosen;
    }
 
    
    // textbox
//    private String pool = "";
//    
//    public String getPool() {
//        return pool;
//    }
//
//    public void setPool(String pool) {
//        this.pool = pool;
//    }
    
    
    private SelectItem[] poolColOpts = null;
    
    public SelectItem[] getPoolColOpts(){
        String[] columns = DiversityUtils.poolColumn(sb);
        int columnsLen = columns.length;
        poolColOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            poolColOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return poolColOpts;
    }
    
    private String poolColName = getPoolColOpts()[0].getLabel();
    
    public String getPoolColName() {
        return poolColName;
    }

    public void setPoolColName(String poolColName) {
        this.poolColName = poolColName;
    }

    
    
    private String permutations = "100";
    
    public String getPermutations() {
        return permutations;
    }

    public void setPermutations(String permutations) {
        this.permutations = permutations;
    }
    
  
    private String minsize = "3";
    
    public String getMinsize() {
        return minsize;
    }

    public void setMinsize(String minsize) {
        this.minsize = minsize;
    }
    
    
    private String parallel = "1";
    
    public String getParallel() {
        return parallel;
    }

    public void setParallel(String parallel) {
        this.parallel = parallel;
    }
    
    
//    private String fac_data = "";
//    
//    public String getFac_data() {
//        return fac_data;
//    }
//
//    public void setFac_data(String fac_data) {
//        this.fac_data = fac_data;
//    }
    
    
//    private String xlab = "";
//    
//    public String getXlab() {
//        return xlab;
//    }
//
//    public void setXlab(String xlab) {
//        this.xlab = xlab;
//    }
//    
//    
//    private String ylab = "";
//    
//    public String getYlab() {
//        return ylab;
//    }
//
//    public void setYlab(String ylab) {
//        this.ylab = ylab;
//    }

    
    private String fileunfreqresult = "Incidence-based estimates_freq_all sites.csv";
    private String fileunfreqresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileunfreqresult + "\">" + fileunfreqresult + "</a>";
    
    public String getFileunfreqresultpath() {
        return fileunfreqresultpath;
    }

    public void setFileunfreqresultpath(String fileunfreqresultpath) {
        this.fileunfreqresultpath = fileunfreqresultpath;
    }
    
    
    private String fileunfreqinresult = "Incidence-based estimates_freq_selected variable.csv";
    private String fileunfreqinresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileunfreqinresult + "\">" + fileunfreqinresult + "</a>";
    
    public String getFileunfreqinresultpath() {
        return fileunfreqinresultpath;
    }

    public void setFileunfreqinresultpath(String fileunfreqinresultpath) {
        this.fileunfreqinresultpath = fileunfreqinresultpath;
    }
    
    private String fileunfreqinsresult = "Incidence-based estimates_freq_selected variable_small sample.csv";
    private String fileunfreqinsresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileunfreqinsresult + "\">" + fileunfreqinsresult + "</a>";
    
    public String getFileunfreqinsresultpath() {
        return fileunfreqinsresultpath;
    }

    public void setFileunfreqinsresultpath(String fileunfreqinsresultpath) {
        this.fileunfreqinsresultpath = fileunfreqinsresultpath;
    }
    
    private String fileuncountresult = "Abundance-based estimates_counts.csv";
    private String fileuncountresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileuncountresult + "\">" + fileuncountresult + "</a>";
    
    public String getFileuncountresultpath() {
        return fileuncountresultpath;
    }

    public void setFileuncountresultpath(String fileuncountresultpath) {
        this.fileuncountresultpath = fileuncountresultpath;
    }
    
    private String fileunpoolresult = "Pooled values.csv";
    private String fileunpoolresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileunpoolresult + "\">" + fileunpoolresult + "</a>";
    
    public String getFileunpoolresultpath() {
        return fileunpoolresultpath;
    }

    public void setFileunpoolresultpath(String fileunpoolresultpath) {
        this.fileunpoolresultpath = fileunpoolresultpath;
    }
    
    private String fileunextresult = "Extrapolated richness indices.csv";
    private String fileunextresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileunextresult + "\">" + fileunextresult + "</a>";
    
    public String getFileunextresultpath() {
        return fileunextresultpath;
    }

    public void setFileunextresultpath(String fileunextresultpath) {
        this.fileunextresultpath = fileunextresultpath;
    }
    
    private String fileunnumresult = "Number of species for random ordering of sampling units.csv";
    private String fileunnumresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileunnumresult + "\">" + fileunnumresult + "</a>";
    
    public String getFileunnumresultpath() {
        return fileunnumresultpath;
    }

    public void setFileunnumresultpath(String fileunnumresultpath) {
        this.fileunnumresultpath = fileunnumresultpath;
    }
    
    private String fileunquaresult = "Quantile envelopes of permutations_all sites.csv";
    private String fileunquaresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileunquaresult + "\">" + fileunquaresult + "</a>";
    
    public String getFileunquaresultpath() {
        return fileunquaresultpath;
    }

    public void setFileunquaresultpath(String fileunquaresultpath) {
        this.fileunquaresultpath = fileunquaresultpath;
    }
    
    private String fileunquainresult = "Quantile envelopes of permutations_selected variable.csv";
    private String fileunquainresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileunquainresult + "\">" + fileunquainresult + "</a>";
    
    public String getFileunquainresultpath() {
        return fileunquainresultpath;
    }

    public void setFileunquainresultpath(String fileunquainresultpath) {
        this.fileunquainresultpath = fileunquainresultpath;
    }
    
    
    public UnseendivBean() {
//        color = new SelectItem[6];
//        color[0] = new SelectItem("NULL", "Grayscale");
//        color[1] = new SelectItem("green", "Greenscale");
//        color[2] = new SelectItem("red", "Redscale");
//        color[3] = new SelectItem("royalblue", "Bluescale");
//        color[4] = new SelectItem("wheat", "Yellowscale");
//        color[5] = new SelectItem("darkslategray", "Darkgrayscale");
    

        index = new SelectItem[5];
        index[0] = new SelectItem("NULL", "Jack1");
        index[1] = new SelectItem("jack2", "Jack2");
        index[2] = new SelectItem("chao", "Chao");
        index[3] = new SelectItem("boot", "Boot");
        index[4] = new SelectItem("Species", "Species");
        
        plotdata = new SelectItem[4];
        plotdata[0] = new SelectItem("NULL", "Richness");
        plotdata[1] = new SelectItem("S", "Species");
        plotdata[2] = new SelectItem("chao", "Chao");
        plotdata[3] = new SelectItem("ace", "ACE");
                
        box_color = new SelectItem[6];
        box_color[0] = new SelectItem("NULL", "Skyblue");
        box_color[1] = new SelectItem("green", "Green");
        box_color[2] = new SelectItem("turquoise", "Turquoise");
        box_color[3] = new SelectItem("steelblue", "Steelblue");
        box_color[4] = new SelectItem("peach", "Peach");
        box_color[5] = new SelectItem("wheat", "Wheat");
        
        border_col = new SelectItem[6];
        border_col[0] = new SelectItem("NULL", "Blue");
        border_col[1] = new SelectItem("green", "Green");
        border_col[2] = new SelectItem("turquoise", "Turquoise");
        border_col[3] = new SelectItem("steelblue", "Steelblue");
        border_col[4] = new SelectItem("peach", "Peach");
        border_col[5] = new SelectItem("wheat", "Wheat");      
        
    }
    

    // ACTION BUTTON // 
    public void unseendivUpdateBox_action() {
        DiversityUtils.CreateUnseenDiv(sb, doOriginal, poolColName, doSmallsample, indexchosen, permutations, minsize, parallel); 
        DiversityUtils.PlotPoolBoxplot(sb, plotdatachosen, box_colorchosen, border_colchosen, sb.getNewImage("boxplot_richness"), "png", 72, "false");        
    }
    
    public void unseendivUpdateLine_action() {
        DiversityUtils.CreateUnseenDiv(sb, doOriginal, poolColName, doSmallsample, indexchosen, permutations, minsize, parallel); 
        DiversityUtils.PlotUnseenCurve(sb, sb.getNewImage("plot_matrices"), "png", 72, "false");
    }
    
}