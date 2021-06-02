/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.ordination;

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

import metaboanalyst.utils.DataUtils;


@ManagedBean(name = "brayBean")
public class OABrayBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    

    private String fileSampleScores = "bray_curtis_row_scores.csv";
    private String fileSampleScorePath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileSampleScores + "\">" + fileSampleScores + "</a>";
    
    private String fileColScores = "bray_curtis_column_scores.csv";
    private String fileColScorePath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileColScores + "\">" + fileColScores + "</a>";
    
    private String fileMat = "bray_curtis_" + getDistOpts() + "_dissimilarity_matrix.csv";
    private String fileMatPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileMat + "\">" + fileMat + "</a>";
    private String fileScree = "bray_curtis_scree_data.csv";
    private String fileScreePath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileScree + "\">" + fileScree + "</a>";
            
    private boolean doOriginal = false; 
    private boolean doAbundance = false; 
    
    private boolean varArrows = false; 
    private boolean sampleNames = false;
    private boolean addEllipse = false;
    
    private boolean dataAbsence = false; 
    
    private boolean pointStyle = false; 
    
    private String distOpts = "euclidean";
    private String groupCol = "null";
    private String groupPoint = "null";
    private String color = "null";
    
    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }
    
    public String getGroupCol() {
        return groupCol;
    }

    public void setGroupCol(String groupCol) {
        this.groupCol = groupCol;
    }
    
    public String getGroupPoint() {
        return groupPoint;
    }

    public void setGroupPoint(String groupPoint) {
        this.groupPoint = groupPoint;
    }
    
    public String getDistOpts() {
        return distOpts;
    }

    public void setDistOpts(String distOpts) {
        this.distOpts = distOpts;
    }
    
    public boolean isVarArrows() {
        return varArrows;
    }
    
    public void setVarArrows(boolean varArrows) {
        this.varArrows = varArrows;
    }
    
    public boolean isPointStyle() {
        return pointStyle;
    }
    
    public void setPointStyle(boolean pointStyle) {
        this.pointStyle = pointStyle;
    }
    
    public boolean isAddEllipse() {
        return addEllipse;
    }
    
    public void setAddEllipse(boolean addEllipse) {
        this.addEllipse = addEllipse;
    }
    
    public boolean isSampleNames() {
        return sampleNames;
    }
    
    public void setSampleNames(boolean sampleNames) {
        this.sampleNames = sampleNames;
    }    
    public boolean isDataAbsence() {
        return dataAbsence;
    }
    
    public void setDataAbsence(boolean dataAbsence) {
        this.dataAbsence = dataAbsence;
    }
    
    public boolean isdoAbundance() {
        return doAbundance;
    }
    
    public void setdoAbundance(boolean doAbundance) {
        this.doAbundance = doAbundance;
    }
    
    public boolean isdoOriginal() {
        return doOriginal;
    }
    
    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }

    public String getFileScreePath() {
        return fileScreePath;
    }

    public void setFileScreePath(String fileScreePath) {
        this.fileScreePath = fileScreePath;
    } 
    
    
    public String getFileSampleScorePath() {
        return fileSampleScorePath;
    }

    public void setFileSampleScorePath(String fileSampleScorePath) {
        this.fileSampleScorePath = fileSampleScorePath;
    } 
    
    public String getFileColScorePath() {
        return fileColScorePath;
    }

    public void setFileColScorePath(String fileColScorePath) {
        this.fileColScorePath = fileColScorePath;
    }  
    
    public String getFileMatPath() {
        return fileMatPath;
    }

    public void setFileMatPath(String fileMatPath) {
        this.fileMatPath = fileMatPath;
    }  
    

    
    // ACTION BUTTONS //
    public void brayUpdate_action() {
        OAUtils.CreateBray(sb, doAbundance, distOpts, doOriginal, dataAbsence);
        OAUtils.PlotBray2D(sb, color, addEllipse, varArrows, sampleNames, "NULL", "NULL", "NULL", sb.getNewImage("ord_bray_2D"), "png", 72, "NULL"); 
    }
    
    
    
    
}



