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


@ManagedBean(name = "caBean")
public class OACABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    

    private String fileRowScores = "ca_row_scores.csv";
    private String fileRowScorePath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileRowScores + "\">" + fileRowScores + "</a>";
    
    private String fileColScores = "ca_column_scores.csv";
    private String fileColScorePath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileColScores + "\">" + fileColScores + "</a>";
   
    private String fileScree = "ca_scree_data.csv";
    private String fileScreePath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileScree + "\">" + fileScree + "</a>";
       
    private boolean varArrows = false; 
    
    private boolean doOriginal = false; 
    
    
    private boolean pointStyle = false; 
    
    private String groupPoint = "null";
    private String color = "null";
    private String envDataCol = "null";
    
    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }
    public String getEnvDataCol() {
        return envDataCol;
    }

    public void setEnvDataCol(String envDataCol) {
        this.envDataCol = envDataCol;
    } 

    public String getGroupPoint() {
        return groupPoint;
    }

    public void setGroupPoint(String groupPoint) {
        this.groupPoint = groupPoint;
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
    
    public String getFileScreePath() {
        return fileScreePath;
    }

    public void setFileScreePath(String fileScreePath) {
        this.fileScreePath = fileScreePath;
    } 
    
    
    public String getFileRowScorePath() {
        return fileRowScorePath;
    }

    public void setFileRowScorePath(String fileRowScorePath) {
        this.fileRowScorePath = fileRowScorePath;
    } 
    
    public String getFileColScorePath() {
        return fileColScorePath;
    }

    public void setFileColScorePath(String fileColScorePath) {
        this.fileColScorePath = fileColScorePath;
    }  
    
    public boolean isdoOriginal() {
        return doOriginal;
    }
    
    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
    

    
// ACTION BUTTONS //
    public void caUpdate_action() {
//        OAUtils.CreateBray(sb, "NULL", "NULL", "NULL", "NULL");
//        OAUtils.PlotCCA(sb, color, addEllipse, varArrows, envArrows, envCentroid, sampleNames, groupCol, "NULL", "NULL", sb.getNewImage("ord_cca_2D"), "png", 72, "NULL");
    }
    
    
    
    
}



