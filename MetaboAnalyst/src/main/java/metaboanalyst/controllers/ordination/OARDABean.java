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


@ManagedBean(name = "rdaBean")
public class OARDABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
    //LINK TABLES
//    private String fileNMA = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + "/rda_scree_data.csv>";
    private String fileScree = "rda_scree_data.csv";
    private String fileScreePath  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileScree + "\">" + fileScree + "</a>";
        
    public String getFileScreePath() {
        return fileScreePath;
    }

    public void setFileScreePath(String fileScreePath) {
        this.fileScreePath = fileScreePath;
    }
    
    
    private String fileRowScore = "rda_row_scores.csv";
    private String fileRowScorePath  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileRowScore + "\">" + fileRowScore + "</a>";
    
    public String getFileRowScorePath() {
        return fileRowScorePath;
    }

    public void setFileRowScorePath(String fileRowScorePath) {
        this.fileRowScorePath = fileRowScorePath;
    }
    
    
    private String fileEnvScore = "rda_environment_scores.csv";
    private String fileEnvScorePath  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileEnvScore + "\">" + fileEnvScore + "</a>";
        
    public String getFileEnvScorePath() {
        return fileEnvScorePath;
    }

    public void setFileEnvScorePath(String fileEnvScorePath) {
        this.fileEnvScorePath = fileEnvScorePath;
    }
    
    
    private String fileColScore = "rda_column_scores.csv";
    private String fileColScorePath  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileColScore + "\">" + fileColScore + "</a>";
        
    public String getfileColScorePath() {
        return fileColScorePath;
    }

    public void setFileColScorePath(String fileColScorePath) {
        this.fileColScorePath = fileColScorePath;
    } 
    
    
    //CHECKBOXES
    private boolean doOriginal = false; 
        
    public boolean isdoOriginal() {
        return doOriginal;
    }
    
    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
    
    
    private boolean doAbundance = false;
        
    public boolean isdoAbundance() {
        return doAbundance;
    }
    
    public void setdoAbundance(boolean doAbundance) {
        this.doAbundance = doAbundance;
    }
    
    
    private boolean varArrows = false; 
        
    public boolean isVarArrows() {
        return varArrows;
    }
    
    public void setVarArrows(boolean varArrows) {
        this.varArrows = varArrows;
    }  
    
    
    private boolean sampleNames = false;
        
    public boolean isSampleNames() {
        return sampleNames;
    }
    
    public void setSampleNames(boolean sampleNames) {
        this.sampleNames = sampleNames;
    }
     
    
    private boolean addEllipse = false;
        
    public boolean isAddEllipse() {
        return addEllipse;
    }
    
    public void setAddEllipse(boolean addEllipse) {
        this.addEllipse = addEllipse;
    }
        
    
    private boolean envArrows = false;
        
    public boolean isEnvArrows() {
        return envArrows;
    }
    
    public void setEnvArrows(boolean envArrows) {
        this.envArrows = envArrows;
    }
    
    
    private boolean envCentroid = false;
        
    public boolean isEnvCentroid() {
        return envCentroid;
    }
    
    public void setEnvCentroid(boolean envCentroid) {
        this.envCentroid = envCentroid;
    }
    
    
    //DYNAMIC DROPDOWN
    private SelectItem[] rdaMetaColumnOpts = null; //Grouping variable
        
    public SelectItem[] getRdaMetaColumnOpts(){
        String[] columns = OAUtils.GetRDAMetaColumns(sb);
        int columnsLen = columns.length;
        rdaMetaColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            rdaMetaColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return rdaMetaColumnOpts;
    }
        
    private String rdaMetaColumnName = getRdaMetaColumnOpts()[0].getLabel(); 

    public String getRdaMetaColumnName() {
        return rdaMetaColumnName;
    }

    public void setRdaMetaColumnName(String rdaMetaColumnName) {
        this.rdaMetaColumnName = rdaMetaColumnName;
    }
    
    
    //STATIC DROPDOWN
    private String color = "NULL";
        
    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    } 

    
    //TEXT BOX
    private String envDataCol = " ";
        
    public String getEnvDataCol() {
        return envDataCol;
    }

    public void setEnvDataCol(String envDataCol) {
        this.envDataCol = envDataCol;
    } 
    
    

    // ACTION BUTTONS //
    public void rdaUpdate_action() {
        OAUtils.CreateRDA(sb, doAbundance, envDataCol, doOriginal);
        OAUtils.PlotRDA2D(sb, color, varArrows, envArrows, envCentroid, sampleNames, rdaMetaColumnName, addEllipse, sb.getNewImage("ord_rda_2D"), "png", 72);   
        OAUtils.PlotRDAScree(sb, sb.getNewImage("ord_rda_scree"), "png", 72);
    }

    
}



