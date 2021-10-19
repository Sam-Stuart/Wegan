/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.ordination;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.CAUtils;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.RDataUtils;

import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author Louisa Normington
 */
@ManagedBean(name = "ordNMDSBean")
public class OrdinationNMDSBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    
    
    //TABLES FOR DOWNLOAD
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
    
    private String file2DSampleScores = "nmds_2D_sample_scores.csv";
    private String file2DSampleScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file2DSampleScores + "\">" + file2DSampleScores + "</a>";
 
    public String getFile2DSampleScoresPath() {
        return file2DSampleScoresPath;
    }
        
    public void setFile2DSampleScoresPath(String file2DSampleScoresPath) {
        this.file2DSampleScoresPath = file2DSampleScoresPath;
    } 
    
    
    private String file2DColScores = "nmds_2D_variable_scores.csv";
    private String file2DColScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file2DColScores + "\">" + file2DColScores + "</a>";
 
    public String getFile2DColScoresPath() {
        return file2DColScoresPath;
    }
        
    public void setFile2DColScoresPath(String file2DColScoresPath) {
        this.file2DColScoresPath = file2DColScoresPath;
    } 
    
    
    private String file2DEnvScores = "nmds_2D_constraining_variable_scores.csv"; //TABLE SHOULD ONLY POP UP IF ENVDATA IS UPLOADED
    private String file2DEnvScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file2DEnvScores + "\">" + file2DEnvScores + "</a>";
 
    public String getFile2DEnvScoresPath() {
        return file2DEnvScoresPath;
    }
        
    public void setFile2DEnvScoresPath(String file2DEnvScoresPath) {
        this.file2DEnvScoresPath = file2DEnvScoresPath;
    } 
    
    
    private String file3DSampleScores = "nmds_3D_sample_scores.csv";
    private String file3DSampleScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file3DSampleScores + "\">" + file3DSampleScores + "</a>";
 
    public String getFile3DSampleScoresPath() {
        return file3DSampleScoresPath;
    }
        
    public void setFile3DSampleScoresPath(String file3DSampleScoresPath) {
        this.file3DSampleScoresPath = file3DSampleScoresPath;
    } 
    
    
    private String file3DColScores = "nmds_3D_variable_scores.csv";
    private String file3DColScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file3DColScores + "\">" + file3DColScores + "</a>";
 
    public String getFile3DColScoresPath() {
        return file3DColScoresPath;
    }
        
    public void setFile3DColScoresPath(String file3DColScoresPath) {
        this.file3DColScoresPath = file3DColScoresPath;
    } 
    
    
    private String fileScree = "nmds_scree_data.csv";
    private String fileScreePath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileScree + "\">" + fileScree + "</a>";
 
    public String getFileScreePath() {
        return fileScreePath;
    }
        
    public void setFileScreePath(String fileScreePath) {
        this.fileScreePath = fileScreePath;
    } 
    
    
    
    
    //TEXT BOX  
    private String envDataCol = " ";
        
    public String getEnvDataCol() {
        return envDataCol;
    }

    public void setEnvDataCol(String envDataCol) {
        this.envDataCol = envDataCol;
    } 
    
    
    
    
    //CHECKBOX
    private boolean doAbundance = false; 
    
    public boolean isDoAbundance() {
        return doAbundance;
    }
    
    public void setDoAbundance(boolean doAbundance) {
        this.doAbundance = doAbundance;
    }

    
    private boolean doOriginal = false; 
    
    public boolean isDoOriginal() {
        return doOriginal;
    }
    
    public void setDoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }

    
    
    private boolean ellipseOpts = false; 
    
    public boolean isEllipseOpts() {
        return ellipseOpts;
    }
    
    public void setEllipseOpts(boolean ellipseOpts) {
        this.ellipseOpts = ellipseOpts;
    }
        
    
    private boolean var_arrowsOpts = false; 
    
    public boolean isVar_arrowsOpts() {
        return var_arrowsOpts;
    }
    
    public void setVar_arrowsOpts(boolean var_arrowsOpts) {
        this.var_arrowsOpts = var_arrowsOpts;
    }
    
    
        
    private boolean env_arrowsOpts = false; 
    
    public boolean isEnv_arrowsOpts() {
        return env_arrowsOpts;
    }
    
    public void setEnv_arrowsOpts(boolean env_arrowsOpts) {
        this.env_arrowsOpts = env_arrowsOpts;
    }
    
         
    private boolean env_centOpts = false; 
    
    public boolean isEnv_centOpts() {
        return env_centOpts;
    }
    
    public void setEnv_centOpts(boolean env_centOpts) {
        this.env_centOpts = env_centOpts;
    }
     
         
    private boolean sampleNamesOpts = false; 
    
    public boolean isSampleNamesOpts() {
        return sampleNamesOpts;
    }
    
    public void setSampleNamesOpts(boolean sampleNamesOpts) {
        this.sampleNamesOpts = sampleNamesOpts;
    }

    
    
    
    
    //STATIC DROPDOWN
    private String vegdistOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.vegdistMeasureOpts
    
    public String getVegdistOpts() {
        return vegdistOpts;
    }

    public void setVegdistOpts(String vegdistOpts) {
        this.vegdistOpts = vegdistOpts;
    }

    
    private String ordStressDimOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.ordStressDimensionOpts
    
    public String getOrdStressDimOpts() {
        return ordStressDimOpts;
    }

    public void setOrdStressDimOpts(String ordStressDimOpts) {
        this.ordStressDimOpts = ordStressDimOpts;
    }


    private String ordColorOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.ordColorPaletteOpts
    
    public String getOrdColorOpts() {
        return ordColorOpts;
    }

    public void setOrdColorOpts(String ordColorOpts) {
        this.ordColorOpts = ordColorOpts;
    }
    

    
    //DYNAMIC DROPDOWN 
    private SelectItem[] nmdsMetaColumnOpts = null;
    
    public SelectItem[] getNmdsMetaColumnOpts(){
        String[] columns = OAUtils.GetNMDSMetaColumns(sb);
        int columnsLen = columns.length;
        nmdsMetaColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            nmdsMetaColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return nmdsMetaColumnOpts;
    }
    
    private String nmdsMetaColumnName = getNmdsMetaColumnOpts()[0].getLabel();
    
    public String getNmdsMetaColumnName() {
        return nmdsMetaColumnName;
    }

    public void setNmdsMetaColumnName(String nmdsMetaColumnName) {
        this.nmdsMetaColumnName = nmdsMetaColumnName;
    }

    

//// ACTION BUTTONS //
    public void nmdsUpdate_action2D() {
        OAUtils.CreateNMDSOrdination(sb, doOriginal, vegdistOpts, doAbundance);
        OAUtils.PlotNMDS2DOrdination(sb, ellipseOpts, var_arrowsOpts, env_arrowsOpts, env_centOpts, sampleNamesOpts, ordColorOpts, nmdsMetaColumnName, sb.getNewImage("ord_nmds_2D"), "png", 72);
    }
    public void nmdsUpdate_action3D() {
        OAUtils.CreateNMDSOrdination(sb, doOriginal, vegdistOpts, doAbundance);
        OAUtils.PlotNMDS3DOrdination(sb, ordColorOpts, var_arrowsOpts, nmdsMetaColumnName, sb.getNewImage("ord_nmds_3D"));
    }
    public void nmdsUpdate_actionStress() {
        OAUtils.CreateNMDSOrdination(sb, doOriginal, vegdistOpts, doAbundance);
        OAUtils.PlotNMDSstressOrdination(sb, ordStressDimOpts, sb.getNewImage("ord_nmds_stress"), "png", 72);
    }
    public void nmdsUpdate_actionScree() {
        OAUtils.CreateNMDSOrdination(sb, doOriginal, vegdistOpts, doAbundance);
        OAUtils.PlotNMDSscreeOrdination(sb, sb.getNewImage("ord_nmds_scree"), "png", 72);    
    }
    public void nmdsUpdate_action2Dgraph() {
        OAUtils.PlotNMDS2DOrdination(sb, ellipseOpts, var_arrowsOpts, env_arrowsOpts, env_centOpts, sampleNamesOpts, ordColorOpts, nmdsMetaColumnName, sb.getNewImage("ord_nmds_2D"), "png", 72);
    }
}



