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
@ManagedBean(name = "ordANOSIMBean")
public class OrdinationANOSIMBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    
    
    //TABLES FOR DOWNLOAD
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
    
    private String fileDissMatrix = "anosim_dissimilarity_matrix.csv";
    private String fileDissMatrixPath = "<a target='_blank' href = \"/EnvboAnalyst/resources/users/" + usrName + File.separator + fileDissMatrix + "\">" + fileDissMatrix + "</a>";
 
    public String getFileDissMatrixPath() {
        return fileDissMatrixPath;
    }
        
    public void setFileDissMatrixPath(String fileDissMatrixPath) {
        this.fileDissMatrixPath = fileDissMatrixPath;
    } 
    
   
    
    
    //CHECKBOX
    private boolean doBinary= false; 
    
    public boolean isDoBinary() {
        return doBinary;
    }
    
    public void setDoBinary(boolean doBinary) {
        this.doBinary = doBinary;
    }
    
    
    private boolean doOriginal = false; 
    
    public boolean isDoOriginal() {
        return doOriginal;
    }
    
    public void setDoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }

    
    
    
    //STATIC DROPDOWN
    private String vegdistOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.vegdistMeasureOpts
    
    public String getVegdistOpts() {
        return vegdistOpts;
    }

    public void setVegdistOpts(String vegdistOpts) {
        this.vegdistOpts = vegdistOpts;
    }


    private String ordColorOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.ordColorPaletteOpts
    
    public String getOrdColorOpts() {
        return ordColorOpts;
    }

    public void setOrdColorOpts(String ordColorOpts) {
        this.ordColorOpts = ordColorOpts;
    }
    

    
    //DYNAMIC DROPDOWN 
    private SelectItem[] anosimEnvColumnOpts = null;
    
    public SelectItem[] getAnosimEnvColumnOpts(){
        String[] columns = OAUtils.GetANOSIMEnvColumns(sb);
        int columnsLen = columns.length;
        anosimEnvColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            anosimEnvColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return anosimEnvColumnOpts;
    }
    
    private String anosimEnvColumnName = getAnosimEnvColumnOpts()[0].getLabel();
    
    public String getAnosimEnvColumnName() {
        return anosimEnvColumnName;
    }

    public void setAnosimEnvColumnName(String anosimEnvColumnName) {
        this.anosimEnvColumnName = anosimEnvColumnName;
    }

    

//// ACTION BUTTONS //
    public void anosimUpdate_action() {
        OAUtils.CreateANOSIM(sb, doOriginal, vegdistOpts, doBinary, anosimEnvColumnName);
        OAUtils.PlotANOSIM(sb, ordColorOpts, sb.getNewImage("ord_anosim_plot"), "png", 72);  
    }

}



