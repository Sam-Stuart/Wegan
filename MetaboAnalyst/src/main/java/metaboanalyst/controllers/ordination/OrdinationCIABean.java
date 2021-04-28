/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.ordination;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.CAUtils;
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
@ManagedBean(name = "ordCIABean")
public class OrdinationCIABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
        
    
    //CHECKBOX
    private boolean doOriginal = false; 
    
    public boolean isdoOriginal() {
        return doOriginal;
    }
    
    public void setDoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
    
    
    private boolean doMetaGroup = false; 
    
    public boolean isdoMetaGroup() {
        return doMetaGroup;
    }
    
    public void setDoMetaGroup(boolean doMetaGroup) {
        this.doMetaGroup = doMetaGroup;
    }
    
    
    //TEXT BOX
    private String envInput = "";

    public String getEnvInput() {
        return envInput;
    }

    public void setEnvInput(String envInput) {
        this.envInput = envInput;
    }
    
    
    //STATIC DROPDOWN
    private String coiaDataSetOpts = "main";
    
    public String getCoiaDataSetOpts() {
        return coiaDataSetOpts;
    }

    public void setCoiaDataSetOpts(String coiaDataSetOpts) {
        this.coiaDataSetOpts = coiaDataSetOpts;
    }

    

    private String ordColorOpts = "viridis";
    
    public String getOrdColorOpts() {
        return ordColorOpts;
    }

    public void setOrdColorOpts(String ordColorOpts) {
        this.ordColorOpts = ordColorOpts;
    }
    
    

    private String coiaTypeOpts = "numeric";
    
    public String getCoiaTypeOpts() {
        return coiaTypeOpts;
    }

    public void setCoiaTypeOpts(String coiaTypeOpts) {
        this.coiaTypeOpts = coiaTypeOpts;
    }
    


    //DYNAMIC DROPDOWN 
    private SelectItem[] ciaMetaColumnOpts = null;
    
    public SelectItem[] getCiaMetaColumnOpts(){
        String[] columns = OAUtils.ciaGetMetaColumns(sb);
        System.out.print(columns);
        int columnsLen = columns.length; 
        ciaMetaColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            ciaMetaColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return ciaMetaColumnOpts;
    }
    
    private String ciaMetaColumnName = getCiaMetaColumnOpts()[0].getLabel();
        
    public String getCiaMetaColumnName() {
        return ciaMetaColumnName;
    }

    public void setCiaMetaColumnName(String ciaMetaColumnName) {
        this.ciaMetaColumnName = ciaMetaColumnName;
    }
    
    
// ACTION BUTTON //
    public void ciaUpdate_action() {
        OAUtils.CreateCIAOrdination(sb, coiaTypeOpts, envInput, doOriginal); 
        OAUtils.PlotCIAscatterOrdination(sb, doMetaGroup, ciaMetaColumnName, ordColorOpts, sb.getCurrentImage("ord_cia_scatter"), "png", 72); //ordMetaColnameOpts is a dynamic dropdown
        OAUtils.PlotCIAloadingOrdination(sb, coiaDataSetOpts, sb.getCurrentImage("ord_cia_loading"), "png", 72);
        OAUtils.PlotCIAscreeOrdination(sb, sb.getCurrentImage("ord_cia_scree"), "png", 72);    
    }
    
}



