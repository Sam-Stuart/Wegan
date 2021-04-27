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
@ManagedBean(name = "ordCIABean")
public class OrdinationCIABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
        
    //Hard code insupplementary data for testing:
//    envData <- .readDataTable("/home/louisa/Wegan/MetaboAnalyst/src/main/webapp/resources/data/dune_env.csv")
    
    //CHECKBOX
    private boolean doOriginal = false; 
    
    public boolean isdoOriginal() {
        return doOriginal;
    }
    
    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
    
    
    
    
    //STATIC DROPDOWN
    private String coiaDataSetOpts = "main";
    
    public String getcoiaDataSetOpts() {
        return coiaDataSetOpts;
    }

    public void setcoiaDataSetOpts(String coiaDataSetOpts) {
        this.coiaDataSetOpts = coiaDataSetOpts;
    }

    

    private String ordColorOpts = "viridis";
    
    public String getordColorOpts() {
        return ordColorOpts;
    }

    public void setordColorOpts(String ordColorOpts) {
        this.ordColorOpts = ordColorOpts;
    }
    
    

    private String coiaTypeOpts = "numeric";
    
    public String getcoiaTypeOpts() {
        return coiaTypeOpts;
    }

    public void setcoiaTypeOpts(String coiaTypeOpts) {
        this.coiaTypeOpts = coiaTypeOpts;
    }

// ACTION BUTTONS //
    public void ciaUpdate_action() {
        OAUtils.CreateCIAOrdination(sb, doOriginal, coiaTypeOpts, "NULL", "NULL", "NULL");
        OAUtils.PlotCIAscatterOrdination(sb, "NULL", "NULL", ordColorOpts, sb.getCurrentImage("ord_cia_scatter"), "png", 72, "NULL");
        OAUtils.PlotCIAloadingOrdination(sb, coiaDataSetOpts, sb.getCurrentImage("ord_cia_loading"), "png", 72, "NULL");
        OAUtils.PlotCIAscreeOrdination(sb, sb.getCurrentImage("ord_cia_scree"), "png", 72, "NULL");    
    }
    
}



