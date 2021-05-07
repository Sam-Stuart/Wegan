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
@ManagedBean(name = "ordPCOABean")
public class OrdinationPCOABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
        
    //Hard code insupplementary data for testing:
//    envData <- .readDataTable("/home/louisa/Wegan/MetaboAnalyst/src/main/webapp/resources/data/dune_env.csv")
    
    //CHECKBOX
    private boolean doAbundance = false; 
    
    public boolean isdoAbundance() {
        return doAbundance;
    }
    
    public void setdoAbundance(boolean doAbundance) {
        this.doAbundance = doAbundance;
    }

    
    private boolean doOriginal = false; 
    
    public boolean isdoOriginal() {
        return doOriginal;
    }
    
    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
    
    
    
    private boolean doBinary = false; 
    
    public boolean isdoBinary() {
        return doBinary;
    }
    
    public void setdoBinary(boolean doBinary) {
        this.doBinary = doBinary;
    }
    
    
    private boolean ellipseOpts = false; 
    
    public boolean isellipseOpts() {
        return ellipseOpts;
    }
    
    public void setellipseOpts(boolean ellipseOpts) {
        this.ellipseOpts = ellipseOpts;
    }
        
    
    private boolean var_arrowsOpts = false; 
    
    public boolean isvar_arrowsOpts() {
        return var_arrowsOpts;
    }
    
    public void setvar_arrowsOpts(boolean var_arrowsOpts) {
        this.var_arrowsOpts = var_arrowsOpts;
    }
    
    
        
    private boolean env_arrowsOpts = false; 
    
    public boolean isenv_arrowsOpts() {
        return env_arrowsOpts;
    }
    
    public void setenv_arrowsOpts(boolean env_arrowsOpts) {
        this.env_arrowsOpts = env_arrowsOpts;
    }
    
         
    private boolean env_centOpts = false; 
    
    public boolean isenv_centOpts() {
        return env_centOpts;
    }
    
    public void setenv_centOpts(boolean env_centOpts) {
        this.env_centOpts = env_centOpts;
    }
     
         
    private boolean sampleNamesOpts = false; 
    
    public boolean issampleNamesOpts() {
        return sampleNamesOpts;
    }
    
    public void setsampleNamesOpts(boolean sampleNamesOpts) {
        this.sampleNamesOpts = sampleNamesOpts;
    }
    
     
         
    private boolean point_optionsOpts = false; 
    
    public boolean ispoint_optionsOpts() {
        return point_optionsOpts;
    }
    
    public void setpoint_optionsOpts(boolean point_optionsOpts) {
        this.point_optionsOpts = point_optionsOpts;
    }
    
    
    
    
    //STATIC DROPDOWN
    private String vegdistOpts = "bray";
    
    public String getvegdistOpts() {
        return vegdistOpts;
    }

    public void setvegdistOpts(String vegdistOpts) {
        this.vegdistOpts = vegdistOpts;
    }

    
    private String ordStressDimOpts = "1";
    
    public String getordStressDimOpts() {
        return ordStressDimOpts;
    }

    public void setordStressDimOpts(String ordStressDimOpts) {
        this.ordStressDimOpts = ordStressDimOpts;
    }


    private String ordColorOpts = "viridis";
    
    public String getordColorOpts() {
        return ordColorOpts;
    }

    public void setordColorOpts(String ordColorOpts) {
        this.ordColorOpts = ordColorOpts;
    }
    

//// ACTION BUTTONS //
//    public void pcoaUpdate_action() {
//        OAUtils.CreatePCOAOrdination(sb, vegdistOpts, doAbundance, doOriginal, doBinary, "NULL", "NULL", "NULL");
//        OAUtils.PlotPCOA2DOrdination(sb, ellipseOpts, var_arrowsOpts, env_arrowsOpts, env_centOpts, sampleNamesOpts, point_optionsOpts, ordColorOpts, "NULL", "NULL", sb.getNewImage("ord_pcoa_2D"), "png", 72, "NULL");
////        OAUtils.PlotPCOA3DOrdination(sb, "NULL", "NULL", "NULL", sb.getNewImage("ord_pcoa_3D"), "png", 72, "NULL");
//       OAUtils.PlotPCOAstressOrdination(sb, sb.getNewImage("ord_pcoa_stress"), "png", 72, "NULL");
//       OAUtils.PlotPCOAscreeOrdination(sb, sb.getNewImage("ord_pcoa_scree"), "png", 72, "NULL");    
//    }
    
}



