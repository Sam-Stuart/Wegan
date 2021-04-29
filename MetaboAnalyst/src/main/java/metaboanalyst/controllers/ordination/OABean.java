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
@ManagedBean(name = "oaBean")
public class OABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        if (!sb.isAnalInit(pageID)) {
            if (!FacesContext.getCurrentInstance().isPostback()) {
                //sb.registerPage(pageID);
                switch (pageID) {
                    case "NMDS":
                        doDefaultNMDS();
                        break;
//                    case "PCOA":
//                        doDefaultPCOA();
//                        break;
                    case "CIA":
                        doDefaultCIA();
                        break;
//                    case "ANOSIM":
//                        doDefaultANOSIM();
//                        break;
                    case "RDA":
                        doDefaultRDA();
                        break;                
                }
            }

        }
    }

    private void doDefaultNMDS() {
////        OAUtils.CreateNMDSOrdination(sb, "NULL", false, false, "NULL", "NULL", "NULL");
//        OAUtils.PlotNMDS2DOrdination(sb, false, false, false, false, false, false, "NULL", "NULL", "NULL", sb.getCurrentImage("ord_nmds_2D"), "png", 72, "NULL");
////        OAUtils.PlotNMDS3DOrdination(sb, "NULL", "NULL", "NULL", sb.getCurrentImage("ord_nmds_3D"), "png", 72, "NULL");
//        OAUtils.PlotNMDSstressOrdination(sb, "NULL", sb.getCurrentImage("ord_nmds_stress"), "png", 72, "NULL");
//        OAUtils.PlotNMDSscreeOrdination(sb, sb.getCurrentImage("ord_nmds_scree"), "png", 72, "NULL");
    }
//    
//
//    private void doDefaultPCOA() {
//        OAUtils.CreatePCOAOrdination(sb, "NULL", false, false, false, "NULL", "NULL", "NULL");
//        OAUtils.PlotPCOA2DOrdination(sb, false, false, false, false, false, false, "NULL", "NULL", "NULL", sb.getCurrentImage("ord_pcoa_2D"), "png", 72, "NULL");
////        OAUtils.PlotPCOA3DOrdination(sb, "NULL", "NULL", "NULL", sb.getCurrentImage("ord_pcoa_3D"), "png", 72, "NULL");
//        OAUtils.PlotPCOAstressOrdination(sb, sb.getCurrentImage("ord_pcoa_stress"), "png", 72, "NULL");
//        OAUtils.PlotPCOAscreeOrdination(sb, sb.getCurrentImage("ord_pcoa_scree"), "png", 72, "NULL");
//    }
    
    
    private void doDefaultCIA() {
        OAUtils.CreateCIAOrdination(sb, "NULL", "NULL", false);
        OAUtils.PlotCIAscatterOrdination(sb, false, "NULL", "NULL", sb.getCurrentImage("ord_cia_scatter"), "png", 72);
        OAUtils.PlotCIAloadingOrdination(sb, "NULL", sb.getCurrentImage("ord_cia_loading"), "png", 72);
        OAUtils.PlotCIAscreeOrdination(sb, sb.getCurrentImage("ord_cia_scree"), "png", 72);  
    }
//    
//    private void doDefaultANOSIM() {
//        OAUtils.CreateANOSIMOrdination(sb);
//        OAUtils.PlotANOSIMOrdination(sb, "NULL", sb.getCurrentImage("ord_cia_scatter"), "png", 72);
//    }
    
    private void doDefaultRDA(){
        OAUtils.CreateRDA(sb, "NULL", "NULL", "NULL", "NULL");
        OAUtils.PlotRDA2D(sb, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", sb.getCurrentImage("ord_rda_2D"), "png", 72, "NULL");
        OAUtils.PlotRDAScree(sb, sb.getCurrentImage("ord_rda_scree"), "png", 72, "NULL");
    }
}
