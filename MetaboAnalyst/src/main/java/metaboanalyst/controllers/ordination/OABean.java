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
import metaboanalyst.rwrappers.ChemoMetrics;
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
                    case "PCOA":
                        doDefaultPCOA();
                        break;
                    case "CIA":
                        doDefaultCIA();
                        break;
                    case "ANOSIM":
                        doDefaultANOSIM();
                        break;
                    case "RDA":
                        doDefaultRDA();
                        break;                
                    case "Bray":
                        doDefaultBray();
                        break; 
                    case "CCA":
                        doDefaultCCA();
                        break; 
                    case "CA":
                        doDefaultCA();
                        break;
                    case "PCA":
                        doDefaultPCA();
                        break;
                    case "DCA":
                        doDefaultDCA();
                        break;
                }
            }

        }
    }

    private void doDefaultNMDS() {
        OAUtils.CreateNMDSOrdination(sb, false, "NULL", false);
        OAUtils.PlotNMDS2DOrdination(sb, false, false, false, false, false, "NULL", "NULL", sb.getCurrentImage("ord_nmds_2D"), "png", 72);
        OAUtils.PlotNMDS3DOrdination(sb, "NULL", false, "NULL", sb.getCurrentImage("ord_nmds_3D"));
        OAUtils.PlotNMDSstressOrdination(sb, "NULL", sb.getCurrentImage("ord_nmds_stress"), "png", 72);
        OAUtils.PlotNMDSscreeOrdination(sb, sb.getCurrentImage("ord_nmds_scree"), "png", 72);
    }
   

    private void doDefaultPCOA() {
        OAUtils.CreatePCOAOrdination(sb, false, "NULL", false, false, " ");
        OAUtils.PlotPCOA2DOrdination(sb, false, false, false, false, false, "NULL", "NULL", sb.getCurrentImage("ord_pcoa_2D"), "png", 72);
        OAUtils.PlotPCOA3DOrdination(sb, "NULL", false, "NULL", sb.getCurrentImage("ord_pcoa_3D"));
        OAUtils.PlotPCOAstressOrdination(sb, sb.getCurrentImage("ord_pcoa_stress"), "png", 72);
        OAUtils.PlotPCOAscreeOrdination(sb, sb.getCurrentImage("ord_pcoa_scree"), "png", 72);
    }
    
    
    private void doDefaultCIA() {
        OAUtils.CreateCIAOrdination(sb, false, "NULL", " ");
        OAUtils.PlotCIAscatterOrdination(sb, false, "NULL", "NULL", sb.getCurrentImage("ord_cia_scatter"), "png", 72);
        OAUtils.PlotCIAloadingOrdination(sb, "NULL", sb.getCurrentImage("ord_cia_loading"), "png", 72);
        OAUtils.PlotCIAscreeOrdination(sb, sb.getCurrentImage("ord_cia_scree"), "png", 72);  
    }
    
    private void doDefaultANOSIM() {
        OAUtils.CreateANOSIM(sb, false, "NULL", false, "NULL");
        OAUtils.PlotANOSIM(sb, "NULL", sb.getCurrentImage("ord_anosim_plot"), "png", 72);
    }
    
    private void doDefaultRDA(){
        OAUtils.CreateRDA(sb, false, " ", false);
        OAUtils.PlotRDA2D(sb, "NULL", false, false, false, false, "NULL", false, sb.getCurrentImage("ord_rda_2D"), "png", 72);
        OAUtils.PlotRDAScree(sb, sb.getCurrentImage("ord_rda_scree"), "png", 72);
    }
    
    private void doDefaultBray(){
        OAUtils.CreateBray(sb, false, "euclidean", false, false);
        OAUtils.PlotBray2D(sb, "NULL", false, false, false, "NULL", "NULL", "NULL", sb.getCurrentImage("ord_bray_2D"), "png", 72);
//        OAUtils.PlotBray3D(sb, sb.getCurrentImage("ord_rda_scree"), "png", 72, "NULL");
        OAUtils.PlotBray3D(sb, sb.getCurrentImage("bray_score3d"), "json");
        OAUtils.PlotBrayScree(sb, sb.getCurrentImage("ord_bray_scree"), "png", 72);
    }
    
    private void doDefaultCCA(){
//        if (!OAUtils.CreateCCA(sb, false, false, "NULL")){
//    }
        OAUtils.CreateCCA(sb, false, false, "NULL");
//            RConnection RC = sb.getRConnection();
//            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//        }
        OAUtils.PlotCCA(sb, "NULL", false, false, false, false, false, "NULL", "NULL", "NULL", sb.getCurrentImage("ord_cca_2D"), "png", 72);
        OAUtils.PlotCcaScree(sb, sb.getCurrentImage("ord_cca_scree"), "png", 72);
    }
    
    private void doDefaultCA(){
//        if (!OAUtils.CreateCA(sb)){
//            RConnection RC = sb.getRConnection();
//            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//        }
        OAUtils.CreateCA(sb);
//            RConnection RC = sb.getRConnection();
//            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        OAUtils.PlotCA2D(sb, "NULL", false, false, sb.getCurrentImage("ord_ca_2D"), "png", 72);
        OAUtils.PlotCAScree(sb, sb.getCurrentImage("ord_cca_scree"), "png", 72);
    }
    
    private void doDefaultPCA() {
        ChemoMetrics.InitPCA(sb, false);
        ChemoMetrics.PlotPCAPairSummary(sb, sb.getCurrentImage("pca_pair"), "png", 72, 5);
        ChemoMetrics.PlotPCAScree(sb, sb.getCurrentImage("pca_scree"), "png", 72, 5);
        ChemoMetrics.PlotPCA2DScore(sb, sb.getCurrentImage("pca_score2d"), "png", 72, 1, 2, 0.95, 1, 0);
        ChemoMetrics.PlotPCALoading(sb, sb.getCurrentImage("pca_loading"), "png", 72, 1, 2, "scatter", 1);  // setLoadingTable(pcImpInx);
        ChemoMetrics.PlotPCABiplot(sb, sb.getCurrentImage("pca_biplot"), "png", 72, 1, 2);
        // ChemoMetrics.PlotPCA3DScore(sb, sb.getCurrentImage("pca_score3d"), "png", 72, 1, 2, 3, 40);
        ChemoMetrics.PlotPCA3DScore(sb, sb.getCurrentImage("pca_score3d"), "json", 72, 1, 2, 3);
    }
    
    private void doDefaultDCA(){
//        if (!OAUtils.CreateDCA(sb)){
//            RConnection RC = sb.getRConnection();
//            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//        }
        OAUtils.CreateDCA(sb);
        OAUtils.PlotDCA2D(sb, "NULL", false, false, false, false, false, "NULL", "NULL", "NULL", sb.getCurrentImage("ord_dca_2D"), "png", 72);
        OAUtils.PlotDCAScree(sb, sb.getCurrentImage("ord_dca_scree"), "png", 72);
    }
    
}
