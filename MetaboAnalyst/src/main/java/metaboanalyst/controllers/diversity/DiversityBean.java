/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.diversity;

import metaboanalyst.controllers.ordination.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.DiversityUtils;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.RDataUtils;

import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

@ManagedBean(name = "diversityBean")
public class DiversityBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        if (!sb.isAnalInit(pageID)) {
            if (!FacesContext.getCurrentInstance().isPostback()) {
                switch (pageID) {
                    case "Alpha":
                        doDefaultAlpha();
                        break;
                    case "Beta":
                        doDefaultBeta();
                        break;
                    case "Gamma":
                        doDefaultGamma();
                        break;
                    case "TestTutorial":
                        doDefaultTestTutorial();
                        break;
                    case "Rarefaction":
                         doDefaultRarefaction();
                        break;
                    case "Abunddist":
                         doDefaultAbunddist();
                        break;
                    case "Accummodel":
                         doDefaultAccummodel();
                        break;
                    case "Taxodiv":
                         doDefaultTaxodiv();
                        break;
                    case "Fddiv":
                         doDefaultFddiv();
                        break;
                    case "Unseendiv":
                         doDefaultUnseendiv();
                        break;
                }
            }

        }
    }

    
    private void doDefaultAlpha(){
        if (!DiversityUtils.CreateAlpha(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }
        DiversityUtils.PlotAlpha(sb);
    }
    
    private void doDefaultBeta(){
        if (!DiversityUtils.CreateBeta(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }
        DiversityUtils.PlotBeta(sb);
    }
    
    private void doDefaultGamma(){
        if (!DiversityUtils.CreateGamma(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }
        DiversityUtils.PlotGamma(sb);
    }
    private void doDefaultSpecies(){
        if (!DiversityUtils.CreateSpecies(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }
        DiversityUtils.PlotSpecies(sb);
    }
    
    private void doDefaultTestTutorial(){
        if (!DiversityUtils.CreateTestTutorial(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }
        DiversityUtils.CreateTestTutorial(sb);
        DiversityUtils.PlotTestTutorial(sb, sb.getCurrentImage("gamma_plot"), "png", 72);
    }
    
    private void doDefaultRarefaction(){
//        if (!DiversityUtils.CreateRarefactionDiv(sb, false, "test", "test", false, "test")){
//            RConnection RC = sb.getRConnection();
//            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//        }
        System.out.print("INSIDE RAREFACTION");
        DiversityUtils.CreateRarefactionDiv(sb, false, "NULL", "", false, "NULL");
        DiversityUtils.PlotRarefactionCurveDiversity(sb, "", "NULL", sb.getCurrentImage("Rarefaction_Curve_Plot"), "png", 72, "false");
        DiversityUtils.PlotRarefactionPlotDiversity(sb, "NULL", sb.getCurrentImage("Rarefaction_Linear_Plot"), "png", 72, "false");
        System.out.print("AFTER RAREFACTION");
    }
    
    private void doDefaultAbunddist(){
//        if (!DiversityUtils.CreateRarefactionDiv(sb, false, "test", "test", false, "test")){
//            RConnection RC = sb.getRConnection();
//            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//        }
        System.out.print("INSIDE ABUNDDIST"); 
        DiversityUtils.CreateAbundDistDiv(sb, false, "", false, "");
        DiversityUtils.PlotAbundFisherPlotDiv(sb, "NULL", "NULL", sb.getCurrentImage("Abundance_Fisher_Dist_Plot"), "png", 72, "false");
        DiversityUtils.PlotAbundPrestPlotDiv(sb, "NULL", "NULL", "NULL", sb.getCurrentImage("Abundance_Prest_Dist_Plot"), "png", 72, "false");
        DiversityUtils.PlotAbundRankPlotDiv(sb, sb.getCurrentImage("Abundance_Rank_Dist_Plot"), "png", 72, "false");
        System.out.print("AFTER ABUNDDIST");
    }
    
    private void doDefaultAccummodel(){
//        if (!DiversityUtils.CreateRarefactionDiv(sb, false, "test", "test", false, "test")){
//            RConnection RC = sb.getRConnection();
//            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//        }
        System.out.print("INSIDE ACCUMMODEL"); 
        DiversityUtils.CreateAccumModelDiv(sb, false, "", false, "NULL", "NULL", "NULL", "NULL");
        DiversityUtils.PlotAccumCurveDiv(sb, "NULL", "NULL", "NULL", "NULL", "NULL","NULL", "NULL", "", sb.getCurrentImage("Species_Accumulation_Model"), "png", 72, "false");
        System.out.print("AFTER ACCUMMODEL");
    }
    
    private void doDefaultTaxodiv(){
//        if (!DiversityUtils.CreateRarefactionDiv(sb, false, "test", "test", false, "test")){
//            RConnection RC = sb.getRConnection();
//            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//        }
        System.out.print("INSIDE TAXO"); 
        DiversityUtils.CreateTaxoDiv(sb, false, "NULL", false, false, "NULL", false);
        DiversityUtils.PlotTaxaTree(sb, "NULL",sb.getCurrentImage("Taxa_Tree_Plot"), "png", 72, "false");
        DiversityUtils.PlotTaxonScatter(sb, "NULL",sb.getCurrentImage("Taxa_Scatter_Plot"), "png", 72, "false");
        DiversityUtils.PlotTaxonHeatmap(sb, "NULL",sb.getCurrentImage("Taxa_Heatmap_Plot"), "png", 72, "false");
        System.out.print("AFTER TAXO");
    }
    

    private void doDefaultFddiv(){
//        if (!DiversityUtils.CreateRarefactionDiv(sb, false, "test", "test", false, "test")){
//            RConnection RC = sb.getRConnection();
//            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//        }
        System.out.print("INSIDE FD"); 
        DiversityUtils.CreateFdDiv(sb, false, "", "NULL", false, false, "", false, false, false, "", "NULL");
        DiversityUtils.PlotFdTree(sb, "NULL",sb.getCurrentImage("Cluster_Plot"), "png", 72, "false");
        System.out.print("AFTER FD");
    }
    
 
    private void doDefaultUnseendiv(){
//        if (!DiversityUtils.CreateRarefactionDiv(sb, false, "test", "test", false, "test")){
//            RConnection RC = sb.getRConnection();
//            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
//        }
        System.out.print("INSIDE UD"); 
        DiversityUtils.CreateUnseenDiv(sb, false, "", false, "NULL", "", "", "");
        DiversityUtils.PlotPoolBoxplot(sb, "NULL", "NULL", "", "", "NULL", sb.getCurrentImage("boxplot_richness"), "png", 72, "false");
        DiversityUtils.PlotUnseenCurve(sb, "NULL",sb.getCurrentImage("plot_matrices"), "png", 72, "false");
        System.out.print("AFTER UD");
    }
}
