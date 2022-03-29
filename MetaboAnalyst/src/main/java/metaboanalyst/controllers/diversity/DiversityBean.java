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
import metaboanalyst.rwrappers.Clustering;
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
//                    case "Alpha":
//                        doDefaultAlpha();
//                        break;
//                    case "Beta":
//                        doDefaultBeta();
//                        break;
//                    case "Gamma":
//                        doDefaultGamma();
//                        break;
//                    case "TestTutorial":
//                        doDefaultTestTutorial();
//                        break;
                    case "Indices":
                        doDefaultIndices();
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
                    case "Spatialvis":
                        doDefaultSpatialvis();
                    break;
                }
            }

        }
    }
  
    
    private void doDefaultIndices(){
        System.out.print("INSIDE INDICES");
        DiversityUtils.CreateIndicesDiv(sb, false, "NULL", "NULL");
        DiversityUtils.PlotAlphaDiversity(sb, "NULL", sb.getCurrentImage("Alpha_Plot"), "png", 72, "false");
        DiversityUtils.PlotBetaDiversity(sb, "NULL", sb.getCurrentImage("Beta_Plot"), "png", 72, "false");
        System.out.print("AFTER INDICES");
    }
    
    
    private void doDefaultRarefaction(){
        System.out.print("INSIDE RAREFACTION");
        DiversityUtils.CreateRarefactionDiv(sb, false, "NULL", "", false);
        DiversityUtils.PlotRarefactionCurveDiversity(sb, "", "NULL", sb.getCurrentImage("Rarefaction_Curve_Plot"), "png", 72, "false");
        DiversityUtils.PlotRarefactionPlotDiversity(sb, "NULL", sb.getCurrentImage("Rarefaction_Linear_Plot"), "png", 72, "false");
        System.out.print("AFTER RAREFACTION");
    }
    
    private void doDefaultAbunddist(){
        System.out.print("INSIDE ABUNDDIST"); 
        DiversityUtils.CreateAbundFisherDistDiv(sb, false, "");
        DiversityUtils.CreateAbundPresDistDiv(sb, false, "", false, "-1");
        DiversityUtils.CreateAbundRankDistDiv(sb, false, "");
        DiversityUtils.PlotAbundFisherPlotDiv(sb, "NULL", "NULL", sb.getCurrentImage("Abundance_Fisher_Dist_Plot"), "png", 72, "false");
        DiversityUtils.PlotAbundPrestPlotDiv(sb, "NULL", "NULL", "NULL", sb.getCurrentImage("Abundance_Prest_Dist_Plot"), "png", 72, "false");
        DiversityUtils.PlotAbundRankPlotDiv(sb, sb.getCurrentImage("Abundance_Rank_Dist_Plot"), "png", 72, "false");
        System.out.print("AFTER ABUNDDIST");
    }
    
    private void doDefaultAccummodel(){
        System.out.print("INSIDE ACCUMMODEL"); 
        DiversityUtils.CreateAccumModelDiv(sb, false, "100", false, "NULL", "NULL", "NULL", "NULL");
        DiversityUtils.PlotAccumCurveDiv(sb, "NULL", "NULL", "NULL", "NULL", "NULL","NULL", "NULL", "", sb.getCurrentImage("Species_Accumulation_Model"), "png", 72, "false");
        System.out.print("AFTER ACCUMMODEL");
    }
    
    private void doDefaultTaxodiv(){
        System.out.print("INSIDE TAXO"); 
        DiversityUtils.CreateTaxoDiv(sb, false, "NULL", false, false, "NULL", false);
        DiversityUtils.PlotTaxaTree(sb, "NULL",sb.getCurrentImage("Taxa_Tree_Plot"), "png", 72, "false");
        DiversityUtils.PlotTaxonScatter(sb, "NULL", "NULL",sb.getCurrentImage("Taxa_Scatter_Plot"), "png", 72, "false");
        DiversityUtils.PlotTaxonHeatmap(sb, "NULL",sb.getCurrentImage("Taxa_Heatmap_Plot"), "png", 72, "false");
        System.out.print("AFTER TAXO");
    }
    

    private void doDefaultFddiv(){
        System.out.print("INSIDE FD"); 
        DiversityUtils.CreateFdDiv(sb, false, "", "NULL", false, false, "", false, false, false, "", "NULL");
        DiversityUtils.PlotFdTree(sb, "NULL",sb.getCurrentImage("Cluster_Plot"), "png", 72, "false");
        System.out.print("AFTER FD");
    }
    
 
    private void doDefaultUnseendiv(){
        System.out.print("INSIDE UD"); 
        DiversityUtils.CreateUnseenDiv(sb, false, "NULL", false, "NULL", "100", "3", "1");
        DiversityUtils.PlotPoolBoxplot(sb, "NULL", "NULL", "NULL", sb.getCurrentImage("boxplot_richness"), "png", 72, "false");
        DiversityUtils.PlotUnseenCurve(sb, sb.getCurrentImage("plot_matrices"), "png", 72, "false");
        System.out.print("AFTER UD");
    }
    
    
    private void doDefaultSpatialvis(){
        System.out.print("INSIDE Spatialvis"); 
        DiversityUtils.CreateSpatialvis(sb, false, false, false, "", "NULL", "NULL", "NULL", "", "NULL", "", "NULL", false, "NULL", false, false, sb.getCurrentImage("ggmap"), "png", 72, "false");
        System.out.print("AFTER FD");
    }
    
}