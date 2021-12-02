/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.plotting;

import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.ChemoMetrics;
import metaboanalyst.rwrappers.Classifying;
import metaboanalyst.rwrappers.Clustering;
import metaboanalyst.rwrappers.DispersalUtils;
import metaboanalyst.rwrappers.Ordiantion;
import metaboanalyst.rwrappers.RCenter;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.SigVarSelect;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.PlottingUtils;
import metaboanalyst.utils.DataUtils;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import org.primefaces.context.RequestContext;


// All functions being called from plotting.r via PlottingUtils.java
@ManagedBean(name = "plottingMainBean")
public class PlottingMainBean implements Serializable {
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    
    public void performDefaultAnalysis(String pageID) {
        System.out.print(" Currently printing at performDefaultAnalysis");
        System.out.print(sb);
        if (!sb.isAnalInit(pageID)) {
            if (!FacesContext.getCurrentInstance().isPostback()) {
                System.out.print(" pageID");

                //sb.registerPage(pageID);
                switch (pageID) {
                    case "plotting":
                        doDefaultLinear();
                        doDefaultPieChart();
                        doDefaultBarChart();
                        doDefaultBoxChart();
                        doDefaultScatterChart();
                        break;  
                    case "linear":
                        doDefaultLinear();
                        break;
                    case "boxplot":
                        
                        break;
                    case "bargraph":
//                        doDefaultBetaDisper();
                        break;
                }
            }
        }
    }

    private void doDefaultLinear(){       
        PlottingUtils.PlotlinearGraph(sb, sb.getCurrentImage("lin"), "png", 72, 
                "p", 1, "black", 1, 19, "x axis", "y axis", "Linear Plot Title", "NULL", "NULL");     
    }

    private void doDefaultPieChart(){  
        PlottingUtils.CreatePieChart(sb, false, false, 1, "NULL", "NULL", "NULL", "Main Title", false);
        PlottingUtils.PlotPieChart(sb, sb.getCurrentImage("plot_pie_chart"), "png", 72);     
    }
    
    private void doDefaultBarChart(){  
        PlottingUtils.CreateBarChart(sb, "FALSE", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL");
        PlottingUtils.PlotBarChart(sb, sb.getCurrentImage("plot_bar_chart"), "png", 72);     
    }
    
    private void doDefaultBoxChart(){  
        PlottingUtils.CreateBoxPlot(sb, "NULL", "NULL", "NULL","NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "Box Plot Title");
        PlottingUtils.PlotBoxPlot(sb, sb.getCurrentImage("plot_box_chart"), "png", 72);     
    } 
    
    private void doDefaultScatterChart(){  
        PlottingUtils.CreateScatterChart(sb);
        PlottingUtils.PlotScatterChart(sb, sb.getCurrentImage("plot_scatter_chart"), "png", 72);     
    }  
    
    
}

