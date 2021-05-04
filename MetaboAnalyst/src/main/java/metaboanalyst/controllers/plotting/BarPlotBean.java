/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.plotting;

import java.io.File;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.CAUtils;
import metaboanalyst.rwrappers.PlottingUtils;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.chart.Axis;
import org.primefaces.model.chart.AxisType;
import org.primefaces.model.chart.LineChartModel;
import org.primefaces.model.chart.LineChartSeries;
import org.rosuda.REngine.Rserve.RConnection;



@ManagedBean(name = "barPlotBean")
@ViewScoped
public class BarPlotBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    private final SelectItem[] trendlineOpts;
    private final SelectItem[] colorOpts;
    
    private String colorChosen;
    private String lineBF;
   

    public String getColorChosen() {
        return colorChosen;
    }

    public void setColorChosen(String colorChosen) {
        this.colorChosen = colorChosen;
    }
    
    public String getLineBF() {
        return lineBF;
    }

    public void setLineBF(String linBF) {
        this.lineBF = lineBF;
    }
        
    public SelectItem[] getTrendlineOpts() {
        return trendlineOpts;
    }
    
    public SelectItem[] getColorOpts() {
        return colorOpts;
    }
    
    
    public void barBtn_action() {
        PlottingUtils.CreateBarChart(sb, "FALSE", "NULL", lineBF, colorChosen, "NULL", "NULL", "NULL", "NULL");
        PlottingUtils.PlotBarChart(sb, sb.getCurrentImage("plot_bar_chart"), "png", 72); 
    }
    
    public BarPlotBean() {
        
        trendlineOpts = new SelectItem[3];
        trendlineOpts[0] = new SelectItem("lbf", "Line of Best Fit");
        trendlineOpts[1] = new SelectItem("lowess", "Locally Weighted Trend Line");
        trendlineOpts[2] = new SelectItem("NULL", "None");

        colorOpts = new SelectItem[5];
        colorOpts[0] = new SelectItem("r", "Rainbow");
        colorOpts[1] = new SelectItem("v", "Viridis");
        colorOpts[2] = new SelectItem("g", "Grey");
        colorOpts[3] = new SelectItem("p", "Plasma");
        colorOpts[4] = new SelectItem("NULL", "Light blue");
        
    }
   
}
