/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.plotting;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Locale.Category;
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
    private final SelectItem[] colorOpts;
    
    private String colorChosen;
    private String labx;
    private String laby;
    private String title;
    private String[] columnNames = {"Volvo", "BMW", "Ford", "Mazda"};
    
    public void setColumnNames(String[] columnNames) {
        this.columnNames = columnNames;
    } 
    
    public String[] getColumnNames() {
        return columnNames;
    }
    
    public void setTitle(String title) {
        this.title = title;
    } 
    
    public String getTitle() {
        return title;
    }
    public void setLaby(String laby) {
        this.laby = laby;
    } 
    
    public String getLaby() {
        return laby;
    }
    
    public void setLabx(String labx) {
        this.labx = labx;
    } 
    
    public String getlabx() {
        return labx;
    }
    
    public String getColorChosen() {
        return colorChosen;
    }

    public void setColorChosen(String colorChosen) {
        this.colorChosen = colorChosen;
    }
    
    
    public SelectItem[] getColorOpts() {
        return colorOpts;
    }
    

    public void barBtn_action() {
        PlottingUtils.CreateBarChart(sb, "FALSE", "NULL", "NULL", colorChosen, labx, laby, "NULL", title);
        PlottingUtils.PlotBarChart(sb,  sb.getNewImage("plot_bar_chart"), "png", 72); 
    }
    
    public BarPlotBean() {

        colorOpts = new SelectItem[5];
        colorOpts[0] = new SelectItem("r", "Rainbow");
        colorOpts[1] = new SelectItem("v", "Viridis");
        colorOpts[2] = new SelectItem("g", "Grey");
        colorOpts[3] = new SelectItem("p", "Plasma");
        colorOpts[4] = new SelectItem("NULL", "Light blue");
        
  
    }
   
}
