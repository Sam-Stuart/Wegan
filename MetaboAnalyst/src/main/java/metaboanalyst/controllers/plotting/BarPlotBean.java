/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.plotting;

import java.io.Serializable;
import javax.inject.Named;
import javax.faces.view.ViewScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.PlottingUtils;
import metaboanalyst.utils.DataUtils;




@ViewScoped
@Named("barPlotBean")
public class BarPlotBean implements Serializable {

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
        PlottingUtils.CreateBarChart(sb, "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", false);
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
