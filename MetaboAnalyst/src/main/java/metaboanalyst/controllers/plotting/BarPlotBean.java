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

    private final SessionBean1 sb = DataUtils.findBean("sessionBean1");
    private final SelectItem[] aggregateOpts;
    
    private String aggregateFunChosen;
    private String labx;
    private String laby;
    private String title;

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
    
    public String getLabx() {
        return labx;
    }
    
    public String getAggregateFunChosen() {
        return aggregateFunChosen;
    }

    public void setAggregateFunChosen(String aggregateFunChosen) {
        this.aggregateFunChosen = aggregateFunChosen;
    }
    
    
    public SelectItem[] getAggregateOpts() {
        return aggregateOpts;
    }
    

    public void barBtn_action() {
        System.out.println("I was called");
        PlottingUtils.CreateBarChart(sb, "NULL", "NULL", this.labx, this.laby, "NULL", "NULL", this.title, this.aggregateFunChosen, false);
        PlottingUtils.PlotBarChart(sb,  sb.getNewImage("plot_bar_chart"), "png", 72); 
    }
    
    public BarPlotBean() {
        aggregateOpts = new SelectItem[5];
        aggregateOpts[0] = new SelectItem("mean", "Mean");
        aggregateOpts[1] = new SelectItem("sum", "Sum");
        aggregateOpts[2] = new SelectItem("length", "Count");
        aggregateOpts[3] = new SelectItem("min", "Min");
        aggregateOpts[4] = new SelectItem("max", "Max");  
    }
   
}
