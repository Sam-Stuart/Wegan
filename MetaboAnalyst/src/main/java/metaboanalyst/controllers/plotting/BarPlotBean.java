/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.plotting;

import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.rwrappers.PlottingUtils;

@ManagedBean(name = "barPlotBean")
@ViewScoped
public class BarPlotBean extends PlotBean implements Serializable {
    private final SelectItem[] aggregateOpts;
    private String aggregateFunChosen;
    
    public BarPlotBean() {
        super();
        aggregateOpts = new SelectItem[5];
        aggregateOpts[0] = new SelectItem("mean", "Mean");
        aggregateOpts[1] = new SelectItem("sum", "Sum");
        aggregateOpts[2] = new SelectItem("length", "Count");
        aggregateOpts[3] = new SelectItem("min", "Min");
        aggregateOpts[4] = new SelectItem("max", "Max");
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

    @Override
    public void button_action() {
        PlottingUtils.CreateBarChart(sb, "NULL", "NULL", this.labx, this.laby, "NULL", "NULL", this.title, this.aggregateFunChosen, this.chosenTitleTextSize, this.chosenAxisTextSize, false);
        PlottingUtils.PlotBarChart(sb, sb.getNewImage("plot_bar_chart"), "png", 72);
    }
}
