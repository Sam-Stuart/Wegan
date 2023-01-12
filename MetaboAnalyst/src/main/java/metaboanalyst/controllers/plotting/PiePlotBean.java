
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.plotting;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.rwrappers.PlottingUtils;
import java.util.ArrayList;

/**
 *
 * @author Hieu
 */
@ManagedBean(name = "piePlotBean")
@ViewScoped
public class PiePlotBean extends PlotBean implements Serializable {

    private final SelectItem[] aggregateOpts;
    private String aggregateFunChosen;

    private String facA = "null";

    public PiePlotBean() {
        super();
        aggregateOpts = new SelectItem[5];
        aggregateOpts[0] = new SelectItem("mean", "Mean");
        aggregateOpts[1] = new SelectItem("sum", "Sum");
        aggregateOpts[2] = new SelectItem("length", "Count");
        aggregateOpts[3] = new SelectItem("min", "Min");
        aggregateOpts[4] = new SelectItem("max", "Max");

        numericBoxColumnOpts = this.getNumericBoxColumnOpts();
        if (numericBoxColumnOpts.length != 0) {
            this.facA = numericBoxColumnOpts[0].getLabel();
        }

    }

    public String getFacA() {
        return facA;
    }

    public void setFacA(String facA) {
        this.facA = facA;
    }

    public SelectItem[] getAggregateOpts() {
        return aggregateOpts;
    }

    public String getAggregateFunChosen() {
        return aggregateFunChosen;
    }

    public void setAggregateFunChosen(String aggregateFunChosen) {
        this.aggregateFunChosen = aggregateFunChosen;
    }

    @Override
    public void button_action() {
        PlottingUtils.CreatePieChart(sb, this.facA, "NULL", labx, laby, "NULL", title, "NULL", this.aggregateFunChosen, false);
        PlottingUtils.PlotPieChart(sb, sb.getNewImage("plot_pie_chart"), "png", 72);
    }

}
