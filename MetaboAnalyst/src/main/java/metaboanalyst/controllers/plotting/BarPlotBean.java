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
    private final SelectItem[] textFontSizeOpts;

    private final SelectItem[] axisFontSizeOpts;

    private String aggregateFunChosen;
    private String labx;
    private String laby;
    private String title;
    private Integer chosenAxisTextSize;
    private Integer chosenTitleTextSize;
    
    public BarPlotBean() {
        aggregateOpts = new SelectItem[5];
        aggregateOpts[0] = new SelectItem("mean", "Mean");
        aggregateOpts[1] = new SelectItem("sum", "Sum");
        aggregateOpts[2] = new SelectItem("length", "Count");
        aggregateOpts[3] = new SelectItem("min", "Min");
        aggregateOpts[4] = new SelectItem("max", "Max");
        
        SelectItem[] fontSizeOpts = new SelectItem[5];
        fontSizeOpts[0] = new SelectItem(12, "Extra Small");
        fontSizeOpts[1] = new SelectItem(16, "Small");
        fontSizeOpts[2] = new SelectItem(20, "Medium");
        fontSizeOpts[3] = new SelectItem(24, "Large");
        fontSizeOpts[4] = new SelectItem(32, "Extra Large");
        
        this.textFontSizeOpts = fontSizeOpts.clone();
        this.axisFontSizeOpts = fontSizeOpts.clone();
    }

    public SelectItem[] getTextFontSizeOpts() {
        return textFontSizeOpts;
    }

    public SelectItem[] getAxisFontSizeOpts() {
        return axisFontSizeOpts;
    }

    public Integer getChosenAxisTextSize() {
        return chosenAxisTextSize;
    }

    public void setChosenAxisTextSize(Integer chosenAxisTextSize) {
        this.chosenAxisTextSize = chosenAxisTextSize;
    }

    public Integer getChosenTitleTextSize() {
        return chosenTitleTextSize;
    }

    public void setChosenTitleTextSize(Integer chosenTitleTextSize) {
        this.chosenTitleTextSize = chosenTitleTextSize;
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
        PlottingUtils.CreateBarChart(sb, "NULL", "NULL", this.labx, this.laby, "NULL", "NULL", this.title, this.aggregateFunChosen, this.chosenTitleTextSize, this.chosenAxisTextSize, false);
        PlottingUtils.PlotBarChart(sb, sb.getNewImage("plot_bar_chart"), "png", 72);
    }

    

}
