
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
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.PlottingUtils;
import metaboanalyst.utils.DataUtils;
import java.util.ArrayList; // import the ArrayList class

/**
 *
 * @author Hieu
 */
@ManagedBean(name = "piePlotBean")
@ViewScoped
public class PiePlotBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    private final SelectItem[] aggregateOpts;
    private final SelectItem[] textFontSizeOpts;

    private final SelectItem[] axisFontSizeOpts;
    private ArrayList<SelectItem> numericBoxColumnOpts;

    private String aggregateFunChosen;
    private String chosenTitleTextSize;

    private String chosenAxisTextSize;
    private String labx;
    private String laby;
    private String title;
    private String facA = getNumericBoxColumnOpts().get(0).getLabel();

    public PiePlotBean() {
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

    public String getFacA() {
        return facA;
    }

    public void setFacA(String facA) {
        this.facA = facA;
    }

    public void setNumericBoxColumnOpts() {
        List<String> columns = Arrays.asList(PlottingUtils.GetNumericDataColumnsBoxPlt(sb));
        this.numericBoxColumnOpts = new ArrayList<SelectItem>();
        columns.forEach((e) -> {
            this.numericBoxColumnOpts.add(new SelectItem(e, e));
        });
    }

    public ArrayList<SelectItem> getNumericBoxColumnOpts() {
        if (this.numericBoxColumnOpts == null) {
            this.setNumericBoxColumnOpts();
        }
        return this.numericBoxColumnOpts;
    }

    public SelectItem[] getAggregateOpts() {
        return aggregateOpts;
    }

    public SelectItem[] getTextFontSizeOpts() {
        return textFontSizeOpts;
    }

    public SelectItem[] getAxisFontSizeOpts() {
        return axisFontSizeOpts;
    }

    public String getAggregateFunChosen() {
        return aggregateFunChosen;
    }

    public void setAggregateFunChosen(String aggregateFunChosen) {
        this.aggregateFunChosen = aggregateFunChosen;
    }

    public String getChosenTitleTextSize() {
        return chosenTitleTextSize;
    }

    public void setChosenTitleTextSize(String chosenTitleTextSize) {
        this.chosenTitleTextSize = chosenTitleTextSize;
    }

    public String getChosenAxisTextSize() {
        return chosenAxisTextSize;
    }

    public void setChosenAxisTextSize(String chosenAxisTextSize) {
        this.chosenAxisTextSize = chosenAxisTextSize;
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

    public void pieBtn_action() {
        PlottingUtils.CreatePieChart(sb, this.facA, "NULL", labx, laby, "NULL", title, "NULL", this.aggregateFunChosen, false);
        PlottingUtils.PlotPieChart(sb, sb.getNewImage("plot_pie_chart"), "png", 72);
    }

}
