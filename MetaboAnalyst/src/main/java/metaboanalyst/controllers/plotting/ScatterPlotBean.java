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
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author Leif & Hieu
 */
@ManagedBean(name = "scatterBean")
@ViewScoped
public class ScatterPlotBean extends PlotBean implements Serializable {

    // Class Necessary Attributes
    private SelectItem[] boxColumnOpts = null;
    private SelectItem[] numericBoxColumnOpts = null;
    private String legendTitle = "NULL";
    private String boxLabels = "NULL";
    private String lineColor = "NULL";
    private String color = "NULL";
    private String facA = getNumericBoxColumnOpts()[0].getLabel();
    private String facB = getNumericBoxColumnOpts()[1].getLabel();

    public ScatterPlotBean() {
        super();
    }
//    Get dependant data

    public void setBoxColumnOpts() {
        String[] columns = PlottingUtils.GetDataColumnsBoxPlt(sb);
        int columnsLen = columns.length;
        this.boxColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < (columnsLen); i++) {
            this.boxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
    }

    public SelectItem[] getBoxColumnOpts() {
        if (this.boxColumnOpts == null) {
            this.setBoxColumnOpts();
        }
        return this.boxColumnOpts;
    }
//Get independant data

    public void setNumericBoxColumnOpts() {
        String[] columns = PlottingUtils.GetNumericDataColumnsBoxPlt(sb);
        int columnsLen = columns.length;
        this.numericBoxColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < (columnsLen); i++) {
            this.numericBoxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
    }

    public SelectItem[] getNumericBoxColumnOpts() {
        if (this.numericBoxColumnOpts == null) {
            this.setNumericBoxColumnOpts();
        }
        return this.numericBoxColumnOpts;
    }

    public String getFacA() {
        return facA;
    }

    public void setFacA(String facA) {
        this.facA = facA;
    }

    public String getFacB() {
        return facB;
    }

    public void setFacB(String facB) {
        this.facB = facB;
    }

    public String getColor() {
        return color;
    }

    public void setColor(String color) {
        this.color = color;
    }

    public void setboxLabels(String boxLabels) {
        this.boxLabels = boxLabels;
    }

    public String getboxLabels() {
        return boxLabels;
    }

    public void setLegendTitle(String legendTitle) {
        this.legendTitle = legendTitle;
    }

    public String getLegendTitle() {
        return legendTitle;
    }

    public void setLineColor(String lineColor) {
        this.lineColor = color;
    }

    public String getLineColor() {
        return this.lineColor;
    }

    @Override
    public void button_action() {
        if (!PlottingUtils.CreateScatterChart(sb, facA, facB, "lm", lineColor, color, this.labx, this.laby, this.title, this.chosenTitleTextSize, this.chosenAxisTextSize, this.data)) {
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        } else {
            PlottingUtils.PlotScatterChart(sb, sb.getNewImage("plot_scatter_chart"), "png", 72);
        }
    }

}
