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
import metaboanalyst.rwrappers.RDataUtils;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author Leif & Hieu
 */
@ManagedBean(name = "boxPlotBean")
@ViewScoped
public class BoxPlotBean extends PlotBean implements Serializable {

    private String legendTitle = "NULL";
    private String boxLabels = "NULL";
    private String color = "NULL";
    private SelectItem[] boxColumnOpts = null;
    private String facC = "NULL";
    private String facB = "NULL";

    public BoxPlotBean() {
        super();

        factorBoxColumnOpts = this.getFactorBoxColumnOpts();
        numericBoxColumnOpts = this.getNumericBoxColumnOpts();

        if (factorBoxColumnOpts.length != 0) {
            this.facC = factorBoxColumnOpts[0].getLabel();
        }

        if (numericBoxColumnOpts.length != 0) {
            this.facB = numericBoxColumnOpts[0].getLabel();
        }
    }

    public SelectItem[] getBoxColumnOpts() {
        String[] columns = PlottingUtils.GetDataColumnsBoxPlt(sb);
        int columnsLen = columns.length;
        boxColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < (columnsLen); i++) {
            boxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }

        return boxColumnOpts;
    }

    @Override
    public void button_action() {
        if (!PlottingUtils.CreateBoxPlot(sb, facC, facB, facC, color, labx, laby, legendTitle, title, data)) {
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        } else {
            PlottingUtils.PlotBoxPlot(sb, sb.getNewImage("plot_box_chart"), "png", 72);
        }
    }

    public String getFacB() {
        return facB;
    }

    public void setFacB(String facB) {
        this.facB = facB;
    }

    public String getFacC() {
        return facC;
    }

    public void setFacC(String facC) {
        this.facC = facC;
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
}
