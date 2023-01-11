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
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.PlottingUtils;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author Leif & Hieu
 */
@ManagedBean(name = "boxPlotBean")
@ViewScoped
public class BoxPlotBean extends PlotBean implements Serializable{

    private String box_xAxis = "NULL";
    private String box_yAxis = "NULL";
    private String legendTitle = "NULL";
    private String boxLabels = "NULL";
    private String color = "NULL";
    private SelectItem[] boxColumnOpts = null;
    private SelectItem[] factorBoxColumnOpts = null;
    private SelectItem[] numericBoxColumnOpts = null;
    private String facC = getFactorBoxColumnOpts()[0].getLabel();
    private String facB = getNumericBoxColumnOpts()[0].getLabel();

    public BoxPlotBean() {
        super();
    }
 

    public String getBox_xAxis() {
        return box_xAxis;
    }

    public void setBox_xAxis(String box_xAxis) {
        this.box_xAxis = box_xAxis;
    }

    public String getBox_yAxis() {
        return box_yAxis;
    }

    public void setBox_yAxis(String box_yAxis) {
        this.box_yAxis = box_yAxis;
    }

    public SelectItem[] getBoxColumnOpts() {
        String[] columns = PlottingUtils.GetDataColumnsBoxPlt(sb);
        int columnsLen = columns.length;
        boxColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        //boxColumnOpts[0] = new SelectItem("All", " ");
        for (int i = 0; i < (columnsLen); i++) {
            boxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }

        return boxColumnOpts;
    }
    
    public SelectItem[] getFactorBoxColumnOpts() {
        String[] columns = PlottingUtils.GetFactorDataColumnsBoxPlt(sb);

        if (columns != null) {
            int columnsLen = columns.length;
            factorBoxColumnOpts = new SelectItem[columnsLen];
            List<String> columnNames = Arrays.asList(columns);
            for (int i = 0; i < (columnsLen); i++) {
                factorBoxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
            }
            return factorBoxColumnOpts;
        }

        return new SelectItem[0];
    }

    public SelectItem[] getNumericBoxColumnOpts() {
        String[] columns = PlottingUtils.GetNumericDataColumnsBoxPlt(sb);
        int columnsLen = columns.length;
        numericBoxColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < (columnsLen); i++) {
            numericBoxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }

        return numericBoxColumnOpts;
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

    public void button_action() {
        if (!PlottingUtils.CreateBoxPlot(sb, facC, facB, facC, color, labx, laby, legendTitle, title, data)) {
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        } else {
            PlottingUtils.PlotBoxPlot(sb, sb.getNewImage("plot_box_chart"), "png", 72);
        }
    }
}
