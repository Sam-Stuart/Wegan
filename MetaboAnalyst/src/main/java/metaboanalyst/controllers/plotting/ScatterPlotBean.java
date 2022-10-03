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
 * @author Leif
 */
@ManagedBean(name = "scatterBean")
@ViewScoped
public class ScatterPlotBean implements Serializable {
    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    
    // Class Necessary Attributes
    private String box_xAxis= "NULL";
    private String box_yAxis= "NULL";
    private SelectItem[] boxColumnOpts = null;
    private SelectItem[] numericBoxColumnOpts = null;
    private String xLabel = " ";
    private String yLabel = " ";
    private String legendTitle = "NULL";
    private String mainTitle = " ";
    private String boxLabels = "NULL";
    private String color = "NULL";
    private boolean data = false;
    private String facA = getNumericBoxColumnOpts()[0].getLabel();
    private String facB = getNumericBoxColumnOpts()[1].getLabel();
    
    public String getBox_xAxis() {
        return this.box_xAxis;
    }

    public void setBox_xAxis(String box_xAxis) {
        this.box_xAxis = box_xAxis;
    }

    public String getBox_yAxis() {
        return this.box_yAxis;
    }

    public void setBox_yAxis(String box_yAxis) {
        this.box_yAxis = box_yAxis;
    }
   
    public void setBoxColumnOpts(){
        String[] columns = PlottingUtils.GetDataColumnsBoxPlt(sb);
        int columnsLen = columns.length;
        this.boxColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < (columnsLen); i++) {
            this.boxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
    }
    
    public SelectItem[] getBoxColumnOpts() {
        if (this.boxColumnOpts == null)
            this.setBoxColumnOpts();
        return this.boxColumnOpts;
    }
 
    public void setNumericBoxColumnOpts(){
        String[] columns = PlottingUtils.GetNumericDataColumnsBoxPlt(sb);
        int columnsLen = columns.length;
        this.numericBoxColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < (columnsLen); i++) {
            this.numericBoxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
    }
    
    public SelectItem[] getNumericBoxColumnOpts() {
        if (this.numericBoxColumnOpts == null)
            this.setNumericBoxColumnOpts();
        return this.numericBoxColumnOpts;
    }
    
    
    
    public String getxLabel() {
        return this.xLabel;
    }
    
    public void setxLabel(String input) {
        this.xLabel = input;
    }
    
    public String getyLabel() {
        return this.yLabel;
    }
    
    public void setyLabel(String input) {
        this.yLabel = input;
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
    
    public void setMainTitle(String mainTitle) {
        this.mainTitle = mainTitle;
    } 
    
    public String getMainTitle() {
        return mainTitle;
    }

    public boolean data() {
        return data;
    }
    
    public void setData(boolean data) {
        this.data = data;
    }
    public void scatterBtn_action() {
        if (!PlottingUtils.CreateScatterChart(sb, facA, facB, "lm", "red", color, xLabel, yLabel, mainTitle, data)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        } else {
            PlottingUtils.PlotScatterChart(sb, sb.getNewImage("plot_scatter_chart"), "png", 72);
        }
    
    }
   
}
