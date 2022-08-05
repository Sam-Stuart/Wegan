/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.plotting;

import java.io.File;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.CAUtils;
import metaboanalyst.rwrappers.DispersalUtils;
import metaboanalyst.rwrappers.PlottingUtils;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.chart.Axis;
import org.primefaces.model.chart.AxisType;
import org.primefaces.model.chart.LineChartModel;
import org.primefaces.model.chart.LineChartSeries;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author Leif
 */
@ManagedBean(name = "boxPlotBean")
@ViewScoped
public class BoxPlotBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void boxBtn_action() {

    
    }
    
    private String box_xAxis= "NULL";
    private String box_yAxis= "NULL";
    
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
   
    private SelectItem[] boxColumnOpts = null;
    private SelectItem[] factorBoxColumnOpts = null;
    private SelectItem[] numericBoxColumnOpts = null;
    
    public SelectItem[] getBoxColumnOpts(){
        String[] columns = PlottingUtils.GetDataColumnsBoxPlt(sb);
        int columnsLen = columns.length;
        boxColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        //boxColumnOpts[0] = new SelectItem("All", " ");
        for (int i = 0; i < (columnsLen); i++) {
            boxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        
        //List<String> columnNames = Arrays.asList(columns);
        return boxColumnOpts;
    }
    
//    private SelectItem[] pcaMetaColumnOpts = null;
//    public SelectItem[] getPcaMetaColumnOpts(){
//        String[] columns = OAUtils.GetPCAMetaColumns(sb);
//        int columnsLen = columns.length;
//        pcaMetaColumnOpts = new SelectItem[columnsLen];
//        List<String> columnNames = Arrays.asList(columns);
//        for (int i = 0; i < columnsLen; i++) {
//            pcaMetaColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
//        }
//        return pcaMetaColumnOpts;
//    }
//    private String pcaMetaColumnName = getPcaMetaColumnOpts()[0].getLabel();
//    
    
    
    public SelectItem[] getFactorBoxColumnOpts(){
        String[] columns = PlottingUtils.GetFactorDataColumnsBoxPlt(sb);
        int columnsLen = columns.length;
        factorBoxColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        //boxColumnOpts[0] = new SelectItem("All", " ");
        for (int i = 0; i < (columnsLen); i++) {
            factorBoxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        
        //List<String> columnNames = Arrays.asList(columns);
        return factorBoxColumnOpts;
    }
    public SelectItem[] getNumericBoxColumnOpts(){
        String[] columns = PlottingUtils.GetNumericDataColumnsBoxPlt(sb);
        int columnsLen = columns.length;
        numericBoxColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        //boxColumnOpts[0] = new SelectItem("All", " ");
        for (int i = 0; i < (columnsLen); i++) {
            numericBoxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        
        //List<String> columnNames = Arrays.asList(columns);
        return numericBoxColumnOpts;
    }
    
    private String xLabel = " ";
    private String yLabel = " ";
    private String legendTitle = "NULL";
    private String mainTitle = "NULL";
    private String boxLabels = "NULL";
    //private String facA = "NULL";
    //private String facB = "NULL";
    private String facC = "NULL"; 
    private String color = "NULL";
    private String type  = "NULL";
    private String data = "NULL";
    
    private String facA = getFactorBoxColumnOpts()[0].getLabel();
    private String facB = getNumericBoxColumnOpts()[0].getLabel();
    
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
    
    public void setxLabel(String xLabel) {
        this.xLabel = xLabel;
    } 
    
    public String getxLabel() {
        return xLabel;
    }
    
    public void setyLabel(String yLabel) {
        this.yLabel = yLabel;
    } 
    
    public String getyLabel() {
        return yLabel;
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
    
    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }
    
    public void boxplotBtn_action() {
        if (!PlottingUtils.CreateBoxPlot(sb, box_xAxis, box_yAxis, facC, color, xLabel, yLabel, legendTitle, mainTitle, data)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }else {
            PlottingUtils.PlotBoxPlot(sb, sb.getNewImage("plot_box_chart"), "png", 72);
        }
//        DispersalUtils.CreateBetaDisper(sb, groups, labels, anovaString);
//        DispersalUtils.PlotBetaDisper(sb, sb.getNewImage("betadisper"), "png", 72, 5);
              
        //RequestContext.getCurrentInstance().scrollTo(":ac:form1:bealsPane");
    }
}
