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
    
    private String box_xAxis= "NA";

    public String getBox_xAxis() {
        return box_xAxis;
    }

    public void setBox_xAxis(String box_xAxis) {
        this.box_xAxis = box_xAxis;
    }
    
    private String box_yAxis= "NA";

    public String getBox_yAxis() {
        return box_yAxis;
    }

    public void setBox_yAxis(String box_yAxis) {
        this.box_yAxis = box_yAxis;
    }
   
    private SelectItem[] boxColumnOpts = null;
    
    public SelectItem[] getBoxColumnOpts(){
        String[] columns = PlottingUtils.GetDataColumns(sb);
        int columnsLen = columns.length;
        boxColumnOpts = new SelectItem[columnsLen+1];
        List<String> columnNames = Arrays.asList(columns);
        boxColumnOpts[0] = new SelectItem("All", "All");
        for (int i = 0; i < (columnsLen); i++) {
            boxColumnOpts[i+1] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        
        //List<String> columnNames = Arrays.asList(columns);
        return boxColumnOpts;
    }
    
    private String xLabel = "null";
    private String yLabel = "null";
    private String legendTitle = "null";
    private String mainTitle = "null";
    private String boxLabels = "null";
    private String facA = "null";
    private String facB = "null";
    private String facC = "null"; 
    private String color = "null";
    private String type  = "null";
    
    
    
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
    
    public void boxplotBtn_action() {
        
        if (!PlottingUtils.CreateBoxPlot(sb, facA, facB, facC, type, color, xLabel, yLabel, boxLabels, legendTitle, mainTitle)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }else {
            PlottingUtils.PlotBoxPlot(sb, sb.getNewImage("boxplot"), "png", 72);
        }
//        DispersalUtils.CreateBetaDisper(sb, groups, labels, anovaString);
//        DispersalUtils.PlotBetaDisper(sb, sb.getNewImage("betadisper"), "png", 72, 5);
              
        //RequestContext.getCurrentInstance().scrollTo(":ac:form1:bealsPane");
    }
}
