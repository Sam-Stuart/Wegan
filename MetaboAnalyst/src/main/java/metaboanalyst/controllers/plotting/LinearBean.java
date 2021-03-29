/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

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
@ManagedBean(name = "linear")
@ViewScoped
public class LinearBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    
    private String linearPlotType = "l";

    public String getLinearPlotType() {
        return linearPlotType;
    }

    public void setLinearPlotType(String linearPlotType) {
        this.linearPlotType = linearPlotType;
    }
    
    private int linNumLines = 1;

    public int getLinNumLines() {
        return linNumLines;
    }

    public void setLinNumLines(int linNumLines) {
        this.linNumLines = linNumLines;
    }
    
    private String linColor = "black";

    public String getLinColor() {
        return linColor;
    }

    public void setLinColor(String linColor) {
        this.linColor = linColor;
    }
    
    private int linWeight = 1;
    
    public int getLinWeight(){
        return linWeight;
    }

    public void setLinWeight(int linWeight){
        this.linWeight = linWeight; 
    }
    
//    defines what shapes to use as points on the graph, default is filled in bullets.
    private int linPchs = 21;  
    
    public int getLinPchs(){
        return linPchs;
    }

    public void setLinPchs(int linPchs){
        this.linPchs = linPchs; 
    }
    
    private String linxLabel = "x axis";

    public String getLinxLabel() {
        return linxLabel;
    }

    public void setLinxLabel(String linxLabel) {
        this.linxLabel = linxLabel;
    }
    
    private String linyLabel = "y axis";

    public String getLinyLabel() {
        return linyLabel;
    }

    public void setLinyLabel(String linyLabel) {
        this.linyLabel = linyLabel;
    }
    
    private String linTitle = "Graph Title";

    public String getLinTitle() {
        return linTitle;
    }

    public void setLinTitle(String linTitle) {
        this.linTitle = linTitle;
    }
    
    
    private SelectItem[] linColumnOpts = null;
    
    public SelectItem[] getLinColumnOpts(){
        String[] columns = PlottingUtils.GetDataColumns(sb);
        int columnsLen = columns.length;
        linColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            linColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        //List<String> columnNames = Arrays.asList(columns);
        return linColumnOpts;
    }
    private String linColumnNameA = getLinColumnOpts()[0].getLabel();
    
    public String getLinColumnNameA() {
        return linColumnNameA;
    }

    public void setLinColumnNameA(String linColumnNameA) {
        this.linColumnNameA = linColumnNameA;
    }
    
    private String linColumnNameB = getLinColumnOpts()[1].getLabel();
    
    public String getLinColumnNameB() {
        return linColumnNameB;
    }

    public void setLinColumnNameB(String linColumnNameB) {
        this.linColumnNameB = linColumnNameB;
    }
    public void linearBtn_action() {

        PlottingUtils.PlotlinearGraph(sb, sb.getNewImage("lin"), "png", 72, 
                linearPlotType, linNumLines, linColor, linWeight, linPchs, linxLabel, linyLabel, linTitle, linColumnNameA, linColumnNameB);
        RequestContext.getCurrentInstance().scrollTo(":ac:form1:linPane");
    }

   
}
