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
@ManagedBean(name = "piePlotBean")
@ViewScoped
public class PiePlotBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private boolean byRow;
    private boolean bySum;
    private boolean lgnd;
    private String labels;
    private String colorChosen;
    private String mainTitle;
    private final SelectItem[] colorOpts;
    
    
    public void setLabels(String labels) {
        this.labels = labels;
    } 
    
    public String getLabels() {
        return labels;
    }
    
    public void setMainTitle(String mainTitle) {
        this.mainTitle = mainTitle;
    } 
    
    public String getMainTitle() {
        return mainTitle;
    }
    
    public boolean isBySum() {
        return bySum;
    }
    
    public void setBySum(boolean bySum) {
        this.bySum = bySum;
    }
    public boolean isLgnd() {
        return lgnd;
    }
    
    public void setLgnd(boolean lgnd) {
        this.lgnd = lgnd;
    }
    public boolean isByRow() {
        return byRow;
    }
    
    public void setByRow(boolean byRow) {
        this.byRow = byRow;
    }
    public String getColorChosen() {
        return colorChosen;
    }

    public void setColorChosen(String colorChosen) {
        this.colorChosen = colorChosen;
    }
    
    public SelectItem[] getColorOpts() {
        return colorOpts;
    }
    
    public void pieBtn_action() {

    
    }
    public PiePlotBean() {

        colorOpts = new SelectItem[5];
        colorOpts[0] = new SelectItem("r", "Rainbow");
        colorOpts[1] = new SelectItem("v", "Viridis");
        colorOpts[2] = new SelectItem("g", "Grey");
        colorOpts[3] = new SelectItem("p", "Plasma");
        colorOpts[4] = new SelectItem("NULL", "Light blue");
        
    }
   
   
}
