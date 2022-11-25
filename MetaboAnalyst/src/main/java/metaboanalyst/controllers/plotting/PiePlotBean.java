
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.plotting;

import java.io.Serializable;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.PlottingUtils;
import metaboanalyst.utils.DataUtils;

/**
 *
 * @author Leif
 */
@ManagedBean(name = "piePlotBean")
@ViewScoped
public class PiePlotBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private String labx;
    private String laby;
    private String title;
    
    
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
        PlottingUtils.CreatePieChart(sb, "NULL", "NULL", labx, laby, "NULL", title, "NULL", "NULL", false);
        PlottingUtils.PlotBarChart(sb,  sb.getNewImage("plot_pie_chart"), "png", 72); 
    }
   
}
