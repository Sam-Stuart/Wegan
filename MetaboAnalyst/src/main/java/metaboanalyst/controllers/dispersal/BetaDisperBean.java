/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.dispersal;

import metaboanalyst.controllers.plotting.*;
import java.io.File;
import java.io.Serializable;
import static java.lang.Boolean.TRUE;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.User;
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
@ManagedBean(name = "betadisperBean")
@ViewScoped
public class BetaDisperBean implements Serializable {
    
    
    
    
    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
    // String File names 
    
    private String groups;
    private String labels;
    
    private boolean anovaBoolean = false;
    
   
    
    private String fileBetaEig= "eigenvalues_betadispersal.csv";
    private String fileBetaEigPath  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileBetaEig + "\">" + fileBetaEig + "</a>";
    
    private String fileBetaVectors= "vectors_betadispersal.csv";
    private String fileBetaVectorsPath  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileBetaVectors + "\">" + fileBetaVectors + "</a>";
    
    private String fileBetaCentroids= "centroids_betadispersal.csv";
    private String fileBetaCentroidsPath  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileBetaCentroids + "\">" + fileBetaCentroids + "</a>";
    
    
    // getters and setters
    public void setGroups(String groups) {
        this.groups = groups;
    } 
    
    public String getGroups() {
        return groups;
    }
    
    public void setLabels(String labels) {
        this.labels = labels;
    } 
    
    public String getlabels() {
        return labels;
    }
    
    
     public boolean getAnovaBoolean(){
        return anovaBoolean;
    }
    
    public void setAnovaBoolean(boolean anovaBoolean){
        this.anovaBoolean = anovaBoolean;
    }
    
    String anovaString = anovaBoolean ? "TRUE" : "FALSE";
    
    public String getFileBetaEigPath() {
        return fileBetaEigPath;
    }

    public void setFileBetaEigPath(String fileBetaEigPath) {
        this.fileBetaEigPath = fileBetaEigPath;
    }
    
    public String getFileBetaVectorsPath() {
        return fileBetaVectorsPath;
    }

    public void setFileBetaVectorsPath(String fileBetaVectorsPath) {
        this.fileBetaVectorsPath = fileBetaVectorsPath;
    }
    
    public String getFileBetaCentroidsPath() {
        return fileBetaCentroidsPath;
    }

    public void setFileBetaCentroidsPath(String fileBetaCentroidsPath) {
        this.fileBetaCentroidsPath = fileBetaCentroidsPath;
    }
    
    
    
    
    
    public void betadisperBtn_action() {
        
        String bealsIncludeString = anovaBoolean ? "TRUE" : "FALSE";
        System.out.print("BetaDisperButton action");
        System.out.print(groups);
        System.out.print(labels);
        System.out.print(bealsIncludeString);
        if (!DispersalUtils.CreateBetaDisper(sb, groups, labels, anovaString)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }else {
            DispersalUtils.PlotBetaDisper(sb, sb.getNewImage("betadisper"), "png", 72, 5);
        }
//        DispersalUtils.CreateBetaDisper(sb, groups, labels, anovaString);
//        DispersalUtils.PlotBetaDisper(sb, sb.getNewImage("betadisper"), "png", 72, 5);
              
        //RequestContext.getCurrentInstance().scrollTo(":ac:form1:bealsPane");
    }
}
