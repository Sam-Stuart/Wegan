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
@ManagedBean(name = "bealsBean")
@ViewScoped
public class BealsBean implements Serializable {
    
    
    
    
    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
    // String File names 
    
    private String fileBealsMatrix= "beals_matrix.csv";
    private String fileBealsMatrixPath  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileBealsMatrix + "\">" + fileBealsMatrix + "</a>";
    
    
    // getters and setters
    
    public String getFileBealsMatrixPath() {
        return fileBealsMatrixPath;
    }

    public void setFileBealsMatrixPath(String fileBealsMatrixPath) {
        this.fileBealsMatrixPath = fileBealsMatrixPath;
    }
    
    
    private String bealsSpecies= "NA";

    public String getBealsSpecies() {
        return bealsSpecies;
    }

    public void setBealsSpecies(String bealsSpecies) {
        this.bealsSpecies = bealsSpecies;
    }
    
    private int bealsType= 1;

    public int getBealsType() {
        return bealsType;
    }

    public void setBealsType(int bealsType) {
        this.bealsType = bealsType;
    }
    private boolean bealsIncludeBoolean = true;
    
    public boolean getBealsInclude(){
        return bealsIncludeBoolean;
    }
    
    public void setBealsInclude(boolean bealsInclude){
        this.bealsIncludeBoolean = bealsInclude;
    }
    
    
//    

    private SelectItem[] bealsSpeciesTest = null;
    
    public SelectItem[] getBealsSpeciesTest(){
        bealsSpeciesTest = new SelectItem[3];
        bealsSpeciesTest[0] = new SelectItem("Na", "All");
        bealsSpeciesTest[1] = new SelectItem("1", "1");
        bealsSpeciesTest[2] = new SelectItem("2", "2");
        return bealsSpeciesTest;
    }
    private SelectItem[] bealsSpeciesOpts = null;
    
    public SelectItem[] getBealsSpeciesOpts(){
        String[] columns = DispersalUtils.GetDataColumns(sb);
        int columnsLen = columns.length;
        bealsSpeciesOpts = new SelectItem[columnsLen+1];
        List<String> columnNames = Arrays.asList(columns);
        bealsSpeciesOpts[0] = new SelectItem("All", "All");
        for (int i = 0; i < (columnsLen); i++) {
            bealsSpeciesOpts[i+1] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        
        //List<String> columnNames = Arrays.asList(columns);
        return bealsSpeciesOpts;
    }
    private SelectItem[] bealsTypeOpts = null; 
    
    public SelectItem[] getBealsTypeOpts(){
        bealsTypeOpts = new SelectItem[4];
        bealsTypeOpts[0] = new SelectItem("0", "0");
        bealsTypeOpts[1] = new SelectItem("1", "1");
        bealsTypeOpts[2] = new SelectItem("2", "2");
        bealsTypeOpts[3] = new SelectItem("3", "3");
        
        return bealsTypeOpts;    
        
    }
    
    
    public void bealsBtn_action() {
        
        String bealsIncludeString = bealsIncludeBoolean ? "TRUE" : "FALSE";
        
        DispersalUtils.CreateBeals(sb, bealsSpecies, "data", bealsType, bealsIncludeString);
        DispersalUtils.PlotBeals(sb, sb.getNewImage("beals"), "png", 72, 5,bealsSpecies);
              
        //RequestContext.getCurrentInstance().scrollTo(":ac:form1:bealsPane");
    }

   
}
