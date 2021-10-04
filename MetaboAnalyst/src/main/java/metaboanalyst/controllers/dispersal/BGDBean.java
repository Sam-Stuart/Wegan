/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.dispersal;

import metaboanalyst.controllers.plotting.*;
import java.io.File;
import java.io.Serializable;
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
@ManagedBean(name = "BGDBean")
@ViewScoped
public class BGDBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
    
    
    public static double[][] GetBGDSigMat(SessionBean1 sb, String tableName) {
        System.out.print(tableName);
        try {
            String rCommand = "GetBGDSigMat(NA" + ",\""+  tableName + "\")";
            return sb.getRConnection().eval(rCommand).asDoubleMatrix();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }


    public static String[] GetBGDSigRowNames(SessionBean1 sb, String tableName) {
        try {
            String rCommand = "GetBGDSigRowNames(NA" + ",\""+  tableName + "\")";
            System.out.print(rCommand);
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }

    public static String[] GetBGDSigColNames(SessionBean1 sb, String tableName) {
        try {
            String rCommand = "GetBGDSigColNames(NA" + ",\""+  tableName + "\")";
            return sb.getRConnection().eval(rCommand).asStrings();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }
    public static String GetBGDSigFileName(SessionBean1 sb, String tableName) {
        try {
            String rCommand = "GetBGDSigFileName(NA)";
            //  RCenter.recordRCommand(RC, rCommand);
            return sb.getRConnection().eval(rCommand).asString();
        } catch (Exception rse) {
            System.out.println(rse);
        }
        return null;
    }
    
     // String File names 
    //DD1
    private String fileDD1= "DD1.csv";
    private String fileDD1Path  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileDD1 + "\">" + fileDD1 + "</a>";
    //DD2
    private String fileDD2= "DD2.csv";
    private String fileDD2Path  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileDD2 + "\">" + fileDD2 + "</a>";
    //DD3
    private String fileDD3= "DD3.csv";
    private String fileDD3Path  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileDD3 + "\">" + fileDD3 + "</a>";
    //DD4
    private String fileDD4= "DD4.csv";
    private String fileDD4Path  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileDD4 + "\">" + fileDD4 + "</a>";
    //McNemar
    private String fileMcNemar= "McNemar.csv";
    private String fileMcNemarPath  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileMcNemar + "\">" + fileMcNemar + "</a>";
    //ProbMcNemar
    private String fileProbMcNemar= "ProbMcNemar.csv";
    private String fileProbMcNemarPath  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileProbMcNemar + "\">" + fileProbMcNemar + "</a>";
    
    // getters and setters
    //DD1
    public String getFileDD1Path() {
        return fileDD1Path;
    }

    public void setFileDD1Path(String fileDD1Path) {
        this.fileDD1Path = fileDD1Path;
    }
    //DD2
    public String getFileDD2Path() {
        return fileDD2Path;
    }

    public void setFileDD2Path(String fileDD2Path) {
        this.fileDD2Path = fileDD2Path;
    }
    //DD3
    public String getFileDD3Path() {
        return fileDD3Path;
    }

    public void setFileDD3Path(String fileDD3Path) {
        this.fileDD3Path = fileDD3Path;
    }
    //DD4
    public String getFileDD4Path() {
        return fileDD4Path;
    }

    public void setFileDD4Path(String fileDD4Path) {
        this.fileDD4Path = fileDD4Path;
    }
    //McNemar
    public String getFileMcNemarPath() {
        return fileMcNemarPath;
    }

    public void setFileMcNemarPath(String fileMcNemarPath) {
        this.fileMcNemarPath = fileMcNemarPath;
    }
    //ProbMcNemar
    public String getFileProbMcNemarPath() {
        return fileProbMcNemarPath;
    }

    public void setFileProbMcNemarPath(String fileProbMcNemarPath) {
        this.fileProbMcNemarPath = fileProbMcNemarPath;
    }
    
    

   
}
