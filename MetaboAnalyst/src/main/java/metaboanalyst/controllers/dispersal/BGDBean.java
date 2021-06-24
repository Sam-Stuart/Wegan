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
    
    // String File names 
    
    private String fileBGD1= "bgd_DD1.csv";
    private String fileBGD1Path  = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileBGD1+ "\">" + fileBGD1+ "</a>";
    ///home/leif/Wegan/MetaboAnalyst/src/main/java/metaboanalyst/controllers/dispersal/BGDBean.java
    
    // getters and setters
    
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
    
    
    

   
}
