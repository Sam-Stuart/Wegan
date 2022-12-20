/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.correlation;

import java.io.File;
import java.io.Serializable;
//import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
// added:
//import javax.faces.context.FacesContext; 
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
//addded:
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.CAUtils;
import metaboanalyst.rwrappers.RDataUtils;

//added: from RarefactionBean 202207-
//import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "linearCABean")
public class LinearCABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
//     private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");

     //TABLES FOR DOWNLOAD
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
//    private String fileLinModVals[] = null;
    private SelectItem[] corrColumnOpts = null;
    private String corrColumnNameA = getCorrColumnOpts()[1].getLabel();
    private String corrColumnNameB = getCorrColumnOpts()[0].getLabel();
    private boolean doOriginal = false;
    private boolean doPlotEq = false; 
    private boolean doPlotRsq = false;
    private boolean doPlotRsqAdj = false;
    private boolean doPlotConfInt = false;
    private String corColorDotsOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corColorOpts
    private String corColorLineOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corColorOpts
    private String corPlotTitle = " ";
    private String corPlotXlab = " ";
    private String corPlotYlab = " ";
    private String corTextSizeTitle= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeTitle
    private String corTextSizeXlab= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeXlab
    private String corTextSizeYlab= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeYlab
    private String corTextSizeXtick= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeXtick
    private String corTextSizeYtick= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeYtick
    
    
//    private List<String> corrLinearResults = null;
//    
//    public List<String> getCorrLinearResults(){
//        String[] results = CAUtils.GetLinearCAResults(sb);
//        corrLinearResults = Arrays.asList(results);
//        
//        return corrLinearResults;
//    }
//            getSummaryLinDownload();  
   
 
//    FILE NAME FOR SUMMARY TXT CREATED   
//    private String fileLinModVals = "corr_linear_model_summary.txt";

//    DYNAMIC FILENAME (variables of model included)
//    private String fileLinModVals[] = null;
//    public void getCorrLinearResults(){
//        this.fileLinModVals = CAUtils.GetLinearCAResults(sb);
//    }  
//    
//    private String fileLinModValsPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileLinModVals[0] + "\">" + fileLinModVals[0] + "</a>";
    
    
    private String getSummaryDownload(){
        String facA = getCorrColumnNameA();
        String facB = getCorrColumnNameB();
        return "linear_regression_summary_" + facA + "-" + facB + ".txt";
    }
        
    private String fileLinModVals = getSummaryDownload();
    private String fileLinModValsPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileLinModVals + "\">" + fileLinModVals + "</a>";
    
    public String getFileLinModValsPath() {
        return fileLinModValsPath;
    }
        
    public void setFileLinModValsPath(String fileLinModValsPath) {
        this.fileLinModValsPath = fileLinModValsPath;
    } 
    // GET COLUMN NAMES
//    try to get it with cleaner, variable replacing way (didn't work (202211-15
//    public SelectItem[] getCorrColumnOpts(SelectItem[] nameofvar){
//        String[] columns = CAUtils.GetDataColumns(sb);
//        int columnsLen = columns.length;
//        nameofvar = new SelectItem[columnsLen];
//        List<String> columnNames = Arrays.asList(columns);
//        for (int i = 0; i < columnsLen; i++) {
//            nameofvar[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
//        }
//        //List<String> columnNames = Arrays.asList(columns);
//        return nameofvar;
//    }
    
//    private SelectItem[] corrColumnOpts = null;
    public SelectItem[] getCorrColumnOpts(){
        String[] columns = CAUtils.GetDataColumns(sb);
        int columnsLen = columns.length;
        corrColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            corrColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        //List<String> columnNames = Arrays.asList(columns);
        return corrColumnOpts;
    }
    
//    private String corrColumnNameA = getCorrColumnOpts()[1].getLabel();
    public String getCorrColumnNameA() {
        return corrColumnNameA;
    }

    public void setCorrColumnNameA(String corrColumnNameA) {
        this.corrColumnNameA = corrColumnNameA;
    }
    
//    private String corrColumnNameB = getCorrColumnOpts()[0].getLabel();
    public String getCorrColumnNameB() {
        return corrColumnNameB;
    }

    public void setCorrColumnNameB(String corrColumnNameB) {
        this.corrColumnNameB = corrColumnNameB;
    }    
    
  // CHECK BOX for using normalized data (default) or original data
//    private boolean doOriginal = false;
    public boolean isdoOriginal() {
        return doOriginal;
    }

    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
    
  // CHECK BOX for adding (default) or omitting equation to plot (at top), see correlation_linear.R 
  // when >1 of rsq, eq, & rsqadj are checked, values are seperated by " | " 
//    private boolean doPlotEq = false;
    public boolean isdoPlotEq() {
        return doPlotEq;
    }

    public void setdoPlotEq(boolean doPlotEq) {
        this.doPlotEq = doPlotEq;
    } 
  // CHECK BOX for adding (default) or omitting rsq to plot (at top), see correlation_linear.R 
  // when >1 of rsq, eq, & rsqadj are checked, values are seperated by " | " 
//     private boolean doPlotRsq = false;
    public boolean isdoPlotRsq() {
        return doPlotRsq;
    }

    public void setdoPlotRsq(boolean doPlotRsq) {
        this.doPlotRsq = doPlotRsq;
    }     
  // CHECK BOX for omitting (default) or adding rsq-adj to plot (at top), see correlation_linear.R 
  // when >1 of rsq, eq, & rsqadj are checked, values are seperated by " | " 
//    private boolean doPlotRsqAdj = false;
    public boolean isdoPlotRsqAdj() {
        return doPlotRsqAdj;
    }

    public void setdoPlotRsqAdj(boolean doPlotRsqAdj) {
        this.doPlotRsqAdj = doPlotRsqAdj;
    }  
  // CHECK BOX for omitting (default) or adding confidence interval to line, see correlation_linear.R 
//    private boolean doPlotConfInt = false;
    public boolean isdoPlotConfInt() {
        return doPlotConfInt;
    }

    public void setdoPlotConfInt(boolean doPlotConfInt) {
        this.doPlotConfInt = doPlotConfInt;
    }    
    
 //STATIC DROPDOWN for selecting colour of dots on plot
//    replaced: corColorOpts
//    private String corColorDotsOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corColorOpts
    public String getCorColorDotsOpts() {
        return corColorDotsOpts;
    }

    public void setCorColorDotsOpts(String corColorDotsOpts) {
        this.corColorDotsOpts = corColorDotsOpts;
    }

 //STATIC DROPDOWN for selecting colour of line on plot
//    private String corColorLineOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corColorOpts
    public String getCorColorLineOpts() {
        return corColorLineOpts;
    }

    public void setCorColorLineOpts(String corColorLineOpts) {
        this.corColorLineOpts = corColorLineOpts;
    }
  
  // TEXT BOX 
//    private String corPlotTitle = " ";
    public String getCorPlotTitle() {
        return corPlotTitle;
    }

    public void setCorPlotTitle(String corPlotTitle) {
        this.corPlotTitle = corPlotTitle;
    }
 
  // TEXT BOX 
//    private String corPlotXlab = " ";
    public String getCorPlotXlab() {
        return corPlotXlab;
    }

    public void setCorPlotXlab(String corPlotXlab) {
        this.corPlotXlab = corPlotXlab;
    }       
    
 // TEXT BOX 
//    private String corPlotYlab = " ";
    public String getCorPlotYlab() {
        return corPlotYlab;
    }

    public void setCorPlotYlab(String corPlotYlab) {
        this.corPlotYlab = corPlotYlab;
    }   
     
//TEXT SIZE    
       //STATIC DROPDOWN title text size
//    WAS: corPlotLabelSize; applicaitonbean: corPlotLabSize
//    private String corTextSizeTitle= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeTitle
    public String getCorTextSizeTitle() {
        return corTextSizeTitle;
    }

    public void setCorTextSizeTitle(String corTextSizeTitle) {
        this.corTextSizeTitle = corTextSizeTitle;
    }  
    
       //STATIC DROPDOWN title text size
//    private String corTextSizeXlab= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeXlab
    public String getCorTextSizeXlab() {
        return corTextSizeXlab;
    }

    public void setCorTextSizeXlab(String corTextSizeXlab) {
        this.corTextSizeXlab = corTextSizeXlab;
    }  
    
       //STATIC DROPDOWN title text size
//    private String corTextSizeYlab= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeYlab
    public String getCorTextSizeYlab() {
        return corTextSizeYlab;
    }

    public void setCorTextSizeYlab(String corTextSizeYlab) {
        this.corTextSizeYlab = corTextSizeYlab;
    }  
    
       //STATIC DROPDOWN title text size
//    private String corTextSizeXtick= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeXtick
    public String getCorTextSizeXtick() {
        return corTextSizeXtick;
    }

    public void setCorTextSizeXtick(String corTextSizeXtick) {
        this.corTextSizeXtick = corTextSizeXtick;
    } 
    
       //STATIC DROPDOWN title text size
//    private String corTextSizeYtick= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeYtick
    public String getCorTextSizeYtick() {
        return corTextSizeYtick;
    }

    public void setCorTextSizeYtick(String corTextSizeYtick) {
        this.corTextSizeYtick = corTextSizeYtick;
    } 
    
    
//    private String corPlotLinearWhich = "NULL";
// 
//    public String getCorPlotLinearWhich() {
//        return corPlotLinearWhich;
//    }
//        
//    public void setCorPlotLinearWhich(String corPlotLinearWhich) {
//        this.corPlotLinearWhich = corPlotLinearWhich;
//    } 
    
    
//    // ACTION BUTTONS //
//    public void corrLin1Btn_action() {
//        CAUtils.CreateLinearModel(sb, corrColumnNameA, corrColumnNameB);
//        //CAUtils.CreateLinearModel(sb, "/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/target/MetaboAnalyst-4.34/resources/data/dune_weights.csv");
//        CAUtils.PlotLinearCA(sb, sb.getNewImage("corr_linear"), "png", 72);
//    }
//
//    
//}
    
    // ACTION BUTTONS //
    public void corrLin1Btn_action() {
        CAUtils.CreateLinearModel(sb, corrColumnNameA, corrColumnNameB, doOriginal);
        //CAUtils.CreateLinearModel(sb, "/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/target/MetaboAnalyst-4.34/resources/data/dune_weights.csv");
//        corrLin1_Update_action
        
        CAUtils.PlotLinearCA(sb, 
                corrColumnNameA, corrColumnNameB, 
                doOriginal, 
                corColorDotsOpts, corColorLineOpts, 
               doPlotConfInt, doPlotEq, doPlotRsq, doPlotRsqAdj,
               corPlotTitle, corPlotXlab, corPlotYlab,
               corTextSizeTitle, corTextSizeXlab, corTextSizeYlab, corTextSizeXtick, corTextSizeYtick,
//                 sb.getCurrentImage("corr_linear"),"png", 72);
          sb.getNewImage("corr_linear"),"png", 72); 
//        CAUtils.ConvertLinearJSONCA(sb, corPlotLinearWhich);
    }
   

 // ACTION BUTTONS //
    public void corrLin2Btn_action() {
        CAUtils.CreateLinearModel(sb, corrColumnNameA, corrColumnNameB, doOriginal);
        //CAUtils.CreateLinearModel(sb, "/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/target/MetaboAnalyst-4.34/resources/data/dune_weights.csv");
//        corrLin1_Update_action
        
        CAUtils.PlotLinearPredictCA(sb,
                corrColumnNameA, corrColumnNameB, 
                doOriginal,
                corColorDotsOpts, corColorLineOpts, doPlotConfInt,
                doPlotEq, doPlotRsq, doPlotRsqAdj,
               corPlotTitle, corPlotXlab, corPlotYlab,
               corTextSizeTitle, corTextSizeXlab, corTextSizeYlab, corTextSizeXtick, corTextSizeYtick,
          sb.getNewImage("corr_linear_pred"),"png", 72);
//         CAUtils.ConvertLinearJSONCA(sb, corPlotLinearWhich);        

    }
   
    
     public void corrLin3Btn_action() {
        CAUtils.CreateLinearModel(sb, corrColumnNameA, corrColumnNameB, doOriginal);
        //CAUtils.CreateLinearModel(sb, "/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/target/MetaboAnalyst-4.34/resources/data/dune_weights.csv");
//        corrLin1_Update_action
        
        CAUtils.PlotLinearNormResidCA(sb, 
                corrColumnNameA, corrColumnNameB, 
                doOriginal,
                corColorDotsOpts, corColorLineOpts, 
               corPlotTitle, corPlotXlab, corPlotYlab,
               corTextSizeTitle, corTextSizeXlab, corTextSizeYlab, corTextSizeXtick, corTextSizeYtick,
          sb.getNewImage("corr_linear_normres"),"png", 72);
//         CAUtils.ConvertLinearJSONCA(sb, corPlotLinearWhich);                 

    }
     
      public void corrLin4Btn_action() {
         CAUtils.CreateLinearModel(sb, corrColumnNameA, corrColumnNameB, doOriginal);         
//        CAUtils.CreateLinearModel(sb, "Petal_Width", "Sepal_Length", doOriginal);
        //CAUtils.CreateLinearModel(sb, "/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/target/MetaboAnalyst-4.34/resources/data/dune_weights.csv");
//        corrLin1_Update_action
        
        CAUtils.PlotLinearResidFitCA(sb, 
                corrColumnNameA, corrColumnNameB, doOriginal,
                corColorDotsOpts, corColorLineOpts, 
               corPlotTitle, corPlotXlab, corPlotYlab,
               corTextSizeTitle, corTextSizeXlab, corTextSizeYlab, corTextSizeXtick, corTextSizeYtick,
          sb.getNewImage("corr_linear_resfit"),"png", 72);
//        CAUtils.ConvertLinearJSONCA(sb, corPlotLinearWhich);              

    }
      
//        private String getSummaryLinDownload(){
//        String facA = getCorrColumnNameA();
//        String facB = getCorrColumnNameB();
//        return "linear_regession_summary_" + facA + "~" + facB + ".txt";
//    }
    
    
}