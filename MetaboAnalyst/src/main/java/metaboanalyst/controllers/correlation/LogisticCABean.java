/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.correlation;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.CAUtils;
import metaboanalyst.rwrappers.RDataUtils;

import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author dnallen
 */
@ManagedBean(name = "logCABean")
public class LogisticCABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    //TABLES FOR DOWNLOAD
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
    
    private String fileLogModVals = "corr_logistic_model_summary.txt";
//            getSummaryLinDownload();
    private String fileLogModValsPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileLogModVals + "\">" + fileLogModVals + "</a>";
 
    public String getFileLogModValsPath() {
        return fileLogModValsPath;
    }
        
    public void setFileLogModValsPath(String fileLogModValsPath) {
        this.fileLogModValsPath = fileLogModValsPath;
    } 
    
    
//    Independent Variable Names (Textbox)
    private String indInput = "";

    public String getIndInput() {
        return indInput;
    }

    public void setIndInput(String indInput) {
        this.indInput = indInput;
    }
    
    //   Reference Level Order
    private String indOrder = "";

    public String getIndOrder() {
        return indOrder;
    }

    public void setIndOrder(String indOrder) {
        this.indOrder = indOrder;
    }
    
    private SelectItem[] columnOpts = null;
    
    public SelectItem[] getColumnOpts(){
        String[] columns = CAUtils.GetCatDataColumns(sb);
        int columnsLen = columns.length;
        columnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            columnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        //List<String> columnNames = Arrays.asList(columns);
        return columnOpts;
    }
    
    private String responseVar = getColumnOpts()[0].getLabel();
    
    public String getResponseVar() {
        return responseVar;
    }

    public void setResponseVar(String responseVar) {
        this.responseVar = responseVar;
    }
    
    private SelectItem[] columnLevelOpts = null;
    
    public SelectItem[] getColumnLevelOpts(){
        String[] columns = CAUtils.GetCatLevelDataColumns(sb, "NULL");
        int columnsLen = columns.length;
        columnLevelOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            columnLevelOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        //List<String> columnNames = Arrays.asList(columns);
        return columnLevelOpts;
    }
    
    private String responseLevelVar = getColumnLevelOpts()[0].getLabel();
    
    public String getResponseLevelVar() {
        return responseLevelVar;
    }

    public void setResponseLevelVar(String responseLevelVar) {
        this.responseLevelVar = responseLevelVar;
    }
        

    
    private List<String> corrPolyResults = null;
    
    public List<String> getCorrPolyResults(){
        String[] results = CAUtils.GetPolyCAResults(sb, 2);
        corrPolyResults = Arrays.asList(results);
        
        return corrPolyResults;
    }
   
    
    
  // CHECK BOX for using normalized data (default) or original data
    private boolean doOriginal = false;

    public boolean isdoOriginal() {
        return doOriginal;
    }

    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
  // CHECK BOX for adding (default) or omitting equation to plot (at top), see correlation_linear.R 
  // when >1 of rsq, eq, & rsqadj are checked, values are seperated by " | " 
     private boolean doPlotEq = false;

    public boolean isdoPlotEq() {
        return doPlotEq;
    }

    public void setdoPlotEq(boolean doPlotEq) {
        this.doPlotEq = doPlotEq;
    } 
  // CHECK BOX
     private boolean doPlotRsq = false;

    public boolean isdoPlotRsq() {
        return doPlotRsq;
    }

    public void setdoPlotRsq(boolean doPlotRsq) {
        this.doPlotRsq = doPlotRsq;
    }     
  // CHECK BOX
     private boolean doPlotRsqAdj = false;

    public boolean isdoPlotRsqAdj() {
        return doPlotRsqAdj;
    }

    public void setdoPlotRsqAdj(boolean doPlotRsqAdj) {
        this.doPlotRsqAdj = doPlotRsqAdj;
    }  
  // CHECK BOX 
     private boolean doPlotConfInt = false;

    public boolean isdoPlotConfInt() {
        return doPlotConfInt;
    }

    public void setdoPlotConfInt(boolean doPlotConfInt) {
        this.doPlotConfInt = doPlotConfInt;
    }    
    
 //STATIC DROPDOWN    
    private String corModelType = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.logModType

    public String getCorModelType() {
        return corModelType;
    }

    public void setCorModelType(String corModelType) {
        this.corModelType = corModelType;
    }
    
    // CHECK BOX 
     private boolean doLabelXtickRotate = false;

    public boolean isdoLabelXtickRotate() {
        return doLabelXtickRotate;
    }

    public void setdoLabelXtickRotate(boolean doLabelXtickRotate) {
        this.doLabelXtickRotate = doLabelXtickRotate;
    }
    
 //STATIC DROPDOWN for selecting colour:   replaced: corColorOpts : change name from logPaletteOpts to corPaletteOpts, and logColorPaletteOpts to corColorPaletteOpts
    private String corPaletteOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corColorPaletteOpts

    public String getCorPaletteOpts() {
        return corPaletteOpts; 
    }

    public void setCorPaletteOpts(String corPaletteOpts) {
        this.corPaletteOpts = corPaletteOpts;
    }
    
 //STATIC DROPDOWN for selecting colours, in ROC plot
    private String corPaletteBrewerOpts = "NULL"; // CORRESPONDS WITH applicationBean1.corBrewerPaletteOpts

    public String getCorPaletteBrewerOpts() {
        return corPaletteBrewerOpts; 
    }

    public void setCorPaletteBrewerOpts(String corPaletteBrewerOpts) {
        this.corPaletteBrewerOpts = corPaletteBrewerOpts;
    }    
    

    // CHECK BOX 
     private boolean doPlotLegHoriz = false;

    public boolean isdoPlotLegHoriz() {
        return doPlotLegHoriz; 
    }

    public void setdoPlotLegHoriz(boolean doPlotLegHoriz) {
        this.doPlotLegHoriz = doPlotLegHoriz;
    }
    
 //STATIC DROPDOWN for selecting colour of line on plot
    private String corPlotLegPosOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.logLegPosOpts

    public String getCorPlotLegPosOpts() {
        return corPlotLegPosOpts;
    }

    public void setCorPlotLegPosOpts(String corPlotLegPosOpts) {
        this.corPlotLegPosOpts = corPlotLegPosOpts;
    }
    
  
  // TEXT BOX 
    private String corPlotTitle = " ";
    
    public String getCorPlotTitle() {
        return corPlotTitle;
    }

    public void setCorPlotTitle(String corPlotTitle) {
        this.corPlotTitle = corPlotTitle;
    }
 
     // TEXT BOX 
    private String corPlotXlab = " ";
    
    public String getCorPlotXlab() {
        return corPlotXlab;
    }

    public void setCorPlotXlab(String corPlotXlab) {
        this.corPlotXlab = corPlotXlab;
    }       
    
 // TEXT BOX 
    private String corPlotYlab = " ";
    
    public String getCorPlotYlab() {
        return corPlotYlab;
    }

    public void setCorPlotYlab(String corPlotYlab) {
        this.corPlotYlab = corPlotYlab;
    } 
    
    
        // ACTION BUTTONS //
    public void corrLogBtn1_action() {
        CAUtils.CreateLogisticModel(sb, 
                responseVar,indInput,doOriginal,
                  corModelType, responseLevelVar, indOrder);
        CAUtils.PlotLogisticEffectCA(sb, corModelType, doPlotConfInt,
                 corPlotTitle, corPlotXlab, corPlotYlab,               
                doLabelXtickRotate, corPaletteOpts, doPlotLegHoriz, corPlotLegPosOpts,
                sb.getCurrentImage("corr_log_eff"), "png", 72);
    }
    
   // ACTION BUTTONS //
    public void corrLogBtn2_action() {
        CAUtils.CreateLogisticModel(sb,
               responseVar,indInput, doOriginal,
               corModelType, responseLevelVar, indOrder);
//                columnNameA, indInput);
        CAUtils.PlotLogisticROCCA(sb, corModelType,
               corPaletteBrewerOpts, corPlotTitle, 
                sb.getCurrentImage("corr_log_roc"), "png", 72);
    }
    
    
    
    
    
        // ACTION BUTTONS //
//    public void corrLogBtn_action() {
//        CAUtils.CreateSVMModel(sb, columnNameA, indInput);
//        CAUtils.PlotSVMCA(sb, sb.getCurrentImage("corr_svm"), "png", 72);
//    }
//    // ACTION BUTTONS //
//    public void corrPolyPredBtn_action() {
//        System.out.println("Inside poly");
//        CAUtils.CreatePolynomialModel(sb, columnNameA, columnNameB);
//        CAUtils.PlotPolynomialCA(sb, polyDegree, sb.getCurrentImage("corr_poly"), "png", 72);
//        CAUtils.PlotPolynomialPredictCA(sb, 2, sb.getCurrentImage("corr_poly_pred"), "png", 72);
//        System.out.println("Done poly");
//    }    
}
