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
import javax.faces.bean.ViewScoped;  // added to help customize graph part work

//import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.enterprise.context.RequestScoped;
import javax.inject.Named;

/**
 *
 * @author gpsykes
 */
@ManagedBean(name = "logCABean")
//@ViewScoped
public class LogisticCABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    //TABLES FOR DOWNLOAD
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
    
    //   (Predictor) Indep. Variable NAMES (Textbox)
    private String indInput = "";
    //   (Predictor) Indep. Variable NAMES (Textbox)     AUTOFILL
    private SelectItem[] corrColumnOptsFill = null;
    //   (Predictor) Indep. Variable NAMES (Textbox) get     AUTOFILL
    private String corrFillColumnNames = getFillColumnOpts()[0].getLabel();
    // (Response) Dependent Variable NAMES (Dropdown)
    private SelectItem[] columnOpts = null;
    // (Response) Dependent Variable NAME (get it)
    private String responseVar = getColumnOpts()[0].getLabel();
    // (Response) Dependent Variable LEVELS (dropdown)
    private SelectItem[] columnLevelOpts = null;
    // (Response) Dependent Variable LEVEL (get one)
    private String responseLevelVar = getColumnLevelOpts()[0].getLabel();
    //   Reference Level ORDER (Response variable)
    private String indOrder = "";
    
    private boolean doOriginal = false;
    private boolean doPlotEq = false; 
    private boolean doPlotRsq = false;
    private boolean doPlotRsqAdj = false;
    private boolean doPlotConfInt = false;
    private String corModelType = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corModType    
    private boolean doLabelXtickRotate = false;
    private boolean doPlotLegHoriz = false;
    private String corPlotLegPosOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corLegPosOpts
    private String corPaletteOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corColorPaletteOpts
    private String corPaletteBrewerOpts = "NULL"; // CORRESPONDS WITH applicationBean1.corBrewerPaletteOpts
//    private String corColorDotsOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corLinColorDotsOpts
//    private String corColorLineOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corLinColorLineOpts
    private String corPlotTitle = " ";
    private String corPlotXlab = " ";
    private String corPlotYlab = " ";
    private String corTextSizeTitle= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeTitle
    private String corTextSizeXlab= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeXlab
    private String corTextSizeYlab= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeYlab
    private String corTextSizeXtick= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeXtick
    private String corTextSizeYtick= "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corSizeYtick
    
    
    
    private String getSummaryDownload(){
        String facA = getResponseVar();
        return "logistic_regression_summary_" + facA + ".txt";
    }
    
    private String fileLogModVals = getSummaryDownload();
//    private String fileLogModVals = "corr_logistic_model_summary.txt";
    private String fileLogModValsPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileLogModVals + "\">" + fileLogModVals + "</a>";
 
    public String getFileLogModValsPath() {
        return fileLogModValsPath;
    }
        
    public void setFileLogModValsPath(String fileLogModValsPath) {
        this.fileLogModValsPath = fileLogModValsPath;
    } 

//-----------------------------------------------------------------------------------------------    
 //  DYNAMIC CHECKBOXES 
  // (PREDICTOR) INDEPENDENT VARIABLE
  //  dzone.com/articles/binding-dynamic-multi-select
    //   (Predictor) Indep. Variable NAMES (multiple checkboxes)    
    private List<String> corrPredictors;//items
  
    String[] predictors = CAUtils.GetDataColumns(sb);
    List<String> predictorNames = Arrays.asList(predictors);     
  //fill the check map with the items defaulted to unchecked
    public Map<String,Boolean> hashmapper(){
        Map<String,Boolean> checkMap = new HashMap<String,Boolean>();
            for(String item:predictorNames) {
			checkMap.put(item, Boolean.FALSE);
		}
            return checkMap;
        }
    private Map<String,Boolean> checkMap = hashmapper();    
    
    public List<String> getPredictors() { //items
//	return corrPredictors; //items
        return predictorNames;
    }

    public Map<String, Boolean> getCheckMap() {
	return checkMap;
    }
//        this creates our list of items and populates it with a set of values. The checkMap will be used to track which items are selected and which are not. The map contains a set of String,Boolean pairs.
//Add one more method to return a string telling us which items are selected
    public String getPredictorsSelected() {
	String result = "";
	    for (Entry<String, Boolean> entry : checkMap.entrySet()) {
		    if (entry.getValue()) {
				result = result + ", " + entry.getKey();
			}
		}
		if (result.length() == 0) {
			return "";
		} else {
			return result.substring(2);
		}
	}
    
//-----------------------------------------------------------------------------------------------    
 // (PREDICTOR) INDEPENDENT VARIABLE
    //   (Predictor) Indep. Variable NAMES (Textbox)
//    private String indInput = "";
    public String getIndInput() {
        return indInput;
    }

    public void setIndInput(String indInput) {
        this.indInput = indInput;
    }   
    
    //   (Predictor) Indep. Variable NAMES (Textbox)     AUTOFILL
//    private SelectItem[] corrColumnOptsFill = null;
    public SelectItem[] getFillColumnOpts(){
        String[] columns = CAUtils.GetDataColumns(sb);
        int columnsLen = columns.length;
        corrColumnOptsFill = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            corrColumnOptsFill[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        //List<String> columnNames = Arrays.asList(columns);
        return corrColumnOptsFill;
    }
    
    //   (Predictor) Indep. Variable NAMES (Textbox) get     AUTOFILL
//    private String corrFillColumnNames = getFillColumnOpts()[0].getLabel();
    public String getCorrFillColumnNames() {
        return corrFillColumnNames;
    }

    public void setCorrFillColumnNames(String corrFillColumnNames) {
        this.corrFillColumnNames = corrFillColumnNames;
    }     
    
   // (RESPONSE) DEPENDENT VARIABLE
    
    // (Response) Dependent Variable NAMES (Dropdown)
//    private SelectItem[] columnOpts = null;   
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
    
    // (Response) Dependent Variable NAME (get it)
//    like corrColumnNameA in LinearCABean.java
//    private String responseVar = getColumnOpts()[0].getLabel();
    public String getResponseVar() {
        return responseVar;
    }

    public void setResponseVar(String responseVar) {
        this.responseVar = responseVar;
    }
    
    // (Response) Dependent Variable LEVELS (dropdown)
//    private SelectItem[] columnLevelOpts = null;
    public SelectItem[] getColumnLevelOpts(){
        String[] columns = CAUtils.GetCatLevelDataColumns(sb, responseVar);
        int columnsLen = columns.length;
        columnLevelOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            columnLevelOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        //List<String> columnNames = Arrays.asList(columns);
        return columnLevelOpts;
    }
    
    // (Response) Dependent Variable LEVEL (get one)
//    private String responseLevelVar = getColumnLevelOpts()[0].getLabel();
    public String getResponseLevelVar() {
        return responseLevelVar;
    }

    public void setResponseLevelVar(String responseLevelVar) {
        this.responseLevelVar = responseLevelVar;
    }
    
    //   Reference Level ORDER (Response variable)
//    private String indOrder = "";
    public String getIndOrder() {
        return indOrder;
    }

    public void setIndOrder(String indOrder) {
        this.indOrder = indOrder;
    }
  
    
    
    private List<String> corrPolyResults = null;
    public List<String> getCorrPolyResults(){
        String[] results = CAUtils.GetPolyCAResults(sb, 2);
        corrPolyResults = Arrays.asList(results);
        
        return corrPolyResults;
    }
   
//-----------------------------------------------------------------------------------------------    
//ON-PLOT OPTIONS    
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
//     private boolean doPlotEq = false;
    public boolean isdoPlotEq() {
        return doPlotEq;
    }

    public void setdoPlotEq(boolean doPlotEq) {
        this.doPlotEq = doPlotEq;
    } 
    // CHECK BOX
//     private boolean doPlotRsq = false;
    public boolean isdoPlotRsq() {
        return doPlotRsq;
    }

    public void setdoPlotRsq(boolean doPlotRsq) {
        this.doPlotRsq = doPlotRsq;
    }     
    // CHECK BOX
//     private boolean doPlotRsqAdj = false;
    public boolean isdoPlotRsqAdj() {
        return doPlotRsqAdj;
    }

    public void setdoPlotRsqAdj(boolean doPlotRsqAdj) {
        this.doPlotRsqAdj = doPlotRsqAdj;
    }  
    // CHECK BOX 
//     private boolean doPlotConfInt = false;
    public boolean isdoPlotConfInt() {
        return doPlotConfInt;
    }

    public void setdoPlotConfInt(boolean doPlotConfInt) {
        this.doPlotConfInt = doPlotConfInt;
    }    
    
    //STATIC DROPDOWN    
//    private String corModelType = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corModType
    public String getCorModelType() {
        return corModelType;
    }

    public void setCorModelType(String corModelType) {
        this.corModelType = corModelType;
    }
    
    // CHECK BOX 
//     private boolean doLabelXtickRotate = false;
    public boolean isdoLabelXtickRotate() {
        return doLabelXtickRotate;
    }

    public void setdoLabelXtickRotate(boolean doLabelXtickRotate) {
        this.doLabelXtickRotate = doLabelXtickRotate;
    }    
    
    // CHECK BOX 
//     private boolean doPlotLegHoriz = false;
    public boolean isdoPlotLegHoriz() {
        return doPlotLegHoriz; 
    }

    public void setdoPlotLegHoriz(boolean doPlotLegHoriz) {
        this.doPlotLegHoriz = doPlotLegHoriz;
    }
    
    //STATIC DROPDOWN for selecting colour of line on plot
//    private String corPlotLegPosOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corLegPosOpts
    public String getCorPlotLegPosOpts() {
        return corPlotLegPosOpts;
    }

    public void setCorPlotLegPosOpts(String corPlotLegPosOpts) {
        this.corPlotLegPosOpts = corPlotLegPosOpts;
    }
    
//-----------------------------------------------------------------------------------------------    
//COLOURS    
    //STATIC DROPDOWN for selecting colour:   replaced: corColorOpts : change name from logPaletteOpts to corPaletteOpts, and logColorPaletteOpts to corColorPaletteOpts
//    private String corPaletteOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corColorPaletteOpts
    public String getCorPaletteOpts() {
        return corPaletteOpts; 
    }

    public void setCorPaletteOpts(String corPaletteOpts) {
        this.corPaletteOpts = corPaletteOpts;
    }
     
    //STATIC DROPDOWN for selecting colours, in ROC plot
//    private String corPaletteBrewerOpts = "NULL"; // CORRESPONDS WITH applicationBean1.corBrewerPaletteOpts
    public String getCorPaletteBrewerOpts() {
        return corPaletteBrewerOpts; 
    }

    public void setCorPaletteBrewerOpts(String corPaletteBrewerOpts) {
        this.corPaletteBrewerOpts = corPaletteBrewerOpts;
    }    
//-----------------------------------------------------------------------------------------------
//PLOT LABELS  
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
    
//-----------------------------------------------------------------------------------------------
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
    
//-----------------------------------------------------------------------------------------------    
        // ACTION BUTTONS //
    public void corrLogBtn1_action() {
        responseVar = getColumnOpts()[0].getLabel();
        responseLevelVar = getColumnLevelOpts()[0].getLabel();
        CAUtils.CreateLogisticModel(sb, 
                responseVar, 
                getPredictorsSelected(),
//                indInput,
                doOriginal,
                  corModelType, responseLevelVar, indOrder);
        CAUtils.PlotLogisticEffectCA(sb, doOriginal, corModelType, doPlotConfInt,
                 doLabelXtickRotate, corPaletteOpts, doPlotLegHoriz, corPlotLegPosOpts,
                 corPlotTitle, corPlotXlab, corPlotYlab,   
                 corTextSizeTitle, corTextSizeXlab, corTextSizeYlab, corTextSizeXtick, corTextSizeYtick,
                sb.getCurrentImage("corr_log_eff"), "png", 72);
    }
    
   // ACTION BUTTONS //
    public void corrLogBtn2_action() {
        responseVar = getColumnOpts()[0].getLabel();
        responseLevelVar = getColumnLevelOpts()[0].getLabel();
        CAUtils.CreateLogisticModel(sb,
               responseVar, 
               getPredictorsSelected(),
//                indInput,
                doOriginal,
               corModelType, responseLevelVar, indOrder);
//                columnNameA, indInput);
        CAUtils.PlotLogisticROCCA(sb, doOriginal, corModelType,
               corPaletteBrewerOpts, corPlotTitle, 
                corTextSizeTitle, corTextSizeXlab, corTextSizeYlab, corTextSizeXtick, corTextSizeYtick,
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
