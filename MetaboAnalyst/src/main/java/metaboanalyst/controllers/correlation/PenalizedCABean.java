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

import javax.faces.bean.ManagedBean; // added to help customize graph part work
import javax.faces.bean.ViewScoped;  // added to help customize graph part work

/**
 *
 * @author dnallen
 */
@ManagedBean(name = "penalCABean")
@ViewScoped
public class PenalizedCABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

      //TABLES FOR DOWNLOAD
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
    
    private String filePenModVals = "penalized_regression_summary.txt";
//            getSummaryLinDownload();
    private String filePenModValsPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filePenModVals + "\">" + filePenModVals + "</a>";
 
    public String getFilePenModValsPath() {
        return filePenModValsPath;
    }
        
    public void setFilePenModValsPath(String filePenModValsPath) {
        this.filePenModValsPath = filePenModValsPath;
    } 

    
    
    private boolean addWeights = false;
    
    public boolean isaddWeights() {
        return addWeights;
    }

    public void setAddWeights(boolean addWeights) {
        this.addWeights = addWeights;
    }

    
    private SelectItem[] columnOpts = null;
    
    public SelectItem[] getColumnOpts(){
        String[] columns = CAUtils.GetPenalizedColumns(sb);
        int columnsLen = columns.length;
        columnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            columnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        //List<String> columnNames = Arrays.asList(columns);
        return columnOpts;
    }
    
    private String columnNameA = getColumnOpts()[0].getLabel();
    
    public String getColumnNameA() {
        return columnNameA;
    }

    public void setColumnNameA(String columnNameA) {
        this.columnNameA = columnNameA;
    }

    private String columnNameB = getColumnOpts()[1].getLabel();
    
    public String getColumnNameB() {
        return columnNameB;
    }

    public void setColumnNameB(String columnNameB) {
        this.columnNameB = columnNameB;
    }
    
    
       // CHECK BOX for using normalized data (default) or original data
    private boolean doOriginal = false;

    public boolean isdoOriginal() {
        return doOriginal;
    }

    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
   
  // CHECK BOX for omitting (default) or adding confidence interval to line, see correlation_linear.R 
     private boolean doPlotConfInt = false;

    public boolean isdoPlotConfInt() {
        return doPlotConfInt;
    }

    public void setdoPlotConfInt(boolean doPlotConfInt) {
        this.doPlotConfInt = doPlotConfInt;
    }    
    
 //STATIC DROPDOWN for selecting colour of dots on plot
//    replaced: corColorOpts
    private String corColorDotsOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corColorOpts

    public String getCorColorDotsOpts() {
        return corColorDotsOpts;
    }

    public void setCorColorDotsOpts(String corColorDotsOpts) {
        this.corColorDotsOpts = corColorDotsOpts;
    }

 //STATIC DROPDOWN for selecting colour of line on plot
    private String corColorLineOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corColorOpts

    public String getCorColorLineOpts() {
        return corColorLineOpts;
    }

    public void setCorColorLineOpts(String corColorLineOpts) {
        this.corColorLineOpts = corColorLineOpts;
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
    
    
    private List<String> corrPenalResults = null;
    
    public List<String> getCorrPenalResults(){
        String[] results = CAUtils.GetPenalizedCAResults(sb);
        corrPenalResults = Arrays.asList(results);
        
        return corrPenalResults;
    }
    
// ACTION BUTTONS //
    public void penalizedUpdate_action() {
        System.out.println(corrFunctionMethods);
        CAUtils.CreatePenalizedModel(sb, columnNameA, corrFunctionMethods);
        CAUtils.PlotPenalizedCA(sb,columnNameA, 
                 corrFunctionMethods, corColorDotsOpts, corColorLineOpts,
               corPlotTitle, corPlotXlab, corPlotYlab,
                sb.getNewImage("corr_penalized"), "png", 72);
    }
    
    public void penalizedUpdate2_action() {
        System.out.println(corrFunctionMethods);
        CAUtils.CreatePenalizedModel(sb, columnNameA, corrFunctionMethods);
        CAUtils.PlotPenalizedCVCA(sb, columnNameA,
                corrFunctionMethods, corColorDotsOpts, corColorLineOpts,
               corPlotTitle, corPlotXlab, corPlotYlab,
                sb.getNewImage("corr_penalized2"), "png", 72);
    }


//    INPUTS // 
    // PENALIZED
    private String corrFunctionMethods = "ridge";
    
    public String getCorrFunctionMethods() {
        return corrFunctionMethods;
    }

    public void setCorrFunctionMethods(String corrFunctionMethods) {
        this.corrFunctionMethods = corrFunctionMethods;
    }
    
        // ACTION BUTTONS //
    public void corrPenal1Btn_action() {
        System.out.println("Inside poly");

        System.out.println("Done poly");
    }
    // ACTION BUTTONS //
    public void corrPenalPredBtn_action() {
        System.out.println("Inside poly");
        CAUtils.CreatePolynomialModel(sb, columnNameA, columnNameB, doOriginal);

        System.out.println("Done poly");
    }    
}
