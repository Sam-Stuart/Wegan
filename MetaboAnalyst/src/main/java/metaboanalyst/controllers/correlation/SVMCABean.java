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
@ManagedBean(name = "svmCABean")
public class SVMCABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    

//    private String filePolySummary = getSummaryPolyDownload();
    private String fileSVMSummary = "svm_regession_summary.txt";
    private String fileSVMSummaryPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileSVMSummary + "\">" + fileSVMSummary + "</a>";
    
    public String getFileSVMSummaryPath() {
        return fileSVMSummaryPath;
    }

    public void setFileSVMSummaryPath(String fileSVMSummaryPath) {
        this.fileSVMSummaryPath = fileSVMSummaryPath;
    }
    
    
    private String indInput = "";

    public String getIndInput() {
        return indInput;
    }

    public void setIndInput(String indInput) {
        this.indInput = indInput;
    }
    
    private SelectItem[] columnOpts = null;
    
    public SelectItem[] getColumnOpts(){
        String[] columns = CAUtils.GetPolynomialColumns(sb);
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
        
   // CHECK BOX for using normalized data (default) or original data
    private boolean doOriginal = false;

    public boolean isdoOriginal() {
        return doOriginal;
    }

    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
  // CHECK BOX 
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
    
    
    
//    private List<String> corrPolyResults = null;
//    
//    public List<String> getCorrPolyResults(){
//        String[] results = CAUtils.GetPolyCAResults(sb, 2);
//        corrPolyResults = Arrays.asList(results);
//        
//        return corrPolyResults;
//    }
    
        // ACTION BUTTONS //
    public void corrSVMBtn_action() {
        CAUtils.CreateSVMModel(sb, columnNameA, indInput);
        CAUtils.PlotSVMCA(sb, sb.getNewImage("corr_svm"), "png", 72);
    }
//    // ACTION BUTTONS //
//    public void corrPolyPredBtn_action() {
//        System.out.println("Inside poly");
//        CAUtils.CreatePolynomialModel(sb, columnNameA, columnNameB);
//        CAUtils.PlotPolynomialCA(sb, polyDegree, sb.getCurrentImage("corr_poly"), "png", 72);
//        CAUtils.PlotPolynomialPredictCA(sb, 2, sb.getCurrentImage("corr_poly_pred"), "png", 72);
//        System.out.println("Done poly");
//    }    
}
