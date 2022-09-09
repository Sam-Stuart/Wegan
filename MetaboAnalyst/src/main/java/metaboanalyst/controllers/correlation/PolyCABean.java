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
// added:
import javax.faces.context.FacesContext; 
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


@ManagedBean(name = "polyCABean")
public class PolyCABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private boolean addWeights = false;
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    

    private String filePolySummary = getSummaryPolyDownload();
//    private String filePolySummary = "polynomial_regession_summary_degree.txt";
    private String filePolySummaryPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filePolySummary + "\">" + filePolySummary + "</a>";
    
    public String getFilePolySummaryPath() {
        return filePolySummaryPath;
    }

    public void setFilePolySummaryPath(String filePolySummaryPath) {
        this.filePolySummaryPath = filePolySummaryPath;
    }
    
    private String getSummaryPolyDownload(){
        String degree = getPolyDegree(); 
        String facA = getColumnNameA();
        String facB = getColumnNameB();
        return "polynomial_regession_summary_degree_" + degree + "_" + facA + "~" + facB + ".txt";
    }

    
    
    public boolean isaddWeights() {
        return addWeights;
    }

    public void setAddWeights(boolean addWeights) {
        this.addWeights = addWeights;
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
  // CHECK BOX for adding (default) or omitting equation to plot (at top), see correlation_linear.R 
  // when >1 of rsq, eq, & rsqadj are checked, values are seperated by " | " 
     private boolean doPlotEq = false;

    public boolean isdoPlotEq() {
        return doPlotEq;
    }

    public void setdoPlotEq(boolean doPlotEq) {
        this.doPlotEq = doPlotEq;
    } 
  // CHECK BOX for adding (default) or omitting rsq to plot (at top), see correlation_linear.R 
  // when >1 of rsq, eq, & rsqadj are checked, values are seperated by " | " 
     private boolean doPlotRsq = false;

    public boolean isdoPlotRsq() {
        return doPlotRsq;
    }

    public void setdoPlotRsq(boolean doPlotRsq) {
        this.doPlotRsq = doPlotRsq;
    }     
  // CHECK BOX for omitting (default) or adding rsq-adj to plot (at top), see correlation_linear.R 
  // when >1 of rsq, eq, & rsqadj are checked, values are seperated by " | " 
     private boolean doPlotRsqAdj = false;

    public boolean isdoPlotRsqAdj() {
        return doPlotRsqAdj;
    }

    public void setdoPlotRsqAdj(boolean doPlotRsqAdj) {
        this.doPlotRsqAdj = doPlotRsqAdj;
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
    
    
    private SelectItem[] degreeOpts = null;
    
    public SelectItem[] getDegreeOpts(){
        String[] columns = CAUtils.GetPolyDegrees(sb);
        int columnsLen = columns.length;
        degreeOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            degreeOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        //List<String> columnNames = Arrays.asList(columns);
        return degreeOpts;
    }
    
    private String polyDegree = getDegreeOpts()[0].getLabel();
    
    public String getPolyDegree() {
        return polyDegree;
    }

    public void setPolyDegree(String polyDegree) {
        this.polyDegree = polyDegree;
    }
    
    private List<String> corrPolyResults = null;
    
    
    public List<String> getCorrPolyResults(){
        String[] results = CAUtils.GetPolyCAResults(sb, 2);
        corrPolyResults = Arrays.asList(results);
        
        return corrPolyResults;
    }
    
        // ACTION BUTTONS //
    public void corrPoly1Btn_action() {
        CAUtils.CreatePolynomialModel(sb, columnNameA, columnNameB, doOriginal);
        
        CAUtils.PlotPolynomialCA(sb, polyDegree, doOriginal,
                corColorDotsOpts, corColorLineOpts, doPlotConfInt,
                doPlotEq, doPlotRsq, doPlotRsqAdj,
               corPlotTitle, corPlotXlab, corPlotYlab,
                 sb.getNewImage("corr_poly"), "png", 72);
    }
    // ACTION BUTTONS //
    public void corrPolyPredBtn_action() {
        CAUtils.CreatePolynomialModel(sb, columnNameA, columnNameB, doOriginal);
        CAUtils.PlotPolynomialCA(sb, polyDegree, doOriginal,
                corColorDotsOpts, corColorLineOpts, doPlotConfInt,
                doPlotEq, doPlotRsq, doPlotRsqAdj,
               corPlotTitle, corPlotXlab, corPlotYlab,
                sb.getNewImage("corr_poly"), "png", 72);
        CAUtils.PlotPolynomialPredictCA(sb, 
                polyDegree, doOriginal,
                corColorDotsOpts, corColorLineOpts, doPlotConfInt,
               corPlotTitle, corPlotXlab, corPlotYlab,
                sb.getNewImage("corr_poly_pred"), "png", 72);
    }  
    
    
}
