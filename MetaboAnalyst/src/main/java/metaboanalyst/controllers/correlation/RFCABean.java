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
@ManagedBean(name = "rfCABean")
public class RFCABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();

//    private String filePolySummary = getSummaryPolyDownload();
    private String fileRFSummary = "rf_regession_summary.txt";
    private String fileRFSummaryPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileRFSummary + "\">" + fileRFSummary + "</a>";
    
    public String getFileRFSummaryPath() {
        return fileRFSummaryPath;
    }

    public void setFileRFSummaryPath(String fileRFSummaryPath) {
        this.fileRFSummaryPath = fileRFSummaryPath;
    }
    
    private String fileMultiSummary = "multivariate_regression_summary.txt";
    private String fileMultiSummaryPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileMultiSummary + "\">" + fileMultiSummary + "</a>";
    
    public String getFileMultiSummaryPath() {
        return fileMultiSummaryPath;
    }

    public void setFileMultiSummaryPath(String fileMultiSummaryPath) {
        this.fileMultiSummaryPath = fileMultiSummaryPath;
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
    
    // CHECK BOX 
     private boolean doLabelPlot = false;

    public boolean isdoLabelPlot() {
        return doLabelPlot;
    }

    public void setdoLabelPlot(boolean doLabelPlot) {
        this.doLabelPlot = doLabelPlot;
    }
    
 //STATIC DROPDOWN for selecting colour:   replaced: corColorOpts : change name from logPaletteOpts to corPaletteOpts, and logColorPaletteOpts to corColorPaletteOpts
    private String corPaletteOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.corColorPaletteOpts

    public String getCorPaletteOpts() {
        return corPaletteOpts; 
    }

    public void setCorPaletteOpts(String corPaletteOpts) {
        this.corPaletteOpts = corPaletteOpts;
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
    
    public void corrRFBtn_action() {
        CAUtils.CreateRFModel(sb, columnNameA, indInput, doOriginal);
        CAUtils.PlotRFCA(sb, columnNameA, indInput, doOriginal,
                  corColorDotsOpts, corColorLineOpts, doPlotConfInt,
               corPlotTitle, corPlotXlab, corPlotYlab,
                sb.getNewImage("corr_rf_pred"), "png", 72);
    }
    
    public void corrRFBtn2_action() {
        CAUtils.CreateRFModel(sb, columnNameA, indInput, doOriginal);
        CAUtils.PlotRFRelativeCA(sb, columnNameA, indInput, doOriginal,
                  corPaletteOpts, doLabelPlot,
               corPlotTitle, corPlotXlab, corPlotYlab,
                sb.getNewImage("corr_rf_relaimpo"), "png", 72);
    }
    
    
 
      public void corrMultiBtn_action() {
        CAUtils.CreateMultivariateModel(sb, columnNameA, indInput, doOriginal);
//         String facA, String predtext, Boolean data,
//             String col_dots, String col_line, Boolean plot_ci, 
//                String plot_title, String plot_xlab, String plot_ylab,
        CAUtils.PlotMultivariateCA(sb,
                columnNameA, indInput, doOriginal,
                corColorDotsOpts, corColorLineOpts, doPlotConfInt,
               corPlotTitle, corPlotXlab, corPlotYlab,
                sb.getNewImage("corr_multi_pred"), "png", 72);
    }
      
         public void corrMultiRelBtn_action() {
        CAUtils.CreateMultivariateModel(sb, columnNameA, indInput, doOriginal);
        CAUtils.PlotMultivariateRelativeCA(sb, 
                columnNameA, indInput, doOriginal,
              corPaletteOpts, doLabelPlot,
               corPlotTitle, corPlotXlab, corPlotYlab,
                sb.getNewImage("corr_multi_relaimpo"), "png", 72);
    }
   
//         corColorDotsOpts, corColorLineOpts, doPlotConfInt,
//                doPlotEq, doPlotRsq, doPlotRsqAdj,
//               corPlotTitle, corPlotXlab, corPlotYlab,
         
//    // ACTION BUTTONS //
//    public void corrPolyPredBtn_action() {
//        System.out.println("Inside poly");
//        CAUtils.CreatePolynomialModel(sb, columnNameA, columnNameB);
//        CAUtils.PlotPolynomialCA(sb, polyDegree, sb.getCurrentImage("corr_poly"), "png", 72);
//        CAUtils.PlotPolynomialPredictCA(sb, 2, sb.getCurrentImage("corr_poly_pred"), "png", 72);
//        System.out.println("Done poly");
//    }    
}
