/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.correlation;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.CAUtils;
import metaboanalyst.rwrappers.RDataUtils;

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
// commented out weights 202206-29
//    private boolean addWeights = false;
//    
//    public boolean isaddWeights() {
//        return addWeights;
//    }
//
//    public void setAddWeights(boolean addWeights) {
//        this.addWeights = addWeights;
//    }

    
    private SelectItem[] corrColumnOpts = null;
    
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
    
    private String corrColumnNameA = getCorrColumnOpts()[1].getLabel();
    
    public String getCorrColumnNameA() {
        return corrColumnNameA;
    }

    public void setCorrColumnNameA(String corrColumnNameA) {
        this.corrColumnNameA = corrColumnNameA;
    }
    
    private String corrColumnNameB = getCorrColumnOpts()[0].getLabel();
    
    public String getCorrColumnNameB() {
        return corrColumnNameB;
    }

    public void setCorrColumnNameB(String corrColumnNameB) {
        this.corrColumnNameB = corrColumnNameB;
    }    
    
    private List<String> corrLinearResults = null;
    
    public List<String> getCorrLinearResults(){
        String[] results = CAUtils.GetLinearCAResults(sb);
        corrLinearResults = Arrays.asList(results);
        
        return corrLinearResults;
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
     private boolean doPlotEqOmit = false;

    public boolean isdoPlotEqOmit() {
        return doPlotEqOmit;
    }

    public void setdoPlotEqOmit(boolean doPlotEqOmit) {
        this.doPlotEqOmit = doPlotEqOmit;
    } 
  // CHECK BOX for adding (default) or omitting rsq to plot (at top), see correlation_linear.R 
  // when >1 of rsq, eq, & rsqadj are checked, values are seperated by " | " 
     private boolean doPlotRsqOmit = false;

    public boolean isdoPlotRsqOmit() {
        return doPlotRsqOmit;
    }

    public void setdoPlotRsqOmit(boolean doPlotRsqOmit) {
        this.doPlotRsqOmit = doPlotRsqOmit;
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
    
//    // ACTION BUTTONS //
//    public void corrLin1Btn_action() {
//        CAUtils.CreateLinearModel(sb, corrColumnNameA, corrColumnNameB);
//        //CAUtils.CreateLinearModel(sb, "/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/target/MetaboAnalyst-4.34/resources/data/dune_weights.csv");
//        CAUtils.PlotLinearCA(sb, sb.getNewImage("corr_linear"), "png", 72);
//    }
//
//    
//}
    
// ORDER OF ARGUMENTS 202206-29
  
// mSetObj=NA, facA="NULL", facB="NULL", data="false",
//    col_dots="NULL", col_line="NULL", plot_ci="false", # weights=NULL,
// no_plot_eq="false", no_plot_rsq="false",
//plot_rsq_adj="false", imgName # ,format="png", dpi=72, width=NA
    
    // ACTION BUTTONS //
    public void corrLin1Btn_action() {
//        CAUtils.CreateLinearModel(sb, corrColumnNameA, corrColumnNameB);
        //CAUtils.CreateLinearModel(sb, "/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/target/MetaboAnalyst-4.34/resources/data/dune_weights.csv");
        
        CAUtils.PlotLinearCA(sb, corrColumnNameA, corrColumnNameB, 
                doOriginal, corColorDotsOpts, corColorLineOpts, 
               doPlotConfInt, doPlotEqOmit, doPlotRsqOmit,
                doPlotRsqAdj, sb.getCurrentImage("corr_linear"));
    }

    
}
