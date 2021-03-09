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
 * @author dnallen
 */
@ManagedBean(name = "penalCABean")
public class PenalizedCABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

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
    
    
    private List<String> corrPenalResults = null;
    
    public List<String> getCorrPenalResults(){
        String[] results = CAUtils.GetPenalizedCAResults(sb);
        corrPenalResults = Arrays.asList(results);
        
        return corrPenalResults;
    }
    
// ACTION BUTTONS //
    public void penalizedUpdate_action() {
        System.out.println(corrFunctionMethods);
        CAUtils.CreatePenalizedModel(sb, corrFunctionMethods, columnNameA, addWeights);
        CAUtils.PlotPenalizedCA(sb, sb.getCurrentImage("corr_penalized"), "png", 72);
    }
    
    public void penalizedUpdate2_action() {
        System.out.println(corrFunctionMethods);
        CAUtils.CreatePenalizedModel(sb, corrFunctionMethods, columnNameA, addWeights);
        CAUtils.PlotPenalizedCVCA(sb, sb.getCurrentImage("corr_penalized2"), "png", 72);
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
        CAUtils.CreatePolynomialModel(sb, columnNameA, columnNameB);

        System.out.println("Done poly");
    }    
}
