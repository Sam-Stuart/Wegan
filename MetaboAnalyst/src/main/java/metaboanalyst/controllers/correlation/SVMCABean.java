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
@ManagedBean(name = "svmCABean")
public class SVMCABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

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
