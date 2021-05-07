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

    private boolean addWeights = false;
    
    public boolean isaddWeights() {
        return addWeights;
    }

    public void setAddWeights(boolean addWeights) {
        this.addWeights = addWeights;
    }

    
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
    
    // ACTION BUTTONS //
    public void corrLin1Btn_action() {
        CAUtils.CreateLinearModel(sb, corrColumnNameA, corrColumnNameB);
        //CAUtils.CreateLinearModel(sb, "/Users/danaallen/NetBeansProjects/Wegan/MetaboAnalyst/target/MetaboAnalyst-4.34/resources/data/dune_weights.csv");
        CAUtils.PlotLinearCA(sb, sb.getNewImage("corr_linear"), "png", 72);
        RequestContext.getCurrentInstance().scrollTo("linCA:form1:corrLinPane1");
    }

    
}
