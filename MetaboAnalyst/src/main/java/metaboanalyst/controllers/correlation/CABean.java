/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.correlation;

import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.CAUtils;

import metaboanalyst.utils.DataUtils;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "caBean")
public class CABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        if (!sb.isAnalInit(pageID)) {
            //sb.registerPage(pageID);
            switch (pageID) {
                case "Correlations":
                    doDefaultCorrelation();
                    break; 
                case "Linear":
                    doDefaultLinear();
                    break; 
            }
            
        }
    }


    private void doDefaultCorrelation() {
        System.out.println("HELLO RIGHT BEFORE PLOT Corr");
        UniVarTests.PlotCorrHeatMap(sb, sb.getCurrentImage("corr"), "png", 72, "col", "pearson", "bwm", "overview", "F", "F", "F", 100);
    }
    
    private void doDefaultLinear() {
        System.out.println("HELLO RIGHT BEFORE LINEAR");
        CAUtils.PlotLinearCA(sb, sb.getCurrentImage("corr_linear"), "png", 72);
        CAUtils.PlotLinearTableCA(sb);
    }
    
}
