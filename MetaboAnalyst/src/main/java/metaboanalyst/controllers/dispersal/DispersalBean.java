/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.dispersal;

import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.ChemoMetrics;
import metaboanalyst.rwrappers.Classifying;
import metaboanalyst.rwrappers.Clustering;
import metaboanalyst.rwrappers.Dispersal;
import metaboanalyst.rwrappers.Ordiantion;
import metaboanalyst.rwrappers.RCenter;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.SigVarSelect;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.utils.DataUtils;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import org.primefaces.context.RequestContext;
/**
 *
 * @author leif
 */
@ManagedBean(name = "dispersalBean")
public class DispersalBean implements Serializable {
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        System.out.print(" TESTING HERE  at performDefaultAnalysis");
        System.out.print(sb);
        if (!sb.isAnalInit(pageID)) {
            //sb.registerPage(pageID);
            switch (pageID) {
                case "bgd":
                    doDefaultBGD();
                    break;
                case "bsmooth":
                    doDefaultBsmooth();
                    break;
            }
        }
    }

    private void doDefaultBGD() {       
        
        Dispersal.InitBGD(sb);
//        Dispersal.PlotBGD(sb, sb.getNewImage("bgd1"), "png", 72, dispersalBgdNum);
        
    }
    
    private void doDefaultBsmooth() {       
        
        Dispersal.InitBsmooth(sb);
        
    }
    
    private int dispersalBgdNum = 5;

    public int getDispersalBgdNum() {
        return dispersalBgdNum;
    }

    public void setDispersalBgdNum(int dispersalBgdNum) {
        this.dispersalBgdNum = dispersalBgdNum;
    } 
//    public String dispersalBgdBtn_action() {
//        Dispersal.PlotBGD(sb, sb.getNewImage("bgd"), "png", 72, dispersalBgdNum);
//        RequestContext.getCurrentInstance().scrollTo("ac:form2:screePane");
//        return null;
//    }
//    
}
    
