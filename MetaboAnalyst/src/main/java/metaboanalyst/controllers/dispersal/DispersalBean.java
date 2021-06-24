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
import metaboanalyst.rwrappers.DispersalUtils;
import metaboanalyst.rwrappers.Ordiantion;
import metaboanalyst.rwrappers.RCenter;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.SigVarSelect;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.utils.DataUtils;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import org.primefaces.context.RequestContext;
/**
 *
 * @author leif
 */
@ManagedBean(name = "dispersalBean")
public class DispersalBean implements Serializable {
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        
        if (!sb.isAnalInit(pageID)) {
            if (!FacesContext.getCurrentInstance().isPostback()) {


                //sb.registerPage(pageID);
                switch (pageID) {
                    case "bgd":
                        doDefaultBGD();
                        break;
                    case "beals":
                        doDefaultBeals();
                        break;
                    case "betadisper":
                        doDefaultBetaDisper();
                        break;
                }
            }
        }
    }
// Biogeographical Dispersal functions
    private void doDefaultBGD() {       
        
        
        DispersalUtils.InitBGD(sb);
        DispersalUtils.PlotBGD(sb, sb.getCurrentImage("bgd1"), "png", 72, dispersalBgdNum);
        DispersalUtils.PlotBGD(sb, sb.getCurrentImage("bgd2"), "png", 72, dispersalBgdNum);
        DispersalUtils.PlotBGD(sb, sb.getCurrentImage("bgd3"), "png", 72, dispersalBgdNum);
        DispersalUtils.PlotBGD(sb, sb.getCurrentImage("bgd4"), "png", 72, dispersalBgdNum);
        DispersalUtils.PlotBGD(sb, sb.getCurrentImage("bgd5"), "png", 72, dispersalBgdNum);
        DispersalUtils.PlotBGD(sb, sb.getCurrentImage("bgd6"), "png", 72, dispersalBgdNum);
    }
 //    
    private void doDefaultBeals() {       
        DispersalUtils.LoadDplyr(sb);
        DispersalUtils.CreateBeals(sb, "NA", "data", 0, "TRUE");
        DispersalUtils.PlotBeals(sb, sb.getNewImage("beals"), "png", 72, dispersalBgdNum, "NA");
        
    }
    
    private void doDefaultBetaDisper() {       
        
        DispersalUtils.InitBetaDisper(sb);
        DispersalUtils.PlotBetaDisper(sb, sb.getNewImage("betadisper"), "png", 72, dispersalBgdNum);
        
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

    public String dispersalBealsBtn_action() {
          System.out.print("dispersalBeals button action  -dispersalBean");
//        Dispersal.PlotBealsSummary
//        ChemoMetrics.PlotPCAPairSummary(sb, sb.getNewImage("pca_pair"), "png", 72, dispersalBgdNum);
//        RequestContext.getCurrentInstance().scrollTo("ac:form1:pairPane");
        return null;
    }
    
    
    
}

