/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.diversity;

import metaboanalyst.controllers.ordination.*;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.DiversityUtils;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.RDataUtils;

import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

@ManagedBean(name = "diversityBean")
public class DiversityBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        if (!sb.isAnalInit(pageID)) {
            if (!FacesContext.getCurrentInstance().isPostback()) {
                switch (pageID) {
                    case "Alpha":
                        doDefaultAlpha();
                        break;
                    case "Beta":
                        doDefaultBeta();
                        break;
                    case "Gamma":
                        doDefaultGamma();
                        break;
                    case "TestTutorial":
                        doDefaultTestTutorial();
                        break;
                }
            }

        }
    }

    
    private void doDefaultAlpha(){
        if (!DiversityUtils.CreateAlpha(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }
        DiversityUtils.PlotAlpha(sb);
    }
    
    private void doDefaultBeta(){
        if (!DiversityUtils.CreateBeta(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }
        DiversityUtils.PlotBeta(sb);
    }
    
    private void doDefaultGamma(){
        if (!DiversityUtils.CreateGamma(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }
        DiversityUtils.PlotGamma(sb);
    }
    private void doDefaultSpecies(){
        if (!DiversityUtils.CreateSpecies(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }
        DiversityUtils.PlotSpecies(sb);
    }
    
    private void doDefaultTestTutorial(){
        if (!DiversityUtils.CreateTestTutorial(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }
        DiversityUtils.CreateTestTutorial(sb);
        DiversityUtils.PlotTestTutorial(sb, sb.getCurrentImage("gamma_plot"), "png", 72);
    }
    
        private void doDefaultRarefaction(){
        if (!DiversityUtils.CreateTestTutorial(sb)){
            RConnection RC = sb.getRConnection();
            sb.updateMsg("Error", RDataUtils.getErrMsg(RC));
        }

        DiversityUtils.CreateRarefactionDiv(sb, false, "test", "test", false, "test");
        DiversityUtils.PlotRarefactionCurveDiversity(sb, "test", "test", "test", sb.getCurrentImage("Rarefaction_curve"), "png", 72, "false");
    }
}
