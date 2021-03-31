/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.ordination;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.RDataUtils;

import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "oaBean")
public class OABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        if (!sb.isAnalInit(pageID)) {
            if (!FacesContext.getCurrentInstance().isPostback()) {
                //sb.registerPage(pageID);
                switch (pageID) {
                    case "NMDS":
                        doDefaultNMDS();
                        break;
                }
            }

        }
    }

    private void doDefaultNMDS() {
        System.out.println("HELLO RIGHT BEFORE NMDS");
        OAUtils.CreateNMDSOA(sb);
        OAUtils.PlotNMDSscreeOA(sb, sb.getCurrentImage("ord_nmds_scree"), "png", 72);
    }
    
}
