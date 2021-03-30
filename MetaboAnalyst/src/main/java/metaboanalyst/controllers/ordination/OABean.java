/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.ordination;

import metaboanalyst.controllers.correlation.*;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.OAUtils;

import metaboanalyst.utils.DataUtils;

/**
 *
 * @author Louisa
 */
@ManagedBean(name = "oaBean")
public class OABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        if (!sb.isAnalInit(pageID)) {
            //sb.registerPage(pageID);
            switch (pageID) {
                case "NMDS":
                    doDefaultNMDS();
                    break; 
            }
            
        }
    }
    
    private void doDefaultNMDS() {
        System.out.println("HELLO RIGHT BEFORE NMDS");
        OAUtils.CreateNMDSOA(sb);
        OAUtils.PlotNMDSscreeOA(sb, sb.getCurrentImage("ord_nmds_scree"), "png", 72);
    }
    
}
