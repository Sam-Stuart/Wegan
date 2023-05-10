/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.taxonomy;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.ChemoMetrics;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.TaxonomyUtils;

import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author Louisa Normington
 */
@ManagedBean(name = "taxBean")
public class TAXBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public void performDefaultAnalysis(String pageID) {
        if (!sb.isAnalInit(pageID)) {
            if (!FacesContext.getCurrentInstance().isPostback()) {
                //sb.registerPage(pageID);
                switch (pageID) {
                    case "WGCNA":
                        doDefaultWGCNA();
                        break;
                   
                }
            }

        }
    }
//        OAUtils.PlotNMDS2DOrdination(sb, false, false, false, false, false, "NULL", "NULL", sb.getCurrentImage("ord_nmds_2D"), "png", 72);
//        OAUtils.PlotNMDS3DOrdination(sb, "NULL", false, "NULL", sb.getCurrentImage("ord_nmds_3D"));
//        OAUtils.PlotNMDSstressOrdination(sb, "NULL", sb.getCurrentImage("ord_nmds_stress"), "png", 72);
//        OAUtils.PlotNMDSscreeOrdination(sb, "NULL", sb.getCurrentImage("ord_nmds_scree"), "png", 72);
    private void doDefaultWGCNA() {
        TaxonomyUtils.MakeWGCNAData(sb, false, 1);
        TaxonomyUtils.PlotWGCNAGeneNetwork(sb, false, 6, sb.getCurrentImage("tax_wgcna_net"), "png", 72, "average", 400);
    }
   
}
