/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.ordination;

import java.io.File;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.DownloadBean;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.OAUtils;

import metaboanalyst.utils.DataUtils;


@ManagedBean(name = "rdaBean")
public class OARDABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();
    
//    private String fileNMA = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + "/rda_scree_data.csv>";
    private String fileName = "rda_scree_data.csv";
    private String fileNMA = fileNMA = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileName + "\">" + fileName + "</a>";
    
    public String getFileNMA() {
        return fileNMA;
    }

    public void setFileNMA(String fileNMA) {
        this.fileNMA = fileNMA;
    }    
    
    

    
    
    
// ACTION BUTTONS //
    public void ciaUpdate_action() {
//        OAUtils.CreateCIAOrdination(sb, coiaTypeOpts, envInput, doOriginal); 
//        OAUtils.PlotCIAscatterOrdination(sb, doMetaGroup, ciaMetaColumnName, ordColorOpts, sb.getCurrentImage("ord_cia_scatter"), "png", 72); //ordMetaColnameOpts is a dynamic dropdown
//        OAUtils.PlotCIAloadingOrdination(sb, coiaDataSetOpts, sb.getCurrentImage("ord_cia_loading"), "png", 72);
//        OAUtils.PlotCIAscreeOrdination(sb, sb.getCurrentImage("ord_cia_scree"), "png", 72);    
    }
    
    
}



