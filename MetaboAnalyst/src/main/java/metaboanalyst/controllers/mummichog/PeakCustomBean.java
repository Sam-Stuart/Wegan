/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.mummichog;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.REnrichUtils;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.event.TransferEvent;
import org.primefaces.model.DualListModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jasmine
 */
@ManagedBean(name = "peakcBean")
@SessionScoped
public class PeakCustomBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    public String customButton_action() {
        String nextpage = prepareDataEditor();
        return nextpage;
    }

    private String adductList;

    public String getAdductList() {

        if (adductList == null) {
            loadAdductList();
        }
        return adductList;
    }

    private void loadAdductList() {
        adductList = DataUtils.readTextFile(ab.getPosAdductPath());
    }

    public void setAdductList(String geneList) {
        this.adductList = adductList;
    }

    private String currList;

    public String getCurrList() {

        if (currList == null) {
            loadCurrList();
        }
        return currList;
    }

    private void loadCurrList() {
        currList = DataUtils.readTextFile(ab.getCurrPath());
    }

    public void setCurrList(String currList) {
        this.currList = currList;
    }

    private boolean loggedIn = false;
    private boolean currUploaded = false;

    private boolean adductUploaded = false;

    public void handleAdductListUpload() {

        adductList = adductList.trim();
        adductUploaded = false;
        RConnection RC = sb.getRConnection();
        int res = REnrichUtils.readAdductData(RC, adductList);
        String msg = RDataUtils.getCurrentMsg(RC);

        if (res == 1) {
            sb.updateMsg("OK", msg);
            adductUploaded = true;
        } else {
            sb.updateMsg("Error", msg);
        }
    }

    public String integrityCheckMum() {

        if (!(adductList == null | adductList.trim().length() == 0)) {
            handleAdductListUpload();
        }

        if (!adductUploaded && !currUploaded) {
            sb.updateMsg("Error", "Please ensure that the currency and adduct lists are not empty!");
            return null;
        }

        sb.setDataUploaded(true);
        return "mzlibview";

    }

    private DualListModel<String> currItems, adductItems;

    private String adductMode = "positive";

    public String getAdductMode() {
        return adductMode;
    }

    public void setAdductMode(String adductMode) {
        this.adductMode = adductMode;
    }

    public void setCurrItems(DualListModel<String> currItems) {
        this.currItems = currItems;
    }

    public DualListModel<String> getCurrItems() {
        return currItems;
    }

    public void setAdductItems(DualListModel<String> adductItems) {
        this.adductItems = adductItems;
    }

    public DualListModel<String> getAdductItems() {
        replaceAddOpt();
        return adductItems;
    }

    public String prepareDataEditor() {
        String nextpage = "";
        if (!sb.isAnalInit("Analysis options")) {
            String names = DataUtils.readTextFile(ab.getCurrPath());
            String[] newnames = names.split("\n");
            currItems = new DualListModel(Arrays.asList(newnames), new ArrayList());

            String allAdducts = DataUtils.readTextFile(ab.getPosAdductPath());
            String[] newadducts = allAdducts.split("\n");
            adductItems = new DualListModel(Arrays.asList(newadducts), new ArrayList());
            nextpage = "Mum customization";
        }
        return nextpage;
    }

    public void doTransfer(TransferEvent event) {
        StringBuilder builder = new StringBuilder();
        for (Object item : event.getItems()) {
            builder.append(item.toString()).append("<br />");
        }
    }

    public String editCurrency() {
        String[] currVec = currItems.getTarget().toArray(new String[0]);
        if (currVec.length == 0) {
            currVec = new String[]{""};
        }
        RConnection RC = sb.getRConnection();

        RDataUtils.setCurrMapData(RC, currVec);
        int res = RDataUtils.performCurrencyMapping(RC);
        String msg = REnrichUtils.getCurrencyMsg(RC);

        if (res > 0) {
            sb.updateMsg("OK", msg);
            return "mzlibview";
        } else {
            sb.updateMsg("Error", msg);
            return null;
        }
    }
    
    public String editAdducts() {
        String[] addVec = adductItems.getTarget().toArray(new String[0]);
        if (addVec.length == 0) {
            addVec = new String[]{""};
        }
        RConnection RC = sb.getRConnection();

        RDataUtils.setAddMapData(RC, addVec);
        int res = RDataUtils.performAdductMapping(RC, adductMode);
        String msg = REnrichUtils.getAdductMsg(RC);

        if (res > 0) {
            sb.updateMsg("OK", msg);
            return "mzlibview";
        } else {
            sb.updateMsg("Error", msg);
            return null;
        }
    }

    public void replaceAddOpt() {
        
        String dummy = "";

        if (adductMode.equals("positive")) {
         String allAdducts = DataUtils.readTextFile(ab.getPosAdductPath());
            String[] newadducts = allAdducts.split("\n");
            adductItems = new DualListModel(Arrays.asList(newadducts), new ArrayList());
        }else{
            String allAdducts = DataUtils.readTextFile(ab.getNegAdductPath());
            String[] newadducts = allAdducts.split("\n");
            adductItems = new DualListModel(Arrays.asList(newadducts), new ArrayList());
        }
    }

}
