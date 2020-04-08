/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers;

import metaboanalyst.utils.DataUtils;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import metaboanalyst.rwrappers.RDataUtils;
import org.primefaces.event.TransferEvent;
import org.primefaces.model.DualListModel;

import org.rosuda.REngine.Rserve.RConnection;

@ManagedBean(name = "editBean")
@SessionScoped
public class EditBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private DualListModel<String> sampleItems, featureItems, groupItems;

    public DualListModel<String> getGroupItems() {
        return groupItems;
    }

    public void setGroupItems(DualListModel<String> groupItems) {
        this.groupItems = groupItems;
    }

    public void setSampleItems(DualListModel<String> sampleItems) {
        this.sampleItems = sampleItems;
    }

    public void setFeatureItems(DualListModel<String> featureItems) {
        this.featureItems = featureItems;
    }

    public DualListModel<String> getSampleItems() {
        return sampleItems;
    }

    public DualListModel<String> getFeatureItems() {
        return featureItems;
    }

    public void prepareDataEditor() {
        if (!sb.isAnalInit("Data editor")) {
            RConnection RC = sb.getRConnection();
            String[] names = RDataUtils.getPrenormFeatureNames(RC);
            featureItems = new DualListModel(Arrays.asList(names), new ArrayList());
            String[] allSamples = RDataUtils.getSampleNames(RC);
            sampleItems = new DualListModel(Arrays.asList(allSamples), new ArrayList());
            String[] allGroups = RDataUtils.getGroupNames(RC);
            groupItems = new DualListModel(Arrays.asList(allGroups), new ArrayList());
        }
    }

    public void doTransfer(TransferEvent event) {
        StringBuilder builder = new StringBuilder();
        for (Object item : event.getItems()) {
            builder.append(item.toString()).append("<br />");
        }
    }

    public String editGroups() {
        String[] remVec = groupItems.getSource().toArray(new String[0]);
        if (remVec.length < 2) {
            sb.updateMsg("Error", "At least two groups are required.");
            return null;
        }
        String[] grpVec = groupItems.getTarget().toArray(new String[0]);
        if (grpVec.length == 0) {
            grpVec = new String[]{""};
        }
        RConnection RC = sb.getRConnection();
        int res = RDataUtils.updateGroupItems(RC, grpVec);
        String msg = RDataUtils.getCurrentMsg(RC);
        if (res > 0) {
            sb.updateMsg("OK", msg);
            sb.setMultipleGroup(res > 2);
            return "Normalization";
        } else {
            sb.updateMsg("Error", msg);
            return null;
        }

    }

    public String editSamples() {
        String[] smplVec = sampleItems.getTarget().toArray(new String[0]);
        if (smplVec.length == 0) {
            smplVec = new String[]{""};
        }
        RConnection RC = sb.getRConnection();
        int res = RDataUtils.updateSampleItems(RC, smplVec);
        String msg = RDataUtils.getCurrentMsg(RC);
        if (res > 0) {
            sb.updateMsg("OK", msg);
            sb.setMultipleGroup(res > 2);
            return "Normalization";
        } else {
            sb.updateMsg("Error", msg);
            return null;
        }

    }

    public String editFeatures() {
        String[] featVec = featureItems.getTarget().toArray(new String[0]);
        if (featVec.length == 0) {
            featVec = new String[]{""};
        }
        RConnection RC = sb.getRConnection();
        int res = RDataUtils.updateFeatureItems(RC, featVec);
        String msg = RDataUtils.getCurrentMsg(RC);
        if (res == 1) {
            sb.updateMsg("OK", msg);
            return "Normalization";
        } else {
            sb.updateMsg("Error", msg);
            return null;
        }
    }
}
