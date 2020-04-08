/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.functions;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.CmpdBean;
import metaboanalyst.models.NameMapBean;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.REnrichUtils;
import metaboanalyst.rwrappers.SearchUtils;
import metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "mapBean")
@ViewScoped
public class MappingBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private NameMapBean[] nameMaps;

    public NameMapBean[] getNameMapResult() {
        if (nameMaps == null) {
            setupNameMaps();
        }
        return nameMaps;
    }

    private String downloadMsg = "";

    public String getDownloadMsg() {
        return downloadMsg;
    }

    public void setDownloadMsg(String downloadMsg) {
        this.downloadMsg = downloadMsg;
    }

    public void setupNameMaps() {
        RConnection RC = sb.getRConnection();
        // int res = SearchUtils.performApproxSearch(RC, current_hitInx)
        String[] mapRes = SearchUtils.getNameMapTable(RC);
        int row_count = SearchUtils.getNameMapRowNumber(RC);

        nameMaps = new NameMapBean[row_count];

        NameMapBean nameMap;
        for (int i = 0; i < row_count; i++) { //R is column major, walk column by column
            nameMap = new NameMapBean();

            //walk through in order
            int count = 0;
            nameMap.setQuery(mapRes[i + count * row_count]);

            count++;
            nameMap.setHit(mapRes[i + count * row_count]);

            count++;
            nameMap.setHmdb_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setPubchem_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setKegg_id(mapRes[i + count * row_count]);

            count++;
            nameMap.setDetails(mapRes[i + count * row_count]);
            nameMaps[i] = nameMap;
        }
        downloadMsg = "<b>You can download the result </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + "/name_map.csv\"><b>" + "here" + "</b></a>";
    }

    private NameMapBean[] candidateMaps;
    private String preTarget = "";

    public NameMapBean[] getCandidateMaps() {
        if (!target.equals(preTarget)) {
            RConnection RC = sb.getRConnection();
            SearchUtils.performDetailSearch(RC, target);
            String[] canList = SearchUtils.getCandidateList(RC);
            int row_count = SearchUtils.getCanListRowNumber(RC);
            candidateMaps = new NameMapBean[row_count];

            for (int i = 0; i < row_count; i++) { //R is column major, walk column by column
                NameMapBean nameMap = new NameMapBean();
                //walk through in order
                nameMap.setQuery(target);
                int count = 0;
                nameMap.setHit(canList[i + count * row_count]);
                count++;
                nameMap.setHmdb_id(canList[i + count * row_count]);
                count++;
                nameMap.setPubchem_id(canList[i + count * row_count]);
                count++;
                nameMap.setKegg_id(canList[i + count * row_count]);
                candidateMaps[i] = nameMap;
            }
            preTarget = target;
        }
        return candidateMaps;
    }

    private String selectedHit;

    public String getSelectedHit() {
        return selectedHit;
    }

    public void setSelectedHit(String selectedHit) {
        this.selectedHit = selectedHit;
    }

    private String target = "";

    public String getTarget() {
        return target;
    }

    //note this cmpd contain html tags, need to parse out
    public void setCurrentCmpd(String cmpd) {
        target = DataUtils.getStringHTMLTag(cmpd);
    }

    public void selectCandidate() {
        for (NameMapBean candidateMap : candidateMaps) {
            if (candidateMap.isSelected() && !(candidateMap.getHit().equals("None of the above") || candidateMap.getHit().equals(""))) {
                int current_hitInx = SearchUtils.setCandidate(sb.getRConnection(), target, candidateMap.getHit());
                nameMaps[current_hitInx - 1] = candidateMap;
                return;
            }
        }
    }

    public String nextBn_action() {
        //only keep those real match and manually specified match
        //RDataUtils.removeUnmatchedCmpds(RC); 
        if (sb.getAnalType().equals("msetssp")) {
            return "msetssp";
        } else if (sb.getAnalType().equals("msetqea")) {
            return "Data check";
        } else if (sb.getAnalType().equals("msetora")) {
            return "enrichparam";
        } else if (sb.getAnalType().equals("pathora")) {
            return "pathparam";
        } else if (sb.getAnalType().equals("pathqea")) {
            return "Data check";
        } else {
            return null;
        }
    }

    private CmpdBean[] cmpdBeans = null;

    public CmpdBean[] getCmpdBeans() {
        return cmpdBeans;
    }

    private boolean sspInit = false;

    public void initSampleProfiling() {
        if (!sspInit) {
            RConnection RC = sb.getRConnection();
            REnrichUtils.doSSPTest(RC);
            String[] nms = REnrichUtils.getSSPNames(RC);
            String[] concs = REnrichUtils.getSSPConcs(RC);
            String[] hmdbs = REnrichUtils.getSSP_HMDB(RC);
            String[] refConcs = REnrichUtils.getSSPRefConcs(RC);
            String[] states = REnrichUtils.getSSPStates(RC);
            String[] details = REnrichUtils.getSSPdetails(RC);
            cmpdBeans = new CmpdBean[concs.length];

            for (int i = 0; i < concs.length; i++) {
                cmpdBeans[i] = new CmpdBean(nms[i], concs[i], hmdbs[i], refConcs[i], states[i], details[i]);
            }
            sspInit = true;
        }
    }

    public String getSspDetailTxt() {

        RConnection RC = sb.getRConnection();
        String[] concs = RDataUtils.getSSPConcs(RC, target);
        String[] refs = RDataUtils.getSSPReferences(RC, target);
        String[] pmids = RDataUtils.getSSP_Pmids(RC, target);
        String[] notes = RDataUtils.getSSP_Notes(RC, target);

        String str;
        if (concs.length == 1 && concs[0].equalsIgnoreCase("NA")) {
            str = "<font face=\"Arial, Helvetica, Geneva\" size =\"2\">Concentration information is unavailable</font>";
        } else {
            str = "<table width=\"550\" border=\"1\" cellpadding=\"2\" align=\"left\">";
            str = str + "<tr><th width=\"15%\">Study</th><th width=\"25%\">Concentration</th><th width=\"50%\">Reference</th><th width=\"10%\">Note</th></tr>";
            for (int i = 0; i < concs.length; i++) {
                str = str + "<tr>"
                        + "<td>" + "Study " + (i + 1) + "</td>"
                        + "<td>" + concs[i] + "</td>";
                if (pmids[i].equalsIgnoreCase("N/A")) {
                    str = str + "<td>" + refs[i] + "</td>";
                } else {
                    str = str + "<td>" + refs[i] + " (<a href = \"http://www.ncbi.nlm.nih.gov/pubmed/" + pmids[i] + "\">" + "Pubmed" + "</a>) " + "</td>";
                }

                if (notes[i].equalsIgnoreCase("NA")) {
                    str = str + "<td>" + " " + "</td>";
                } else {
                    str = str + "<td>" + notes[i] + "</td>";
                }
                str = str + "</tr>";
            }
            str = str + "</table>";
        }
        return str;
    }

    public String getSspImg() {
        String sspImgName = REnrichUtils.plotCmpdConcRange(sb.getRConnection(), target, "png", 72);
        String url = ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sspImgName;
        return url;
    }

    public String[] getSelectedCmpdList() {
        ArrayList<String> selectedCmpd = new ArrayList();
        for (CmpdBean cb : cmpdBeans) {
            if (cb.isInclude()) {
                selectedCmpd.add(cb.getNameOnly());
            }
        }
        return selectedCmpd.toArray(new String[0]);
    }

    public String sspNextBn_action() {
        String[] selectedCmpds = getSelectedCmpdList();
        if (selectedCmpds.length > 0) {
            RDataUtils.setMapData(sb.getRConnection(), selectedCmpds);
            return "enrichparam";
        } else {
            sb.updateMsg("Error", "No compound was selected for enrichment analysis!");
            return null;
        }
    }
}
