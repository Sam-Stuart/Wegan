/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.mnet;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.faces.bean.ManagedBean;
import metaboanalyst.models.NameMapBean;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.RIntegUtils;
import metaboanalyst.rwrappers.RNetworkUtils;
import metaboanalyst.rwrappers.SearchUtils;
import metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "mnetMapBean")
public class MnetMapBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    //for genes 
    private List<NameMapBean> geneNameMaps;

    public List<NameMapBean> getGeneNameMaps() {
        if (geneNameMaps == null) {
            setupGeneNameMaps();
        }
        return geneNameMaps;
    }

    public void setGeneNameMaps(List<NameMapBean> geneNameMaps) {
        this.geneNameMaps = geneNameMaps;
    }

    private String geneDownloadMsg = "";

    public String getGeneDownloadMsg() {
        return geneDownloadMsg;
    }

    public void setGeneDownloadMsg(String geneDownloadMsg) {
        this.geneDownloadMsg = geneDownloadMsg;
    }

    public void setupGeneNameMaps() {
        RConnection RC = sb.getRConnection();
        // int res = SearchUtils.performApproxSearch(RC, current_hitInx)
        String[] mapRes = SearchUtils.getNetworkGeneNameMapTable(RC);

        if (mapRes == null || mapRes.length == 0) {
            return;
        }

        int row_count = SearchUtils.getGeneNameMapRowNumber(RC);

        geneNameMaps = new ArrayList();

        NameMapBean nameMap;
        for (int i = 0; i < row_count; i++) { //R is column major, walk column by column
            nameMap = new NameMapBean();

            //walk through in order
            int count = 0;
            nameMap.setQuery(mapRes[i + count * row_count]);

            count++;
            nameMap.setHit(mapRes[i + count * row_count]);

            count++;
            nameMap.setSymbol(mapRes[i + count * row_count]);
            
            count++;
            nameMap.setKo(mapRes[i + count * row_count]);

            count++;
            nameMap.setName(mapRes[i + count * row_count]);

            count++;
            nameMap.setDetails(mapRes[i + count * row_count]);
            geneNameMaps.add(nameMap);
        }
        geneDownloadMsg = "<b>You can download the result </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + "/gene_name_map.csv\"><b>" + "here" + "</b></a>";
    }

    private String targetGene = "";

    public String getTargetGene() {
        return targetGene;
    }

    private int current_gene_hitInx = 0;

    public void setCurrentGeneRowInx(int inx) {
        targetGene = geneNameMaps.get(inx).getQuery();
        this.current_gene_hitInx = inx + 1; //R start from 1
    }

    //#####################################
    //for compounds
    private List<NameMapBean> cmpdNameMaps;

    public List<NameMapBean> getCmpdNameMapResult() {
        if (cmpdNameMaps == null) {
            setupCmpdNameMaps();
        }
        return cmpdNameMaps;
    }

    private String cmpdDownloadMsg = "";

    public String getCmpdDownloadMsg() {
        return cmpdDownloadMsg;
    }

    public void setCmpdDownloadMsg(String downloadMsg) {
        this.cmpdDownloadMsg = downloadMsg;
    }

    public void setupCmpdNameMaps() {
        RConnection RC = sb.getRConnection();
        String[] mapRes = SearchUtils.getNameMapTable(RC);

        if (mapRes == null || mapRes.length == 0) {
            return;
        }

        int row_count = SearchUtils.getNameMapRowNumber(RC);

        cmpdNameMaps = new ArrayList<NameMapBean>();

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
            cmpdNameMaps.add(nameMap);
        }
        cmpdDownloadMsg = "<b>You can download the result </b> <a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + "/name_map.csv\"><b>" + "here" + "</b></a>";
    }

    private NameMapBean[] cmpdCandidateMaps;
    private String preTarget = "";

    public NameMapBean[] getCmpdCandidateMaps() {
        if (!targetCmpd.equals(preTarget)) {
            RConnection RC = sb.getRConnection();
            SearchUtils.performDetailSearch(RC, targetCmpd);
            String[] canList = SearchUtils.getCandidateList(RC);
            int row_count = SearchUtils.getCanListRowNumber(RC);
            cmpdCandidateMaps = new NameMapBean[row_count];

            for (int i = 0; i < row_count; i++) { //R is column major, walk column by column
                NameMapBean nameMap = new NameMapBean();
                //walk through in order
                nameMap.setQuery(targetCmpd);
                int count = 0;
                nameMap.setHit(canList[i + count * row_count]);
                count++;
                nameMap.setHmdb_id(canList[i + count * row_count]);
                count++;
                nameMap.setPubchem_id(canList[i + count * row_count]);
                count++;
                nameMap.setKegg_id(canList[i + count * row_count]);
                cmpdCandidateMaps[i] = nameMap;
            }
            preTarget = targetCmpd;
        }
        return cmpdCandidateMaps;
    }

    private String selectedHit;

    public String getSelectedHit() {
        return selectedHit;
    }

    public void setSelectedHit(String selectedHit) {
        this.selectedHit = selectedHit;
    }

    private String targetCmpd = "";

    public String getTargetCmpd() {
        return targetCmpd;
    }

    public void setCurrentCmpdRowInx(int inx) {
        targetCmpd = cmpdNameMaps.get(inx).getQuery();
    }

    public void selectCmpdCandidate() {
        String selectedNm = "";
        NameMapBean nmb = null;
        for (int i = 0; i < cmpdCandidateMaps.length; i++) {
            if (cmpdCandidateMaps[i].isSelected()) {
                NameMapBean nmbn = cmpdCandidateMaps[i];
                selectedNm = nmbn.getHit();
                nmb = cmpdCandidateMaps[i];
                break;
            }
        }
        RConnection RC = sb.getRConnection();
        int current_cmpd_hitInx = SearchUtils.setCandidate(RC, targetCmpd, selectedNm);
        int inx = current_cmpd_hitInx - 1;
        cmpdNameMaps.set(inx, nmb);
    }

    public void deleteCmpd(int inx) {
        cmpdNameMaps.remove(inx);
        RIntegUtils.removeCmpd(sb.getRConnection(), inx + 1);
    }

    public void deleteGene(int inx) {
        geneNameMaps.remove(inx);
        RIntegUtils.removeGene(sb.getRConnection(), inx + 1);
    }
    
    public String prepareNetworkData() {
        RConnection RC = sb.getRConnection();
        int res = RNetworkUtils.prepareNetworkData(RC);
        
        sb.setNetworkQuerySubmitted(true);

        if (res == 0) {
            sb.updateMsg("Error", RDataUtils.getCurrentMsg(RC));
            return null;
        } else {
            return "MnetParam";
        }
    }
}
