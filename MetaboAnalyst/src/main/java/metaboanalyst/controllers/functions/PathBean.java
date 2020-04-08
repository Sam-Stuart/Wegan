/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.functions;

import java.io.Serializable;
import java.util.ArrayList;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.MetSetBean;
import metaboanalyst.models.PABean;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.REnrichUtils;
import metaboanalyst.rwrappers.RGraphUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "pathBean")
@SessionScoped
public class PathBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private String libOpt = "hsa";

    public String getLibOpt() {
        return libOpt;
    }

    public void setLibOpt(String libOpt) {
        this.libOpt = libOpt;
    }

    private String refLibOpt = "all";

    public String getRefLibOpt() {
        return refLibOpt;
    }

    public void setRefLibOpt(String refLibOpt) {
        this.refLibOpt = refLibOpt;
    }

    public String libSubmitBn_action() {
        return "View result";
    }

    private UploadedFile refLibFile;

    public UploadedFile getRefLibFile() {
        return refLibFile;
    }

    public void setRefLibFile(UploadedFile refLibFile) {
        this.refLibFile = refLibFile;
    }

    private String topoCode = "rbc";

    public String getTopoCode() {
        return topoCode;
    }

    public void setTopoCode(String topoCode) {
        this.topoCode = topoCode;
    }

    private String oraStatCode = "hyperg";

    public String getOraStatCode() {
        return oraStatCode;
    }

    public void setOraStatCode(String oraStatCode) {
        this.oraStatCode = oraStatCode;
    }

    private String qeaStatCode = "gt";

    public String getQeaStatCode() {
        return qeaStatCode;
    }

    public void setQeaStatCode(String qeaStatCode) {
        this.qeaStatCode = qeaStatCode;
    }

    private String currentPathName;

    public void setCurrentPathName(String pn) {
        sb.setCurrentPathName(pn);
        this.currentPathName = pn; //R start from 1
    }

    public MetSetBean[] getCurrentPathSet() {
        String[] details = REnrichUtils.getHTMLPathSet(sb.getRConnection(),currentPathName);
        ArrayList<MetSetBean> libVec = new ArrayList();
        libVec.add(new MetSetBean(details[0], details[1], ""));
        return libVec.toArray(new MetSetBean[0]);
    }

    public void handleRefLibUpload() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        try {

            RConnection RC = sb.getRConnection();
            //check if data is uploaded
            if (refLibFile.getSize() == 0) {
                sb.updateMsg("Error","File is empty");
                return;
            }
            String fileName = DataUtils.uploadFile(refLibFile, sb, null, false);
            boolean res = RDataUtils.readKEGGRefLibData(RC, fileName);
            if (res) {
                sb.updateMsg("OK", RDataUtils.getRefLibCheckMsg(RC));
                refLibOpt = "self";
            } else {
                sb.updateMsg("Error:", RDataUtils.getErrMsg(RC));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public String paBn_action() {

        RConnection RC = sb.getRConnection();
        if (RDataUtils.setPathLib(RC, libOpt)) {
            
            String nextpage;
            if(libOpt.contains("smpdb")){
                String [] arrOfStr = libOpt.split("-smpdb", 0);
                libOpt = arrOfStr[0];
                RDataUtils.setOrganism(sb.getRConnection(), libOpt);
                nextpage = "smpdbpathview";
            }
            else {
                nextpage = "pathview";
            }

            if (refLibOpt.equals("all")) {
                RDataUtils.setMetabolomeFilter(sb.getRConnection(), false);
            } else {
                RDataUtils.setMetabolomeFilter(sb.getRConnection(), true);
            }

            String imgName = sb.getNewImage("path_view");
            if (sb.getAnalType().equalsIgnoreCase("pathqea")) {

                if (REnrichUtils.doPathQeaTest(sb, topoCode, qeaStatCode)) {
                    RGraphUtils.plotPathSummary(sb, imgName, "png", 72);
                    String[] pathnames = REnrichUtils.getQEApathNames(RC);
                    String[] keggLnks = REnrichUtils.getQEAKeggIDs(RC);
                    String[] smpdbLnks = null;
                    if (libOpt.equals("hsa")||libOpt.equals("ath")||libOpt.equals("eco")||libOpt.equals("mmu")||libOpt.equals("sce")) {
                        smpdbLnks = REnrichUtils.getQEASMPDBIDs(RC);
                    }

                    if (pathnames.length == 1 && pathnames[0].equals("NA")) {
                        return null;
                    }

                    double[][] mat = REnrichUtils.getQEAMat(RC);
                    paBeans = new PABean[pathnames.length];
                    for (int i = 0; i < pathnames.length; i++) {
                        paBeans[i] = new PABean(pathnames[i], keggLnks[i], smpdbLnks == null ? "" : smpdbLnks[i], (int) mat[i][0], 0, (int) mat[i][1],
                                mat[i][2], mat[i][3], mat[i][4], mat[i][5], mat[i][6]);
                    }
                }
            } else if (REnrichUtils.doPathOraTest(sb, topoCode, oraStatCode)) {
                RGraphUtils.plotPathSummary(sb, imgName, "png", 72);
                String[] rownames = REnrichUtils.getORApathNames(RC);
                String[] keggLnks = REnrichUtils.getORAKeggIDs(RC);
                String[] smpdbLnks = null;
                if (libOpt.equals("hsa")||libOpt.equals("ath")||libOpt.equals("eco")||libOpt.equals("mmu")||libOpt.equals("sce")) {
                    smpdbLnks = REnrichUtils.getORASMPDBIDs(RC);
                }
                double[][] mat = REnrichUtils.getORAMat(RC);
                paBeans = new PABean[rownames.length];
                for (int i = 0; i < rownames.length; i++) {
                    paBeans[i] = new PABean(rownames[i], keggLnks[i], smpdbLnks == null ? "" : smpdbLnks[i],
                            (int) mat[i][0], mat[i][1], (int) mat[i][2], mat[i][3], mat[i][4], mat[i][5], mat[i][6], mat[i][7]);
                }
            }
            return nextpage;
        } else {
            String msg = RDataUtils.getErrMsg(RC);
            sb.updateMsg("Error", "There is something wrong with the pathway enrichment analysis: " + msg);
            return null;
        }
    }

    private PABean[] paBeans;

    public PABean[] getPaBeans() {
        return paBeans;
    }

    public void setPaBeans(PABean[] pabs) {
        paBeans = pabs;
    }

    private String downloadMsg = "";

    public String getDownloadMsg() {
        return downloadMsg;
    }

    public void setDownloadMsg(String downloadMsg) {
        this.downloadMsg = downloadMsg;
    }

    public String nextBn_action() {
        return "Download";
    }
    
    public String viewNetwork(String param){
        RConnection RC = sb.getRConnection();
        String res = RDataUtils.genPathwayJSON(RC, param);
        return res;
    }
}
