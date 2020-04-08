/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.functions;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.MetSetBean;
import metaboanalyst.models.OraBean;
import metaboanalyst.models.QeaBean;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.REnrichUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.DefaultStreamedContent;
import org.primefaces.model.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "msetBean")
@SessionScoped
public class MsetBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private String msetOpt = "pathway";

    public String getMsetOpt() {
        return msetOpt;
    }

    public void setMsetOpt(String msetOpt) {
        this.msetOpt = msetOpt;
    }

    private String libOpt = "all";

    public String getLibOpt() {
        return libOpt;
    }

    public void setLibOpt(String libOpt) {
        this.libOpt = libOpt;
    }

    private UploadedFile msetLibFile;

    public UploadedFile getMsetLibFile() {
        return msetLibFile;
    }

    public void setMsetLibFile(UploadedFile msetLibFile) {
        this.msetLibFile = msetLibFile;
    }

    private String checkMsg = "";

    public String getCheckMsg() {
        return checkMsg;
    }

    public void setCheckMsg(String checkMsg) {
        this.checkMsg = checkMsg;
    }

    public void msetUploadBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        try {
            RConnection RC = sb.getRConnection();

            //check if data is uploaded
            if (msetLibFile.getSize() == 0) {
                sb.updateMsg("Error", "File is empty");
                return;
            }

            if (!msetLibFile.getFileName().endsWith(".csv")) {
                sb.updateMsg("Error","Only comma separated format (*.csv) will be accepted!");
                return;
            }

            // File libFile = new File(homeDir + File.separatorChar + fileName);
            // uploadedFile.write(libFile);
            String fileName = DataUtils.uploadFile(msetLibFile, sb, null, false);
            boolean res = RDataUtils.readMsetLibData(RC, fileName);
            if (res) {
                checkMsg = RDataUtils.getMsetLibCheckMsg(RC);
                sb.updateMsg("OK", checkMsg);
                msetOpt = "self";
            } else {
                sb.updateMsg("Error:", RDataUtils.getErrMsg(RC));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private boolean doMetabolomeFilter = false;

    public boolean isDoMetabolomeFilter() {
        return doMetabolomeFilter;
    }

    public void setDoMetabolomeFilter(boolean doMetabolomeFilter) {
        this.doMetabolomeFilter = doMetabolomeFilter;
    }

    private boolean doMsetFilter = true;

    public boolean isDoMsetFilter() {
        return doMsetFilter;
    }

    public void setDoMsetFilter(boolean doMsetFilter) {
        this.doMsetFilter = doMsetFilter;
    }

    private int minMsetNum = 2;

    public int getMinMsetNum() {
        return minMsetNum;
    }

    public void setMinMsetNum(int minMsetNum) {
        this.minMsetNum = minMsetNum;
    }

    private UploadedFile metabolomeFile;

    public UploadedFile getMetabolomeFile() {
        return metabolomeFile;
    }

    public void setMetabolomeFile(UploadedFile metabolomeFile) {
        this.metabolomeFile = metabolomeFile;
    }

    public void uploadMetabolomeBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        try {

            RConnection RC = sb.getRConnection();
            //check if data is uploaded
            if (metabolomeFile.getSize() == 0) {
                sb.updateMsg("Error","File is empty");
                return;
            }
            String fileName = DataUtils.uploadFile(metabolomeFile, sb, null, false);
            boolean res = RDataUtils.readHMDBRefLibData(RC, fileName);
            if (res) {
                sb.updateMsg("OK", RDataUtils.getRefLibCheckMsg(RC));
                libOpt = "self";
            } else {
                sb.updateMsg("Error:", RDataUtils.getErrMsg(RC));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public String submitBtn_action() {
        RConnection RC = sb.getRConnection();
        int excludeNm = 0;
        if (doMsetFilter) {
            excludeNm = minMsetNum;
        }

        if (libOpt.equals("self")) {
            RDataUtils.setMetabolomeFilter(RC, true);
        } else {
            RDataUtils.setMetabolomeFilter(RC, false);
        }

        //sb.setMsetLibType(msetOpt);
        RDataUtils.setCurrentMsetLib(sb.getRConnection(), msetOpt, excludeNm);
        if (sb.getAnalType().equals("msetqea")) {
            doGlobalTest();
            return "qeaview";
        } else {
            doHyperGeom();
            return "oraview";
        }
    }

    private OraBean[] oraBeans;

    public OraBean[] getOraBeans() {
        return oraBeans;
    }

    public void doHyperGeom() {
        RConnection RC = sb.getRConnection();
        String imgName = sb.getNewImage("ora");
        if (REnrichUtils.hypergeomTest(RC)) {
            REnrichUtils.plotORA(sb, imgOpt, imgName, "png", 72);
            String[] rownames = REnrichUtils.getORARowNames(RC);
            String[] rowStyles = REnrichUtils.getORAColorBar(RC);
            double[][] mat = REnrichUtils.getORAMat(RC);

            oraBeans = new OraBean[rownames.length];
            for (int i = 0; i < rownames.length; i++) {
                oraBeans[i] = new OraBean(rownames[i], "background-color:" + rowStyles[i], (int) mat[i][0], mat[i][1], (int) mat[i][2], mat[i][3], mat[i][4], mat[i][5]);
            }
        }
    }

    private QeaBean[] qeaBeans;

    public QeaBean[] getQeaBeans() {
        return qeaBeans;
    }

    public void doGlobalTest() {
        RConnection RC = sb.getRConnection();
        String imgName = sb.getNewImage("qea");
        if (REnrichUtils.performGlobalTest(RC)) {
            String[] rownames = REnrichUtils.getQEARowNames(RC);

            double[][] mat = REnrichUtils.getQEAMat(RC);
            qeaBeans = new QeaBean[rownames.length];
            String[] rowStyles = REnrichUtils.getQEAColorBar(RC);
            for (int i = 0; i < rownames.length; i++) {
                qeaBeans[i] = new QeaBean(rownames[i], "background-color:" + rowStyles[i], (int) mat[i][0], (int) mat[i][1],
                        mat[i][2], mat[i][3], mat[i][4], mat[i][5], mat[i][6]);
            }
            REnrichUtils.plotQEA(sb, imgOpt, imgName, "png", 72);
        }
    }

    public String getOraImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("ora") + "dpi72.png";
    }

    public String getQeaImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("qea") + "dpi72.png";
    }

    public String nextBn_action() {
        return "Download";
    }
    
    private String msetNm;

    public void setMsetNm(String msetNm) {
        this.msetNm = msetNm;
    }

    public String getMsetImgPath() {
        String imgNm = REnrichUtils.plotQeaMset(sb, msetNm, "png", 72);
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + imgNm;
    }

    public MetSetBean[] getCurrentMsetLib() {
        String[] details = REnrichUtils.getHTMLMetSet(sb.getRConnection(), msetNm);
        ArrayList<MetSetBean> libVec = new ArrayList();
        libVec.add(new MetSetBean(details[0], details[1], details[2]));
        return libVec.toArray(new MetSetBean[0]);
    }

    private String imgOpt = "net";

    public String getImgOpt() {
        return imgOpt;
    }

    public void setImgOpt(String imgOpt) {
        this.imgOpt = imgOpt;
    }

    public void updateEnrichSummary() {
        if (sb.getAnalType().equals("msetqea")) {
            String imgName = sb.getNewImage("qea");
            REnrichUtils.plotQEA(sb, imgOpt, imgName, "png", 72);
        } else {
            String imgName = sb.getNewImage("ora");
            REnrichUtils.plotORA(sb, imgOpt, imgName, "png", 72);
        }
    }

    public DefaultStreamedContent getSifFile() {
        REnrichUtils.prepareSifDownload(sb);
        try {
            File file = new File(sb.getCurrentUser().getHomeDir() + "/metaboanalyst_enrich_sif.zip");
            InputStream input = new FileInputStream(file);
            ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
            return (new DefaultStreamedContent(input, externalContext.getMimeType(file.getName()), file.getName()));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }
}
