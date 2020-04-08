/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.mummichog;

import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.UploadedFile;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "peakLoader")
public class PeakUploadBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private boolean usePeakDataExample;
    private UploadedFile peakFile;
    private String orgOpt = "hsa_recon";
    private double instrumentOpt = 0.1;
    private double pvalCutoff = 0.0001;
    private int permNum = 100;

    private String analType = "mummichog";

    public int getPermNum() {
        return permNum;
    }

    public void setPermNum(int permNum) {
        this.permNum = permNum;
    }

    public double getInstrumentOpt() {
        return instrumentOpt;
    }

    public void setInstrumentOpt(double instrumentOpt) {
        this.instrumentOpt = instrumentOpt;
    }

    private String msModeOpt = "positive";

    public String getMsModeOpt() {
        return msModeOpt;
    }

    public void setMsModeOpt(String msModeOpt) {
        this.msModeOpt = msModeOpt;
    }

    public String getOrgOpt() {
        return orgOpt;
    }

    public void setOrgOpt(String orgOpt) {
        this.orgOpt = orgOpt;
    }

    public double getPvalCutoff() {
        return pvalCutoff;
    }

    public void setPvalCutoff(double pvalCutoff) {
        this.pvalCutoff = pvalCutoff;
    }

    public boolean isUsePeakDataExample() {
        return usePeakDataExample;
    }

    public void setUsePeakDataExample(boolean usePeakDataExample) {
        this.usePeakDataExample = usePeakDataExample;
    }

    public UploadedFile getPeakFile() {
        return peakFile;
    }

    public void setPeakFile(UploadedFile peakFile) {
        this.peakFile = peakFile;
    }

    private String peakOraList = "";

    public String getPeakOraList() {
        return peakOraList;
    }

    public void setPeakOraList(String peakOraList) {
        this.peakOraList = peakOraList;
    }

    private boolean usePeakListExample = false;

    public boolean isUsePeakListExample() {
        return usePeakListExample;
    }

    public void setUsePeakListExample(boolean usePeakListExample) {
        this.usePeakListExample = usePeakListExample;
    }

    public String handleMassAllUpload() {
        if (!sb.doLogin("mass_all", analType, false, false)) {
            sb.updateMsg("Error", "Failed to log in!");
            return null;
        }

        String fileName = "";
        if (usePeakDataExample) {
            fileName = ab.getTestMummichog();
            sb.setDataUploaded(true);
        } else {
            try {
                if (peakFile.getSize() == 0) {
                    sb.updateMsg("Error", "File is empty!");
                    return null;
                }
                fileName = DataUtils.getJustFileName(peakFile.getFileName());
                DataUtils.uploadFile(peakFile, sb, null, ab.isOnPublicServer());
                sb.setDataUploaded(true);

            } catch (Exception e) {
                e.printStackTrace();
                return null;
            }
        }

        RConnection RC = sb.getRConnection();
        if (RDataUtils.readPeakListData(RC, fileName)) {
            RDataUtils.setMummichogParams(RC, instrumentOpt, msModeOpt, pvalCutoff);
         
            return "Data check";
        } else {
            String err = RDataUtils.getErrMsg(sb.getRConnection());
            sb.updateMsg("Error", "Failed to read in the peak list file." + err);
            return null;
        }
    }

}
