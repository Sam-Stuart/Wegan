/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.functions;

import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.RIntegUtils;
import metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;

/**
 * This bean is for tools of various ID mapping
 *
 * @author jianguox
 */
@ManagedBean(name = "integLoader")
public class IntegLoadBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private String integOrg = "NA";

    public String getIntegOrg() {
        return integOrg;
    }

    public void setIntegOrg(String integOrg) {
        this.integOrg = integOrg;
    }

    private String geneList;

    public String getGeneList() {
        return geneList;
    }

    public void setGeneList(String geneList) {
        this.geneList = geneList;
    }

    private String cmpdList;

    public String getCmpdList() {
        return cmpdList;
    }

    public void setCmpdList(String cmpdList) {
        this.cmpdList = cmpdList;
    }

    private boolean useExample = false;

    public boolean isUseExample() {
        return useExample;
    }

    public void setUseExample(boolean useExample) {
        this.useExample = useExample;
    }

    private String geneIDType = "NA";
    private String cmpdIDType = "NA";

    public String getGeneIDType() {
        return geneIDType;
    }

    public void setGeneIDType(String geneIDType) {
        this.geneIDType = geneIDType;
    }

    public String getCmpdIDType() {
        return cmpdIDType;
    }

    public void setCmpdIDType(String cmpdIDType) {
        this.cmpdIDType = cmpdIDType;
    }

    public void updateListArea() {
        if (useExample) {
            integOrg = "hsa";
            cmpdList = DataUtils.readTextFile(ab.getIntegCmpdPath());
            cmpdIDType = "kegg";
            geneList = DataUtils.readTextFile(ab.getIntegGenePath());
            geneIDType = "symbol";
        } else {
            cmpdList = "";
            integOrg = "NA";
            cmpdIDType = "NA";
            geneList = "";
            geneIDType = "NA";
        }
    }

    private boolean loggedIn = false;
    private boolean cmpdMapped = false;

    public void handleCmpdListUpload() {

        cmpdList = cmpdList.trim();
        cmpdMapped = false;
        RConnection RC = sb.getRConnection();
        int res = RIntegUtils.performCmpdMapping(RC, cmpdList, integOrg, cmpdIDType);

        String info[] = RDataUtils.getNameCheckMsgs(RC);
       // int state = Integer.parseInt(info[0]);
        String msg = info[1];

        if (res == 1) {
            sb.updateMsg("OK", msg);
            cmpdMapped = true;
        } else {
            sb.updateMsg("Error", msg);
        }
    }

    private boolean geneMapped = false;

    public void handleGeneListUpload() {

        geneList = geneList.trim();
        geneMapped = false;
        RConnection RC = sb.getRConnection();
        int res = RIntegUtils.performGeneMapping(RC, geneList, integOrg, geneIDType);
        String msg = RDataUtils.getCurrentMsg(RC);
        if (res == 1) {
            sb.updateMsg("OK", msg);
            geneMapped = true;
        } else {
            sb.updateMsg("Error", msg);
        }
    }

    public String integrityCheck() {

        if (integOrg.equals("NA")) {
            sb.updateMsg("Error", "Please choose an organism!");
            return null;
        }

        if (!loggedIn) {
            if (!sb.doLogin("conc", "pathinteg", false, false)) {
                sb.updateMsg("Error", "Log in failed. Please check errors in your R codes or the Rserve permission setting!");
                return null;
            } else {
                loggedIn = true;
            }
        }

        RDataUtils.setOrganism(sb.getRConnection(), integOrg);

        if (!(geneList == null | geneList.trim().length() == 0)) {
            if (geneIDType.equals("na")) {
                sb.updateMsg("Error", "Please specify gene ID type!");
                return null;
            }
            handleGeneListUpload();
        }
        if (!(cmpdList == null | cmpdList.trim().length() == 0)) {
            if (cmpdIDType.equals("na")) {
                sb.updateMsg("Error", "Please specify compound ID type!");
                return null;
            }
            handleCmpdListUpload();
        }

        if (!geneMapped && !cmpdMapped) {

            sb.updateMsg("Error", "Please enter valid input!");
            return null;
        }
        if (!geneMapped && cmpdMapped) {
            sb.updateMsg("Error", "Please use Pathway Analysis module for only metabolite list!");
            return null;
        }
        sb.setDataUploaded(true);
        return "ID map";
    }
}
