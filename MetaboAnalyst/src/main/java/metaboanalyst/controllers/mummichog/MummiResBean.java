/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.mummichog;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.model.ListDataModel;
import metaboanalyst.models.MetSetBean;
import metaboanalyst.models.MummiBean;
import metaboanalyst.models.ResultBeanInteg;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.REnrichUtils;
import metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "mummiBean")
@SessionScoped
public class MummiResBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private String enrichOpt = "fisher";

    public String getEnrichOpt() {
        return enrichOpt;
    }

    public void setEnrichOpt(String enrichOpt) {
        this.enrichOpt = enrichOpt;
    }

    private String currentPathName;

    public void setCurrentPathName(String pn) {
        this.currentPathName = pn; //R start from 1
    }

    public MetSetBean[] getCurrentPathSet() {
        String[] details = REnrichUtils.getMummichogHTMLPathSet(sb.getRConnection(), currentPathName);
        ArrayList<MetSetBean> libVec = new ArrayList();
        libVec.add(new MetSetBean(details[0], details[1], ""));
        return libVec.toArray(new MetSetBean[0]);
    }

    private String pathDBOpt = "hsa_mfn";

    public String getPathDBOpt() {
        return pathDBOpt;
    }

    public void setPathDBOpt(String pathDBOpt) {
        this.pathDBOpt = pathDBOpt;
    }

    private List<ResultBeanInteg> resBeans;

    public List<ResultBeanInteg> getResBeans() {
        return resBeans;
    }

    private String selectedPathName = "NA";

    public String getSelectedPathName() {
        return selectedPathName;
    }

    public void setSelectedPathName(String selectedPathName) {
        sb.setCurrentPathName(selectedPathName);
        this.selectedPathName = selectedPathName;
    }

    private ListDataModel<MummiBean> listModel = null;

    public ListDataModel<MummiBean> getMummiBeans() {
        return listModel;
    }

    public String paBn_action() {

        RConnection RC = sb.getRConnection();
        //if (REnrichUtils.setMassPathLib(RC, pathDBOpt)) {
            if (REnrichUtils.performMummichog(sb, pathDBOpt, enrichOpt, "gamma")) {
                ArrayList<MummiBean> mummiBeans = new ArrayList();
                String[] rownames = REnrichUtils.getMummiPathNames(RC);
                String[] keggLnks = rownames;
                double[][] mat = REnrichUtils.getMummiMat(RC);
                MummiBean mb;
                for (int i = 0; i < rownames.length; i++) {
                    mb = new MummiBean(rownames[i], keggLnks[i], (int) mat[i][0], (int) mat[i][1], (int) mat[i][2], mat[i][3], mat[i][4], mat[i][5]);
                    mummiBeans.add(mb);
                }
                listModel = new ListDataModel(mummiBeans);
            
            return "mummiview";
        } else {
            String msg = RDataUtils.getErrMsg(RC);
            sb.updateMsg("Error", "There is something wrong with the pathway enrichment analysis: " + msg);
            return null;
        }
    }

    public String prepareNetView() {
        return "Metabolic network";
    }
}
