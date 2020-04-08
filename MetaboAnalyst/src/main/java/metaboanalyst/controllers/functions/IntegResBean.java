/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.functions;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.MetSetBean;
import metaboanalyst.models.ResultBeanInteg;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.REnrichUtils;
import metaboanalyst.rwrappers.RIntegUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.chart.ChartSeries;
import org.primefaces.model.chart.HorizontalBarChartModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "integResBean")
@SessionScoped
public class IntegResBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private String enrichOpt = "hyper";
    private String topoOpt = "dc";

    public String getEnrichOpt() {
        return enrichOpt;
    }

    public void setEnrichOpt(String enrichOpt) {
        this.enrichOpt = enrichOpt;
    }

    public String getTopoOpt() {
        return topoOpt;
    }

    public void setTopoOpt(String topoOpt) {
        this.topoOpt = topoOpt;
    }

    private String currentPathName;

    public void setCurrentPathName(String pn) {
        sb.setCurrentPathName(pn);
        this.currentPathName = pn; //R start from 1
    }

    public MetSetBean[] getCurrentPathSet() {
        String[] details = REnrichUtils.getIntegHTMLPathSet(sb.getRConnection(), currentPathName);
        ArrayList<MetSetBean> libVec = new ArrayList();
        libVec.add(new MetSetBean(details[0], details[1], details[2]));
        return libVec.toArray(new MetSetBean[0]);
    }

    private SelectItem[] enrichOpts;

    public SelectItem[] getEnrichOpts() {
        if (enrichOpts == null) {
            enrichOpts = new SelectItem[2];
            enrichOpts[0] = new SelectItem("hyper", "Hypergeometric Test");
            enrichOpts[1] = new SelectItem("fisher", "Fisher's Exact Test");
            //   enrichOpts[2] = new SelectItem("global", "Global Test");
        }
        return enrichOpts;
    }
    private SelectItem[] topoOpts;

    public SelectItem[] getTopoOpts() {
        if (topoOpts == null) {
            topoOpts = new SelectItem[3];
            topoOpts[0] = new SelectItem("dc", "Degree Centrality");
            topoOpts[1] = new SelectItem("bc", "Betweenness Centrality");
            topoOpts[2] = new SelectItem("cc", "Closeness Centrality");
        }
        return topoOpts;
    }

    public String doIntegPathwayAnanlysis() {
        RConnection RC = sb.getRConnection();
        int res = RIntegUtils.performIntegPathwayAnalysis(RC, topoOpt, enrichOpt, pathDBOpt);
        if (res == 1) {
            return "IntegOverview";
        } else {
            String err = RDataUtils.getErrMsg(RC);
            sb.updateMsg("Analysis failed! ", err);
            return null;
        }
    }

    public String setupResTable() {
        populateResBeans();
        return "IntegRes";
    }

    private String pathDBOpt = "integ";

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

    private void populateResBeans() {
                resBeans = new ArrayList<>();
        RConnection RC = sb.getRConnection();

        String[] columnKeys = RIntegUtils.getIntegResColNames(RC);
        String[] ids = RIntegUtils.getIntegPathIDs(RC);
        String[] nms = RIntegUtils.getIntegPathNames(RC);
        double[][] resMat = RIntegUtils.getIntegResMatrix(RC);

        //set up object list and bar graph
        ResultBeanInteg rb;
        if (nms != null && nms.length > 0) {
            for (int i = 0; i < nms.length; i++) {
                rb = new ResultBeanInteg();
                rb.setID(ids[i]);
                rb.setName(nms[i]);
                rb.setHit(i);

                for (int m = 0; m < columnKeys.length; m++) {
                    double val = resMat[i][m];
                    if (columnKeys[m].equals("Total")) {
                        rb.setSetSize((int) Math.round(val));
                    } else if (columnKeys[m].equals("Expected")) {
                        rb.setExpected(val);
                    } else if (columnKeys[m].equals("Hits")) {
                        rb.setHit((int) Math.round(val));
                    } else if (columnKeys[m].equals("P.Value")) {
                        rb.setPval(val);
                    } else if (columnKeys[m].equals("Topology")) {
                        rb.setTopoVal(val);
                    }
                }
                resBeans.add(rb);
            }
        }
    }

    private String selectedPathName = "NA";

    public String getSelectedPathName() {
        return selectedPathName;
    }

    public void setSelectedPathName(String selectedPathName) {
        sb.setCurrentPathName(selectedPathName);
        this.selectedPathName = selectedPathName;
    }

    private HorizontalBarChartModel categoryModel;

    public HorizontalBarChartModel getCategoryModel() {
        createBarModel();
        return categoryModel;
    }

    private void createBarModel() {
        categoryModel = new HorizontalBarChartModel();
        ChartSeries enrichs = new ChartSeries();
        enrichs.setLabel("Enrichment");
        ChartSeries topos = new ChartSeries();
        topos.setLabel("Topology");
        RConnection RC = sb.getRConnection();
        double[][] coords = RIntegUtils.getPathBarStats(RC);
        String[] nms = RIntegUtils.getPathBarNames(RC);

        for (int i = 0; i < nms.length; i++) {
            enrichs.set(nms[i], (int) Math.round(1000 * coords[i][0]));
            topos.set(nms[i], (int) Math.round(1000 * coords[i][1]));
        }
        categoryModel.addSeries(enrichs);
        categoryModel.addSeries(topos);

        categoryModel.setZoom(true);
        categoryModel.setExtender("ext");
        categoryModel.setTitle("Pathway Analysis Overview");
        categoryModel.setLegendPosition("e");
        categoryModel.setStacked(true);
    }

    public String getDownloadPathwayFile() {
        return "<a href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + File.separator + "MetaboAnalyst_result_pathway.csv\">Download Result</a>";
    }

}
