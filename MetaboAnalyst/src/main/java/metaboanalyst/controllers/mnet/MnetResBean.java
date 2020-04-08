/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.mnet;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.models.MetSetBean;
import metaboanalyst.models.ResultBeanInteg;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.NetBean;
import metaboanalyst.models.NetsDataModel;
import metaboanalyst.rwrappers.REnrichUtils;
import metaboanalyst.rwrappers.RIntegUtils;
import metaboanalyst.rwrappers.RNetworkUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.DefaultStreamedContent;
import org.primefaces.model.chart.ChartSeries;
import org.primefaces.model.chart.HorizontalBarChartModel;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "mnetResBean")
@SessionScoped
public class MnetResBean implements Serializable {

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
    
    /**
     * Network methods
     */
    private int nodeCount = 0;
    private int edgeCount = 0;

    public int getNodeCount() {
        return nodeCount;
    }

    public int getEdgeCount() {
        return edgeCount;
    }
    
    private String visMode = "static"; //4 mode: ppi, chem, tf, drug
    private double confThresh = 0.5;

    public String getVisMode() {
        return visMode;
    }

    public void setVisMode(String visMode) {
        this.visMode = visMode;
    }

    public double getConfThresh() {
        return confThresh;
    }

    public void setConfThresh(double confThresh) {
        this.confThresh = confThresh;
    }
    
    private FacesMessage checkMsg;

    private void updateCheckMsg() {
        if (nodeCount > 2000) {
            checkMsg = new FacesMessage(FacesMessage.SEVERITY_WARN, "Warning",
                    "The network size is over <b style='color: orange'>2000</b> nodes! We recommend switch from current network to "
                    + "<b>Zero-order network</b> or <b>Minimum Network</b> function to reduce the size for better performance and experience.");
        } else if (nodeCount > 1200) {
            checkMsg = new FacesMessage(FacesMessage.SEVERITY_INFO, "OK",
                    "Click the <b>Proceed</b> button to perform network analysis and visualization. "
                    + "You may also consider <b>Zero-order network</b> or <b>Minimum Network</b> to reduce the network size "
                    + "for better performance and experience.");
        } else {
            checkMsg = new FacesMessage(FacesMessage.SEVERITY_INFO, "OK",
                    "Please click the <b>Proceed</b> button to perform network analysis and visualization.");
        }
    }
    
    private int queryCount = 0;

    public int getQueryCount() {
        return queryCount;
    }
    private int seedCount = 0;

    public int getSeedCount() {
        return seedCount;
    }
    
    private int ppiOrder = 1;
    
    private boolean netOK = false;

    public boolean isNetOK() {
        return netOK;
    }

    public void setNetOK(boolean netOK) {
        this.netOK = netOK;
    }
    
    public String preparePhenoNetwork() {
        String dbType = "pheno";
        netOK = false;

        int[] res = RNetworkUtils.searchNetDB(sb.getRConnection(), dbType, visMode, confThresh);
        nodeCount = res[0];
        edgeCount = res[1];

        if (nodeCount == 0) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "No matches found for the given seed genes/proteins."));
            return null;
        }

        int[] counts = RNetworkUtils.createIGraph(sb.getRConnection());
        if (counts[0] > 0) {
            queryCount = counts[0];
            seedCount = counts[1];
            nodeCount = counts[2];
            edgeCount = counts[3];
            populateNetBeans();
            updateCheckMsg();
            FacesContext.getCurrentInstance().addMessage("OK", checkMsg);
            ppiOrder = 1;
            
            //default load the the first network
            String netNm = "subnetwork1";
            RNetworkUtils.prepareNetwork(sb.getRConnection(), netNm, "networkanalyst_0.json");
            netOK = true;
            return "MnetStats";
            
        } else {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", "Failed  to process your request: no interaction partners identified."));
            return null;
        }
    }
    
    //type is globaltest or DE-hypergeo test
    public String prepareKEGGNetwork() {
        RNetworkUtils.prepareQueryJson(sb.getRConnection());
        String nm = "network_enrichment_pathway_0";
        RNetworkUtils.doEnrichmentTest_KO01100(sb.getRConnection(), "pathway", nm);
        return "MnetView";
    }

    public String doMnetworkAnalysis(String visMode) {
        this.visMode = visMode;
        String networkpage;
        if(visMode.equals("static")){
            networkpage = prepareKEGGNetwork();
        }
        else{
            networkpage = preparePhenoNetwork();
        }
        return networkpage;
    }
    
    private int imgCount = 0;
    public String exportNetwork(String format) {
        String fileName = "network_" + imgCount;
        if (format.equalsIgnoreCase("graphml")) {
            fileName = fileName + ".graphml";
        } else if (format.equalsIgnoreCase("ndtb")) {//node property table, already there
            fileName = "node_table.csv";
            return fileName;
        } else if (format.equalsIgnoreCase("comtb")) {
            fileName = "module_table.csv";
            return fileName;
        } else if (format.equalsIgnoreCase("funtb")) {
            fileName = "metaboanalyst_enrichment.csv";
            return fileName;
        } else {
            System.out.println("===========Not supported format: " + format);
            return "NA";
        }
        RNetworkUtils.exportNetwork(sb.getRConnection(), fileName);
        imgCount++;
        return fileName;
    }
    
    public String extractModule(String nodeIDs) {
        String res = RNetworkUtils.extractModule(sb.getRConnection(), nodeIDs);
        populateNetBeans(); //remember to update the net stats
        return (res);
    }
    
    //all files generated
    private int fileCount = 0;

    public int getFileCount() {
        fileCount++;
        return fileCount;
    }
    
    private NetsDataModel netsModel;

    public NetsDataModel getNetsDataModel() {
        return netsModel;
    }

    private void populateNetBeans() {
        List<NetBean> netBeans = new ArrayList();
        RConnection RC = sb.getRConnection();
        
        String[] nms = RNetworkUtils.getNetsNames(RC);
        int[] nds = RNetworkUtils.getNetsNodeNum(RC);
        int[] egs = RNetworkUtils.getNetsEdgeNum(RC);
        int[] qs = RNetworkUtils.getNetsQueryNum(RC);
        for (int i = 0; i < nms.length; i++) {
            netBeans.add(new NetBean(nms[i], nds[i], egs[i], qs[i]));
        }
        netsModel = new NetsDataModel(netBeans);
    }

    private List<ResultBeanInteg> resBeans;

    public List<ResultBeanInteg> getResBeans() {
        return resBeans;
    }

    private void populateResBeans() {
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
    
    private int jsonCount = 0;
    
    public String updateNetworkLayout(String algo) {
        jsonCount++;
        String jsonName = "metaboanalyst_" + jsonCount + ".json";
        String res = RNetworkUtils.updateNetworkLayout(sb.getRConnection(), algo, jsonName);
        return res;
    }
    
    public String performNodesFilter(String nodeIDs) {
        jsonCount++;
        String jsonName = "metaboanalyst_node_" + jsonCount + ".json";
        String res = RNetworkUtils.excludeNodes(sb.getRConnection(), nodeIDs, jsonName);
        populateNetBeans();
        return res;
    }
    
    //ajax call to switch network
    public String loadNetwork(String netName) {
        jsonCount++;
        String jsonName = "metaboanalyst_" + jsonCount + ".json";
        int res = RNetworkUtils.prepareNetwork(sb.getRConnection(), netName, jsonName);
        if (res == 1) {
            return jsonName;
        } else {
            return "NA";
        }
    }
    
     public DefaultStreamedContent getSifFile(String name) {
          RNetworkUtils.prepareSubnetDownload(sb.getRConnection(), name);
        try {
            File file = new File(sb.getCurrentUser().getHomeDir() + "/" + name + "_sif.zip");
            InputStream input = new FileInputStream(file);
            ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
            return (new DefaultStreamedContent(input, externalContext.getMimeType(file.getName()), file.getName()));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }    
    
    public int doIntegPathwayAnanlysis(String qlist, String nm) {
        resBeans = new ArrayList<>();
        RConnection RC = sb.getRConnection();
        
        String pathDBOpt;
        if(visMode.equals("gene_metabolites")){
            pathDBOpt = "integ";
        } else if(visMode.equals("metabo_metabolites") || visMode.equals("metabo_phenotypes")){
            pathDBOpt = "metab";
        } else{
            pathDBOpt = "genetic";
        }
        
        int res = RNetworkUtils.updateIntegPathwayAnalysis(RC, qlist, nm, topoOpt, enrichOpt, pathDBOpt);
//        if (res == 1) {
//            return "IntegOverview";
//        } else {
//            String err = RDataUtils.getErrMsg(RC);
//            sb.updateMsg("Analysis failed! ", err);
//            return null;
//        }
        return 1;
    }

}
