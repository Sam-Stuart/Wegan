/*
 * SessionBean1.java
 *
 * Created on Oct 21, 2008, 9:37:17 AM
 */
package metaboanalyst.controllers;

import metaboanalyst.controllers.stats.TimeBean;
import java.io.File;
import java.io.FileInputStream;
import java.io.FilenameFilter;
import java.io.InputStream;
import metaboanalyst.rwrappers.RCenter;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.models.User;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Arrays;
import java.util.Map;
import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import metaboanalyst.models.RcmdBean;
import metaboanalyst.rwrappers.RGraphUtils;
import metaboanalyst.rwrappers.TimeSeries;
import metaboanalyst.rwrappers.UniVarTests;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.event.NodeSelectEvent;
import org.primefaces.model.DefaultStreamedContent;
import org.primefaces.model.TreeNode;
import org.primefaces.model.menu.DefaultMenuItem;
import org.primefaces.model.menu.DefaultMenuModel;
import org.primefaces.model.menu.MenuModel;
import org.rosuda.REngine.REXP;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

@ManagedBean(name = "sessionBean1")
@SessionScoped
public class SessionBean1 implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");

    public SessionBean1() {

    }

    //****************user defined methods**************
    /**
     * *****************************************
     * Methods for users managements *****************************************
     */
    private boolean loggedIn = false;
    private User currentUser = null;
    private RConnection RC = null;
    private String dataType = "unknown";
    private String analType = "unknown";
    
    
    private String tmpPath = "unknown";
    private String usualPath = "unknown";
    
    
    private boolean paired = false;
    private boolean regression = false;
    private boolean keepClsOrder = true; //for multiple groups using PLS-DA and othogonal PLS-DA
    private boolean dataUploaded = false;
    private boolean dataProcessed = false;
    private boolean integChecked = false;
    private boolean networkQuerySubmitted = false;
    private boolean dataNormed = false;
    private TreeNode naviTree = null;
    private final LinkedHashMap<String, Integer> traceTrack = new LinkedHashMap();
    private final LinkedHashMap<String, String> naviTrack = new LinkedHashMap();
    private boolean keepCollapsed = false;

    //Wegan stuff (Testing to check where exactly things get saved to for tmp users)
    //No longer used Can be deleted
    public String getPath() throws REXPMismatchException {
               
        
        try {
           
            String rCommand = "getTempDir()";
            REXP x = RC.eval(rCommand);
            return(x.asString());
            //RCenter.recordRCommand(RC, rCommand);
            
        } catch (RserveException rse) {
            System.out.println(rse);
            return "";
        }
        
        //return usualPath + tmpPath;
    }
    
    
    
    //Function to more easily access the Temp user directory to display images etc. 
    public String getPath2() {
        String y = ab.getRootContext() + getCurrentUser().getRelativeDir();
        //String x = ab.getRootContext();
        return y;
        //return usualPath + tmpPath;
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    /*
     * Log in and out
     * dataType: list, conc, specbin, pktable, nmrpeak, mspeak, msspec
     * analType: WEGAN,stat, pathora, pathqea, msetora, msetssp, msetqea, msetview, cmpdmap, peaksearch, smpmap
     * */
    public boolean doLogin(String dataType, String analType, boolean isRegression, boolean paired) {

        if (currentUser != null) {
            if (RC != null) {
                RC.close();
            }
            currentUser = null;
            reset2DefaultState();
            if (analType.equals("roc")) {
                FacesContext.getCurrentInstance().getExternalContext().getSessionMap().remove("rocAnalBean");
            }
        }

        currentUser = ab.createTempUser();
        RC = RCenter.getRConnection(currentUser.getHomeDir(), ab.getRscriptLoaderPath(), analType);

        if (RC == null) {
            updateMsg("Error", "Cannot connect to Rserve, please start your Rserver with the right permission!");
            return false;
        }

        //do some house cleaning
        String bashPath = RCenter.getBashFullPath(RC);
        DataUtils.killLongRunningRservProcesses(bashPath);

        FacesContext.getCurrentInstance().getExternalContext().getSessionMap().put("MA4_user", currentUser);

        //setDefaultTableView();
        this.dataType = dataType;
        this.analType = analType;
        this.paired = paired;
        this.regression = isRegression;
        traceTrack.clear();
        RDataUtils.initDataObjects(RC, dataType, analType, paired); //record to R
        loggedIn = true;
        return true;
    }

    public void doLogout() {
        if (loggedIn || currentPageID.equals("Upload")) {
            if (RC != null) {
                //RCenter.showMemoryUsage(RC);
                RC.close();
            }
            try {
                FacesContext.getCurrentInstance().getExternalContext().invalidateSession();
                clearJSESSION();
                FacesContext.getCurrentInstance().getExternalContext().redirect("/MetaboAnalyst/faces/home.xhtml");
                loggedIn = false;
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void clearJSESSION() {
        HttpServletResponse response = (HttpServletResponse) FacesContext.getCurrentInstance().getExternalContext().getResponse();
        HttpServletRequest request = (HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest();
        /* Getting session and then invalidating it */
        HttpSession session = request.getSession(false);
        if (request.isRequestedSessionIdValid() && session != null) {
            session.invalidate();
        }
        Cookie[] cookies = request.getCookies();
        for (Cookie cookie : cookies) {
            cookie.setMaxAge(0);
            cookie.setValue(null);
            cookie.setPath("/");
            response.addCookie(cookie);
        }
    }

    /**
     * Record key steps
     */
    public void reset2DefaultState() {
        this.dataUploaded = false;
        this.dataProcessed = false;
        this.integChecked = false;;
        this.dataNormed = false;
    }

    public void setDataUploaded(boolean dataUploaded) {
        this.dataUploaded = dataUploaded;
        this.dataProcessed = false;
        this.integChecked = false;
        this.dataNormed = false;
    }

    public void setDataProcessed(boolean dataProcessed) {
        this.dataProcessed = dataProcessed;
        this.integChecked = false;
        this.dataNormed = false;
    }

    public void setIntegChecked(boolean integChecked) {
        this.integChecked = integChecked;
        this.dataNormed = false;
    }

    public void setDataNormed(boolean dataNormed) {
        this.dataNormed = dataNormed;
    }

    public boolean isNetworkQuerySubmitted() {
        return networkQuerySubmitted;
    }

    public void setNetworkQuerySubmitted(boolean networkQuerySubmitted) {
        this.networkQuerySubmitted = networkQuerySubmitted;
    }

    public boolean isKeepCollapsed() {
        return keepCollapsed;
    }

    public void setKeepCollapsed(boolean keepCollapsed) {
        this.keepCollapsed = keepCollapsed;
    }

    /*
     * navigation tree
     */
    private final List<String> parentNodes = Arrays.asList(new String[]{"Processing", "Statistics","Dispersal","Plotting", "Enrichment", "Pathway", "Time Series", "ID Conversion",
        "Batch Effect", "ROC Analysis", "Integrative Analysis", "Power Analysis", "Multivariate", "Univariate", "Tester"});
    private final List<String> twoGrpsMethods = Arrays.asList(new String[]{"T-test", "Volcano plot", "Fold change", "EBAM", "SVM", "OrthoPLSDA"});

    public void onNodeSelect(NodeSelectEvent event) {
        TreeNode node = event.getTreeNode();
        String parentNm = node.getParent().getData().toString();
        if (parentNodes.contains(parentNm)) {
            node.getParent().setExpanded(true);
        }
        String naviKey = event.getTreeNode().getData().toString();

        if (analType.equals("utils") | analType.equals("metadata")) {
            FacesContext.getCurrentInstance().getApplication().getNavigationHandler()
                    .handleNavigation(FacesContext.getCurrentInstance(), "null", naviKey);
            return;
        }

        if (!naviKey.equals("Upload") && !naviKey.equals("Exit")) {
            if (!dataUploaded) {
                updateMsg("Error", "You need to upload a dataset first!");
                RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                return;
            }
        }

        switch (naviKey) {
            case "Pre-process":
                if (dataType.equals("conc") || dataType.equals("specbin") || dataType.equals("pktable")) {
                    updateMsg("Error", "Your data type does not need this procedure!");
                    RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Data check":
                if (dataType.equals("conc") || dataType.equals("specbin") || dataType.equals("pktable")) {
                    break;
                } else if (!dataProcessed) {
                    updateMsg("Error", "Your need to pre-process your data first!");
                    RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                    return;
                }
            case "Name check":
                if (!dataType.equals("conc")) {
                    updateMsg("Error", "The procedure is only applicable to compound concentration data!");
                    RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Conc. check":
                if (!analType.equals("msetssp")) {
                    updateMsg("Error", "The procedure is only applicable to single sample profiling!");
                    RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Data editor":
                if (RDataUtils.isReadyForEditor(RC) == 0) {
                    updateMsg("Error", "The data needs to be further processed until normalization page for this procedure! ");
                    RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Missing value":
            case "Data filter":
            case "Image options":
            case "Normalization":
                if (!integChecked) {
                    updateMsg("Error", "The data need to pass integrity check first!");
                    RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Analysis options":
            case "Set parameter":
                //need to work out unspecific case
                if (analType.startsWith("mset")) {
                    naviKey = "enrichparam";
                } else if (analType.startsWith("pathinteg")) {
                    naviKey = "IntegAnal";
                } else if (analType.startsWith("path")) {
                    naviKey = "pathparam";
                } else if (analType.startsWith("power")) {
                    naviKey = "powerparam";
                } else if (analType.startsWith("network")) {
                    naviKey = "MnetParam";
                } else if (analType.startsWith("roc")) {
                    naviKey = "Multivariate";
                } else if (analType.startsWith("mummichog")) {
                    naviKey = "mzlibview";
                }
                break;
            case "View result":
                //need to work out unspecific case
                if (analType.equals("msetora") || analType.equals("msetssp")) {
                    naviKey = "oraview";
                } else if (analType.equals("msetqea")) {
                    naviKey = "qeaview";
                } else if (analType.startsWith("pathinteg")) {
                    naviKey = "IntegRes";
                } else if (analType.startsWith("path")) {
                    naviKey = "pathview";
                } else if (analType.startsWith("power")) {
                    naviKey = "powerview";
                } else if (analType.startsWith("mummichog")) {
                    naviKey = "mummiview";
                }
                break;
            case "Set parameters":
                if (!networkQuerySubmitted) {
                    this.updateMsg("Error", "Please submit the mapped list of inputs!");
                    RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                    return;
                }
                break;
            case "Overview":
                naviKey = "IntegOverview";
                break;
            case "Upload":
            case "ID map":
            case "Exit":
            case "Download":
            case "Metabolic network":
            case "ID mapping":
            case "Network viewer":
                break;
            case "ASCA":
                if (((TimeBean) DataUtils.findBean("timeBean")).isTimeOnly()) {
                    updateMsg("Error", "This method has not been tested for time-series only data!");
                    RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                    return;
                }
                break;
            default://all statisitcal 
                if (!dataNormed) {
                    updateMsg("Error", "The data need to be normalized first!");
                    RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                    return;
                }
                if (multiGroup) {
                    if (twoGrpsMethods.contains(naviKey)) {
                        updateMsg("Error", "The method is only applicable for two-group data analysis!");
                        RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                        return;
                    }
                } else if (naviKey.equals("ANOVA")) {
                    updateMsg("Error", "The method is only for multi-group data analysis!");
                    RequestContext.getCurrentInstance().execute("PF('statusDialog').hide()");
                    return;
                }

        }
        FacesContext.getCurrentInstance().getApplication().getNavigationHandler()
                .handleNavigation(FacesContext.getCurrentInstance(), "null", naviKey);

    }

    public void updateMsg(String type, String content) {
        if (type.equalsIgnoreCase("error")) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_ERROR, "Error", content));
        } else if (type.equalsIgnoreCase("warning")) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_WARN, "Warning", content));
        } else {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_INFO, "OK", content));
        }
    }

    public String enterModule() {
        switch (analType) {
            case "stat":
                
                return "Statistics";
            case "msetqea":
                return "enrichparam";
            case "pathqea":
                return "pathparam";
            case "ts":
                return "tsparam";
            case "power":
                return "powerparam";
            case "roc":
                return "rocparam";
            case "nmds":
                return "Statistics";
            case "diverstiy":
                return "Diversity";
            case "dispersal":
                return "Dispersal";
            case "plotting":
                return "Plotting";
                
        }
        return null;
    }

    /*
     * record the pages that have been visited or is visiting
     */
    public void registerPage(String pageName) {
        if (pageName.equals("Home") | pageName.equals("Module")) {
            doLogout();
        } else {
            if (naviTree != null) {
                if (pageName.equals("Name check")) {
                    String info[] = RDataUtils.getNameCheckMsgs(RC);
                    int state = Integer.parseInt(info[0]);
                    String msg = info[1];
                    switch (state) {
                        case 1:
                            break;
                        case 2:
                            updateMsg("Warning", msg);
                            break;
                        default:
                            updateMsg("Error", msg);
                            break;
                    }
                }

                TreeNode node = DataUtils.getSelectedNode(naviTree, pageName);
                System.out.print(" Currently Here -----------------------------");
                System.out.print(pageName);
                node.setSelected(true);
                String parentNm = node.getParent().getData().toString();
                if (parentNodes.contains(parentNm)) {
                    //expant the current parent
                    node.getParent().setExpanded(true);
                    if (parentNm.equals("Tester") | parentNm.equals("Multivariate") | parentNm.equals("Univariate")) {// three layers here
                        node.getParent().getParent().setExpanded(true);
                    }
                    //collapse the others
                    if (!parentNm.equals("Processing") && !analType.equals("utils") && !analType.equals("pathinteg") && !analType.equals("batch")) {
                        DataUtils.getSelectedNode(naviTree, "Processing").setExpanded(false);
                    }
                }
            }
            traceTrack.put(pageName, 0);
            currentPageID = pageName;
        }
    }
    

    /**
     * To remember all image names in order to update immediately to avoid
     * caching problem. And remove images left previously
     */
    private HashMap<String, Integer> imgMap = new HashMap();

    public String getCurrentImage(String key) {
        if (!imgMap.containsKey(key)) {
            imgMap.put(key, 0);
        }
        return key + "_" + imgMap.get(key) + "_";
    }

    public String getNewImage(String key) {
        if (!imgMap.containsKey(key)) {
            imgMap.put(key, 0);
        } else {
            imgMap.put(key, imgMap.get(key) + 1);
        }
        return key + "_" + imgMap.get(key) + "_";
    }

    /**
     * Get images for display
     *
     * @param name: the short image name
     * @return the image at the specified URL
     */
    //@return path to image
    public String getCurrentImageURL(String name) {
        return ab.getRootContext() + getCurrentUser().getRelativeDir() + File.separator + getCurrentImage(name) + "dpi72.png";
    }

    
    
    
    
    
    
    
    
    /**
     * get JSON files for interactive
     *
     * @param name: file name
     * @return the URL
     */
    public String getJsonDir(String name) {
        return currentUser.getRelativeDir() + "/" + getCurrentImage(name) + ".json";
    }

    /**
     * To record all commands that produce the images
     */
    private final HashMap<String, String> graphicsMap = new HashMap();

    public void addGraphicsCMD(String key, String rcmd) {
        graphicsMap.put(key, rcmd);
    }

    public HashMap<String, String> getGraphicsMap() {
        return graphicsMap;
    }

    //when data changed, reset to allow recomputing
    public void resetAnalysis() {
        Iterator<TreeNode> i = naviTree.getChildren().iterator();
        while (i.hasNext()) {
            TreeNode nd = i.next();
            if (nd.getData().toString().equals("Statistics")
                    || nd.getData().toString().equals("Time Series")
                    || nd.getData().toString().equals("Pathway")
                    || nd.getData().toString().equals("Enrichment")
                    || nd.getData().toString().equals("ROC Analysis")
                    || nd.getData().toString().equals("Power Analysis")) {
                Iterator<TreeNode> i2 = nd.getChildren().iterator();
                while (i2.hasNext()) {
                    TreeNode nd2 = i2.next();
                    traceTrack.remove(nd2.getData().toString());
                }
            }
        }
    }

    public boolean isAnalInit(String analCode) {
        return traceTrack.keySet().contains(analCode);
    }

    private String currentPageID = "";

    public String getCurrentPageID() {
        return currentPageID;
    }

    public TreeNode getNaviTree() {
        return naviTree;
    }

    public void destroyUser() {
        currentUser = null;
        RC.close();
    }

    public void setAnalType(String type) {
        analType = type;
        naviTree = DataUtils.createNaviTree(type);
    }

    public String getAnalType() {
        return analType;
    }

    public String getDataType() {
        return dataType;
    }

    public boolean isPaired() {
        return paired;
    }

    public boolean isRegresion() {
        return regression;
    }

    public User getCurrentUser() {
        return currentUser;
    }

    public RConnection getRConnection() {
        return RC;
    }

    private boolean multiGroup = false;

    public boolean isMultiGroup() {
        return multiGroup;
    }

    public void setMultipleGroup(boolean multiGroup) {
        this.multiGroup = multiGroup;
    }

    public boolean isKeepClsOrder() {
        return keepClsOrder;
    }

    public void setKeepClsOrder(boolean keepClsOrder) {
        this.keepClsOrder = keepClsOrder;
    }

    private boolean smallSmplSize = false;

    public boolean isSmallSmplSize() {
        return smallSmplSize;
    }

    public void setSmallSmplSize(boolean smallSmplSize) {
        this.smallSmplSize = smallSmplSize;
    }

    //record whether ttests or anova give significant features
    private boolean anovaSig = false;
    private boolean ttSig = false;

    public boolean isAnovaSig() {
        return anovaSig;
    }

    public void setAnovaSig(boolean anovaSig) {
        this.anovaSig = anovaSig;
    }

    public boolean isTtSig() {
        return ttSig;
    }

    public void setTtSig(boolean ttSig) {
        this.ttSig = ttSig;
    }

    ArrayList<String> sigVec = new ArrayList();

    public String detailsLnk_action(String code) {
        sigVec.add(code);
//        sigVec.add(table);
        System.out.print("----------------------------detailsLnk_action CLASSASSSSS in sessionBean1------------------------");
        System.out.print(" What the fuck");
        System.out.print(code);
        System.out.print(sigVec);
        return "featuredetails";
    }
    
   

    public String getSigSource() {
        return sigVec.get(sigVec.size() - 1);
    }

    ArrayList<String> imgIDVec = new ArrayList();

    public void graphicsLnk_action(String code) {
        imgIDVec.add(code);
    }

    public String getImageSource() {
        return imgIDVec.get(imgIDVec.size() - 1);
    }

    private String currentPathName, currentMBname, currentCmpdName;

    public String getCurrentPathName() {
        return currentPathName;
    }

    public void setCurrentPathName(String currentPathName) {
        this.currentPathName = currentPathName;
    }

    public String getCurrentMBname() {
        return currentMBname;
    }

    public void setCurrentMBname(String currentMBname) {
        this.currentMBname = currentMBname;
    }

    public String getCurrentCmpdName() {
        return currentCmpdName;
    }

    public void setCurrentCmpdName(String currentCmpdName) {
        this.currentCmpdName = currentCmpdName;
    }

    //Remember state for ajax jqplot
    private String[] featureLbls;

    public String[] getFeatureLabels() {
        return featureLbls;
    }

    public void setFeatureLabels(String[] nodeIDs) {
        this.featureLbls = nodeIDs;
    }

    private double[] featureIndex;

    public void setFeatureInx(double[] inx) {
        featureIndex = inx;
    }

    public double[] getFeatureInx() {
        return featureIndex;
    }

    public void setCurrentCmpdIndex() {
        Map<String, String> params = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap();
        double xPos = Double.parseDouble(params.get("cmpdIndex"));
        for (int i = 0; i < featureIndex.length; i++) {
            if (xPos == featureIndex[i]) {
                plotCmpd(i);
                break;
            }
        }
    }
    private String cmpdImg = null;

    private void plotCmpd(int inx) {
        String name = featureLbls[inx];
        UniVarTests.PlotCmpdView(RC, name, "png", 72 + "");
        String cmpdName = name.replaceAll("\\/", "_");
        cmpdImg = ab.getRootContext() + getCurrentUser().getRelativeDir() + File.separator + cmpdName + "_dpi72.png";
    }

    public String getCmpdImg() {
        if (cmpdImg == null) {
            cmpdImg = ab.getRootContext() + "/resources/images/background.png";
        }
        return cmpdImg;
    }

    public void viewCmpdSummary(String name) {
        UniVarTests.PlotCmpdSummary(this, name, "png", 72 + "");
        currentCmpdName = name.replaceAll("\\/", "_");
        RequestContext.getCurrentInstance().scrollTo("form1:imgPane");
    }

    public String getCmpdSummaryImg() {
        return ab.getRootContext() + getCurrentUser().getRelativeDir() + File.separator + currentCmpdName + "_summary_dpi72.png";
    }

    //need to remember some states cross pages
    private boolean msPeakAligned = false;
    private boolean msSpecAligned = false;

    public boolean isMsPeakAligned() {
        return msPeakAligned;
    }

    public void setMsPeakAligned(boolean msPeakAligned) {
        this.msPeakAligned = msPeakAligned;
    }

    public boolean isMsSpecAligned() {
        return msSpecAligned;
    }

    public void setMsSpecAligned(boolean msSpecAligned) {
        this.msSpecAligned = msSpecAligned;
    }

    //For graph regeneration
    private String formatOpt = "png";

    public String getFormatOpt() {
        return formatOpt;
    }

    public void setFormatOpt(String formatOpt) {
        this.formatOpt = formatOpt;
    }

    private int dpiOpt = 300;

    public int getDpiOpt() {
        return dpiOpt;
    }

    public void setDpiOpt(int dpiOpt) {
        this.dpiOpt = dpiOpt;
    }

    private String sizeOpt = "NA";

    public String getSizeOpt() {
        return sizeOpt;
    }

    public void setSizeOpt(String sizeOpt) {
        this.sizeOpt = sizeOpt;
    }

    private String imgDownloadTxt = "";

    public String getImgDownloadTxt() {
        return imgDownloadTxt;
    }

    public String graphBn_action() {
        // TODO: Process the action. Return value is a navigation
        // case name where null will return to the same page.
        //updateColorSetting();
        SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
        String key = sb.getImageSource();
        String imgName = "";
        String mydpi = "72";
        if (formatOpt.equals("png") || formatOpt.equals("tiff")) {
            mydpi = dpiOpt + "";
        }
        switch (key) {
            case "pathway":
                if (currentPathName == null) {
                    updateMsg("Error", "No command found for plotting the image!");
                    return null;
                }
                imgName = RGraphUtils.plotKEGGPath(sb, currentPathName, formatOpt, sizeOpt, mydpi);
                break;
            case "mb": {
                String cmpdName = sb.getCurrentMBname();
                if (cmpdName == null) {
                    updateMsg("Error", "No command found for plotting the image!");
                    return null;
                }
                imgName = TimeSeries.plotMBProfile(sb, cmpdName, formatOpt, mydpi + "");
                break;
            }
            case "cmpd": {
                String cmpdName = sb.getCurrentCmpdName();
                if (cmpdName == null) {
                    updateMsg("Error", "No command found for plotting the image!");
                    return null;
                }
                imgName = UniVarTests.PlotCmpdSummary(sb, cmpdName, formatOpt, mydpi + "");
                break;
            }
            default:
                String rcmd = sb.getGraphicsMap().get(key);
                if (rcmd == null) {
                    updateMsg("Error", "No command found for plotting the image!");
                    return null;
                }
                rcmd = rcmd.replace("png", formatOpt);
                rcmd = rcmd.replace("72", mydpi + "");
                rcmd = rcmd.replace("width=NA", "width=" + sizeOpt);
                imgName = sb.getCurrentImage(key);
                imgName = imgName.replaceAll("\\/", "_");
                imgName = imgName + "dpi" + mydpi + "." + formatOpt;
                try {
                    RConnection RC = sb.getRConnection();
                    RCenter.recordRCommand(RC, rcmd);
                    RC.voidEval(rcmd);
                } catch (Exception e) {
                    e.printStackTrace();
                }
                break;
        }

        imgDownloadTxt = "<b>Download the image: </b> <a target='_blank' href = \"/MetaboAnalyst/resources/users/" + sb.getCurrentUser().getName()
                + File.separator + imgName + "\"><b>" + imgName + "</b></a>";
        return null;
    }

    public DefaultStreamedContent getRCmdFile() {
        try {
            File file = new File(getCurrentUser().getHomeDir() + "/Rhistory.R");
            InputStream input = new FileInputStream(file);
            ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
            return (new DefaultStreamedContent(input, externalContext.getMimeType(file.getName()), file.getName()));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    public ArrayList<RcmdBean> getCmdVec() {
        if (RC != null) {
            String[] cmds = RCenter.getRCommandHistory(RC);
            ArrayList<RcmdBean> myCmds = new ArrayList();
            for (int i = 0; i < cmds.length; i++) {
                myCmds.add(new RcmdBean(i + 1, cmds[i]));
            }
            return myCmds;
        }
        return null;
    }

    private static final String origStyle = "color: #222222; text-decoration: underline";
    private static final String highlightStyle = "color: maroon; text-decoration: none";

    public void addNaviTrack(String pageName, String naviCode) {
        naviTrack.put(pageName, naviCode);
    }

    public MenuModel getSimpleMenuModel() {
        MenuModel simpleMenuModel = new DefaultMenuModel();
        DefaultMenuItem menuItem = new DefaultMenuItem();
        menuItem.setValue("Home");
        menuItem.setUrl("/faces/home.xhtml");
        simpleMenuModel.addElement(menuItem);
        menuItem = new DefaultMenuItem();
        menuItem.setValue("Module");
        menuItem.setUrl("/faces/ModuleView.xhtml");
        simpleMenuModel.addElement(menuItem);
        Iterator<String> it = naviTrack.keySet().iterator();

        while (it.hasNext()) {
            String key = it.next();//System.out.println("========="+key);
            String url = naviTrack.get(key);
            menuItem = new DefaultMenuItem();
            menuItem.setValue(key);
            menuItem.setUrl(url);
            if (key.equals(currentPageID)) {
                menuItem.setStyle(highlightStyle);
            } else {
                menuItem.setStyle(origStyle);
            }
            simpleMenuModel.addElement(menuItem);
        }
        return simpleMenuModel;
    }

    public DefaultStreamedContent getDownloadFile(String fileNm) {
        try {
            File file = new File(currentUser.getHomeDir() + "/" + fileNm);
            InputStream input = new FileInputStream(file);
            ExternalContext externalContext = FacesContext.getCurrentInstance().getExternalContext();
            return (new DefaultStreamedContent(input, externalContext.getMimeType(file.getName()), file.getName()));
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    private String org = "NA";

    public String getOrg() {
        return org;
    }

    public void setOrg(String org) {
        this.org = org;
        RDataUtils.setOrganism(RC, org);
    }

    public void setupFileDownloadZip() {

        File folder = new File(currentUser.getHomeDir());

        //remove previous (if any) zip file
        DataUtils.deleteFile(currentUser, "Download.zip");

        File[] listOfFiles = folder.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".csv");
            }
        });
        DataUtils.createZipFile(listOfFiles, currentUser.getHomeDir());
    }

    public DefaultStreamedContent getDownloadImage(String key, String formatOpt) {
        return (getDownloadFile(key + "." + formatOpt));
    }
    
    //There is difference in URL 
    //public: http://www.metaboanalyst.ca/faces/ModuleView.xhtml          <= faces/
    //local: http://localhost:8080/MetaboAnalyst/faces/ModuleView.xhtml  <= MetaboAnalyst/faces/
    public String getUrlPrefix(){
        if(ab.isOnPublicServer()){
            return "";
        }else{
            return "/MetaboAnalyst";
        }
    }
}
