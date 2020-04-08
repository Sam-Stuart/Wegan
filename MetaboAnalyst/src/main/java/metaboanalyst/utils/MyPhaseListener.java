/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.utils;

import java.io.File;
import javax.faces.context.ExternalContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import metaboanalyst.controllers.functions.MsetBean;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.controllers.functions.PathBean;
import metaboanalyst.controllers.mnet.MnetLoadBean;
import metaboanalyst.controllers.mnet.MnetResBean;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.rwrappers.RGraphUtils;
import metaboanalyst.rwrappers.RIntegUtils;
import metaboanalyst.rwrappers.RNetworkUtils;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author Jeff
 */
public class MyPhaseListener implements PhaseListener {

    static long current = System.currentTimeMillis();
    private static final String AJAX_VIEW_ID = "AjaxCall";
    public static final String USER_SESSION_KEY = "MA4_user";

    @Override
    public void afterPhase(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        String rootId = event.getFacesContext().getViewRoot().getViewId();
        if (rootId.contains(AJAX_VIEW_ID)) {
            handleAjaxRequest(event);
        } else if (!userExists(context)) {
            // send the user to the login view
            if (requestingSecureView(context)) {
                context.getApplication().
                        getNavigationHandler().handleNavigation(context,
                                "*", "Exit");
            }
        }
    }

    @Override
    public void beforePhase(PhaseEvent event) {
    }

    @Override
    public PhaseId getPhaseId() {
        return PhaseId.RESTORE_VIEW;
    }

    // --------------------------------------------------------- Private Methods       
    /**
     * <p>
     * Determine if the user has been authenticated by checking the session for
     * an existing <code>Wuser</code> object.</p>
     *
     * @param context the <code>FacesContext</code> for the current request
     * @return <code>true</code> if the user has been authenticated, otherwise
     * <code>false</code>
     */
    private boolean userExists(FacesContext context) {
        ExternalContext extContext = context.getExternalContext();
        return (extContext.getSessionMap().containsKey(USER_SESSION_KEY));
    }

    /**
     * <p>
     * Determines if the requested view is one of the login pages which will
     * allow the user to access them without being authenticated.</p>
     *
     * <p>
     * Note, this implementation most likely will not work if the
     * <code>FacesServlet</code> is suffix mapped.</p>
     *
     * @param context the <code>FacesContext</code> for the current request
     * @return <code>true</code> if the requested view is allowed to be accessed
     * without being authenticated, otherwise <code>false</code>
     */
    private boolean requestingSecureView(FacesContext context) {
        ExternalContext extContext = context.getExternalContext();
        String path = extContext.getRequestPathInfo();
        if (path == null) {
            return false; //hone page?
        } else {
            return path.startsWith("/Secure");
        }
    }

    private void handleAjaxRequest(PhaseEvent event) {
        FacesContext context = event.getFacesContext();
        SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
        HttpServletResponse response = (HttpServletResponse) context.getExternalContext().getResponse();
        HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
        String funcName = (String) request.getParameter("function");
        String res = "";
        String prefix = sb.getUrlPrefix(); //
        
        if (funcName.equalsIgnoreCase("redraw")) {
            String analType = (String) request.getParameter("analType");
            double zoomPercent = Double.parseDouble(request.getParameter("zoompc"));
            double panXPercent = Double.parseDouble(request.getParameter("panXpc"));
            double panYPercent = Double.parseDouble(request.getParameter("panYpc"));
            int canvasDimX = Integer.parseInt(request.getParameter("width"));
            int canvasDimY = Integer.parseInt(request.getParameter("height"));
            //save the original value
            int canvasDimXo = canvasDimX;
            int canvasDimYo = canvasDimY;

            if (zoomPercent == 100) { //if no scaling needs to be done, pan is irrelevant
                panXPercent = 0;
                panYPercent = 0;
            } else {      //otherwise....
                panXPercent = ((100 - zoomPercent) * panXPercent) / (100 - zoomPercent);  //readjust X pan for larger image
                panYPercent = ((100 - zoomPercent) * panYPercent) / (100 - zoomPercent);  //readjust Y pan for larger image
            }

            canvasDimX = (int) ((zoomPercent / 100) * canvasDimX + 0.5);     //readjust canvas dim x based on scaling
            canvasDimY = (int) ((zoomPercent / 100) * canvasDimY + 0.5);   //readjust canvas dim y based on scaling

            String zoomImgName = "zoom" + current++ + ".png";

            boolean plotRes = false;
            res = "zoomImgURL=\'NA\'";
            RConnection RC = sb.getRConnection();
            if (analType.equals("integ")) {
                plotRes = RIntegUtils.drawKEGGGraph(RC, zoomImgName, canvasDimX, canvasDimY, zoomPercent);
            } else {
                plotRes = RGraphUtils.drawMetPAGraph(RC, zoomImgName, canvasDimX, canvasDimY);
            }

            if (plotRes) {
                String convertPath = RGraphUtils.getConvertFullPath(RC);
                if (convertPath.equals("NA")) {

                } else {
                    int cropright = (int) ((canvasDimX - canvasDimXo) * panXPercent / 100);
                    int croptop = (int) ((canvasDimY - canvasDimYo) * panYPercent / 100);
                    if (croptop < 0) {
                        int wd = 0 - croptop;
                        croptop = 0;
                        cropright = cropright + wd;
                    }
                    if (cropright < 0) {
                        int wd = 0 - cropright;
                        cropright = 0;
                        croptop = croptop + wd;
                    }
                    String zmImgPath = sb.getCurrentUser().getHomeDir() + File.separator + zoomImgName;
                    String cropedImgName = zoomImgName.replace("zoom", "crop");
                    String targetPath = sb.getCurrentUser().getHomeDir() + File.separator + cropedImgName;
                    boolean cropOK = DataUtils.cropImage(convertPath, zmImgPath, targetPath, cropright, croptop, canvasDimXo, canvasDimYo, 100);
                    if (cropOK) {
                        String zmImgURL = "zoomImgURL=\'" + prefix + "/resources/users" + File.separator
                                + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + cropedImgName + "\'";
                        res = zmImgURL;
                    }
                }
            }
        } else if (funcName.equalsIgnoreCase("plotPathway")) {
            String pathName = (String) request.getParameter("pathname");
            //  String cleanPathName = pathName.replace(" ", "_") + ".html";
            //sb.setCurrentPathway(pathName.replace(" ", "_"));
            String analType = (String) request.getParameter("analType");
            int canvasDimX = Integer.parseInt(request.getParameter("width"));
            int canvasDimY = Integer.parseInt(request.getParameter("height"));
            String nodeTipInfo = "";
            if (analType.equals("integ")) {
                nodeTipInfo = RIntegUtils.plotInmexPath(sb.getRConnection(), pathName, canvasDimX, canvasDimY);
            } else {
                nodeTipInfo = RGraphUtils.plotMetPAPath(sb, pathName, canvasDimX, canvasDimY);
            }
            
            String pathPrefix = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator;
            String imgURL = pathPrefix + pathName + ".png";

            String urlCode = "pathImgURL=\'" + imgURL + "\';";

            res = urlCode + "\n" + "pathPrefix=\'" + pathPrefix + "\';\n" + nodeTipInfo;
        } else if (funcName.equalsIgnoreCase("getImageMap")) {
            
            String leftImgURL = prefix + "/resources/users" + File.separator
                    + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + sb.getCurrentImage("path_view") + "dpi72.png";
            String urlCode = "leftImgURL=\"" + leftImgURL + "\"";

            String imgMapInfo = RGraphUtils.getOverviewImgMap(sb.getRConnection());
            res = urlCode + "\n" + imgMapInfo;
        } else if (funcName.equalsIgnoreCase("prepareMsetView")) {
            String id = (String) request.getParameter("id");
            MsetBean msb = (MsetBean) DataUtils.findBean("msetBean");
            msb.setMsetNm(id);
            res = "1";
        } else if (funcName.equalsIgnoreCase("getjobinfo")) {
            MnetLoadBean loadnetb = (MnetLoadBean) DataUtils.findBean("mnetLoader");
            MnetResBean mnetb = (MnetResBean) DataUtils.findBean("mnetResBean");
            String org = loadnetb.getIntegOrg();
            res = prefix +"/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + "||" + org;
            String viewMode = (String) request.getParameter("viewmode");
            if (viewMode.equals("net")) {
                res = res + "||" + mnetb.getVisMode() + "||NA" + "||" + RNetworkUtils.getNetsNamesString(sb.getRConnection());
            } else if (viewMode.equals("pathwaynet")) {
                res = res + "||" + mnetb.getVisMode() + "||NA" + "||null";
            } else if (viewMode.equals("venn")) {
                res = res + "||" + RDataUtils.getSelectedDataNumber(sb.getRConnection()) + "||" + RDataUtils.getSelectedDataNames(sb.getRConnection());
            }
        } else if (funcName.equalsIgnoreCase("getnetinfo")) {
            res = RNetworkUtils.getNetsNamesString(sb.getRConnection());
        } else if (funcName.equalsIgnoreCase("performCommunityDetection")) {
            String method = (String) request.getParameter("method");
            res = RNetworkUtils.detectCommunities(sb.getRConnection(), method);
        } else if (funcName.equalsIgnoreCase("getShortestPaths")) {
            String ndA = (String) request.getParameter("source");
            String ndB = (String) request.getParameter("target");
            res = RNetworkUtils.getShortestPaths(sb.getRConnection(), ndA, ndB);
        } else if (funcName.equalsIgnoreCase("updateNetworkLayout")) {
            String algo = (String) request.getParameter("layoutalgo");
            MnetResBean mnetb = (MnetResBean) DataUtils.findBean("mnetResBean");
            res = mnetb.updateNetworkLayout(algo);
        } else if (funcName.equalsIgnoreCase("performNodesFilter")) {
            String nodeIDs = request.getParameter("nodes");
            MnetResBean mnetb = (MnetResBean) DataUtils.findBean("mnetResBean");
            res = mnetb.performNodesFilter(nodeIDs);

        } else if (funcName.equalsIgnoreCase("networkEnrichment")) {
            String ids = (String) request.getParameter("IDs");
            String funDB = (String) request.getParameter("funDB");
            MnetResBean mnetb = (MnetResBean) DataUtils.findBean("mnetResBean");
            String vismode = mnetb.getVisMode();
            int myres;
            String nm = "metaboanalyst_enrichment_" + mnetb.getFileCount();
            if (funDB.equals("kegg")) {
                //Run enrichment analysis using KEGG genes library
                if (vismode.equals("gene_phenotypes")) {
                    myres = RNetworkUtils.doNetEnrichmentTest(sb.getRConnection(), nm, funDB, ids);
                } else {
                    //Run enrichment analysis using KEGG compounds-genes library
                    myres = mnetb.doIntegPathwayAnanlysis(ids, nm);
                }
            } else {
                myres = RNetworkUtils.doNetEnrichmentTest(sb.getRConnection(), nm, funDB, ids);
            }

            if (myres == 1) {
                res = nm + ".json";
            } else {
                res = "NA.json";
            }
        } else if (funcName.equalsIgnoreCase("exportNetwork")) {
            String format = request.getParameter("format");
            MnetResBean mnetb = (MnetResBean) DataUtils.findBean("mnetResBean");
            res = mnetb.exportNetwork(format);

        } else if (funcName.equalsIgnoreCase("extractModule")) {
            String nodeIDs = request.getParameter("nodeIDs");
            MnetResBean mnetb = (MnetResBean) DataUtils.findBean("mnetResBean");
            res = mnetb.extractModule(nodeIDs);

        } else if (funcName.equalsIgnoreCase("viewNetwork")) {
            String name = request.getParameter("name");
            PathBean pathb = (PathBean) DataUtils.findBean("pathBean");
            res = pathb.viewNetwork(name);            
        } else if (funcName.equalsIgnoreCase("download")) {
            sb.setupFileDownloadZip();
            res = prefix + "/resources/users/" + DataUtils.getJustFileName(sb.getCurrentUser().getHomeDir()) + File.separator + "/Download.zip";

        } else if (funcName.equalsIgnoreCase("getErrorMsg")) {
            res = RDataUtils.getCurrentMsg(sb.getRConnection());
        } else if (funcName.equalsIgnoreCase("prepareNetwork")) {
            String netName = (String) request.getParameter("netName");
            MnetResBean mnetb = (MnetResBean) DataUtils.findBean("mnetResBean");
            res = mnetb.loadNetwork(netName);
        } else if (funcName.equalsIgnoreCase("getcmpdinfo")) {
            String id = (String) request.getParameter("cid");
            res = RDataUtils.getCmpdInfo(sb.getRConnection(), id);
        } else if (funcName.equalsIgnoreCase("getVennGenes")) {
            String areas = request.getParameter("areas");
            res = RDataUtils.getVennGeneNames(sb.getRConnection(), areas);
        } else {
            //Nothing
        }

        //finally send back response
        response.setHeader("Cache-Control", "no-store, no-cache, must-revalidate");
        response.setDateHeader("Expires", 0);
        //Note: set plain text, not xml, since our response is basically free text & javascript code
        response.setContentType("text/plain");

        try {
            response.getWriter().write(res);
            context.responseComplete();
        } catch (Exception exception) {
            exception.printStackTrace();
        }
    }
}
