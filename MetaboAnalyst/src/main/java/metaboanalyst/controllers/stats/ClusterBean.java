/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.File;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.Clustering;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import java.util.Arrays;
import java.util.List;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.DownloadBean;
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.OAUtils;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "clusterBean")
public class ClusterBean implements Serializable{

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();

    private String clustMethodOpt;
    private String clustDistOpt;
    private String scaleOpt = "row";

    public String getScaleOpt() {
        return scaleOpt;
    }

    public void setScaleOpt(String scaleOpt) {
        this.scaleOpt = scaleOpt;
    }
    
    private String dataOpt = "norm";

    public String getDataOpt() {
        return dataOpt;
    }

    public void setDataOpt(String dataOpt) {
        this.dataOpt = dataOpt;
    }
    
    public String getClustMethodOpt() {
        return clustMethodOpt;
    }

    public void setClustMethodOpt(String clustMethodOpt) {
        this.clustMethodOpt = clustMethodOpt;
    }

    public String getClustDistOpt() {
        return clustDistOpt;
    }

    public void setClustDistOpt(String clustDistOpt) {
        this.clustDistOpt = clustDistOpt;
    }

    private String viewOpt = "overview";

    public String getViewOpt() {
        return viewOpt;
    }

    public void setViewOpt(String viewOpt) {
        this.viewOpt = viewOpt;
    }

    public String treeButton_action() {
        String imgName = sb.getNewImage("tree");
        Clustering.PlotClustTree(sb, imgName, "png", 72, clustDistOpt, clustMethodOpt);
        RequestContext.getCurrentInstance().scrollTo("form1:treePane");
        return null;
    }

    private String hmDistOpt;
    private String hmMethodOpt;
    private String hmColorOpt;
    private boolean noReorg = false;
    private boolean useTopFeature = false;
    private int topThresh = 25;
    private String selectMethodOpt;
    private String noOrgOpt;

    public String getSelectMethodOpt() {
        return selectMethodOpt;
    }

    public void setSelectMethodOpt(String selectMethod) {
        this.selectMethodOpt = selectMethod;
    }

    public String getNoOrgOpt() {
        return noOrgOpt;
    }

    public void setNoOrgOpt(String noOrgOpt) {
        this.noOrgOpt = noOrgOpt;
    }

    public int getTopThresh() {
        return topThresh;
    }

    public void setTopThresh(int topThresh) {
        this.topThresh = topThresh;
    }

    public boolean isNoReorg() {
        return noReorg;
    }

    public void setNoReorg(boolean noReorg) {
        this.noReorg = noReorg;
    }

    public boolean isUseTopFeature() {
        return useTopFeature;
    }

    public void setUseTopFeature(boolean useTopFeature) {
        this.useTopFeature = useTopFeature;
    }

    public String getHmDistOpt() {
        return hmDistOpt;
    }

    public void setHmDistOpt(String hmDistOpt) {
        this.hmDistOpt = hmDistOpt;
    }

    public String getHmMethodOpt() {
        return hmMethodOpt;
    }

    public void setHmMethodOpt(String hmMethodOpt) {
        this.hmMethodOpt = hmMethodOpt;
    }

    public String getHmColorOpt() {
        return hmColorOpt;
    }

    public void setHmColorOpt(String hmColorOpt) {
        this.hmColorOpt = hmColorOpt;
    }

    public String hmButton_action() {

        String rowV = "T";
        String colV = "T";

        if (noReorg) {
            if (noOrgOpt.equalsIgnoreCase("row")) {
                rowV = "F";
            } else if (noOrgOpt.equalsIgnoreCase("col")) {
                colV = "F";
            } else {
                rowV = "F";
                colV = "F";
            }
        }

        if (viewOpt.equals("detail")) {
            if (useTopFeature && topThresh > 2000) {
                sb.updateMsg("Warning", "Too many features (over 2000) - reset to 2000.");
                topThresh = 2000;
            } else if (RDataUtils.getNormFeatureNumber(sb.getRConnection()) > 2000) {
                viewOpt = "overview";
                sb.updateMsg("Warning", "Too many features (over 2000) - reset to overview.");
            }
        }
        
        if (useTopFeature) {
            Clustering.PlotSubHeatMap(sb, sb.getNewImage("heatmap"), "png", 72, dataOpt, scaleOpt, hmDistOpt, hmMethodOpt, hmColorOpt, selectMethodOpt, topThresh, viewOpt, rowV, colV, (drawBorders) ? "T" : "F", (grpAves) ? "T" : "F");
        } else {
            Clustering.PlotHeatMap(sb, sb.getNewImage("heatmap"), "png", 72, dataOpt, scaleOpt, hmDistOpt, hmMethodOpt, hmColorOpt, viewOpt, rowV, colV, (drawBorders) ? "T" : "F", (grpAves) ? "T" : "F");
        }
        RequestContext.getCurrentInstance().scrollTo("form1:hmPane");
        return null;
    }

    private int kmClustNm = 3;

    public int getKmClustNm() {
        return kmClustNm;
    }

    public void setKmClustNm(int kmClustNm) {
        this.kmClustNm = kmClustNm;
    }

    public String kmButton_action() {
        Clustering.PlotKmeans(sb, sb.getNewImage("km"), "png", 72, kmClustNm);
        RequestContext.getCurrentInstance().scrollTo("form1:kmPane");
        return null;
    }

    private boolean drawBorders = true;

    public boolean isDrawBorders() {
        return drawBorders;
    }

    public void setDrawBorders(boolean drawBorders) {
        this.drawBorders = drawBorders;
    }

    private boolean grpAves = false;

    public boolean isGrpAves() {
        return grpAves;
    }

    public void setGrpAves(boolean grpAves) {
        this.grpAves = grpAves;
    }
    
    
    private int somXdim = 1;
    private int somYdim = 3;
    private String somInitOpt = "linear";
    private String somNbOpt = "gaussian";

    public String getSomInitOpt() {
        return somInitOpt;
    }

    public void setSomInitOpt(String somInitOpt) {
        this.somInitOpt = somInitOpt;
    }

    public String getSomNbOpt() {
        return somNbOpt;
    }

    public void setSomNbOpt(String somNbOpt) {
        this.somNbOpt = somNbOpt;
    }

    public int getSomXdim() {
        return somXdim;
    }

    public void setSomXdim(int somXdim) {
        this.somXdim = somXdim;
    }

    public int getSomYdim() {
        return somYdim;
    }

    public void setSomYdim(int somYdim) {
        this.somYdim = somYdim;
    }

    public String somButton_action() {
        Clustering.PlotSOM(sb, sb.getNewImage("som"), "png", 72, somXdim, somYdim, somInitOpt, somNbOpt);
        RequestContext.getCurrentInstance().scrollTo("form1:somPane");
        return null;
    }

    public String getSomImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("som") + "dpi72.png";
    }

    //private String kmTxt = "";
    public String getKmTxt() {
        int clustNum = Clustering.GetKMClusterNumber(sb);
        String str = "<h2>K-means clustering details: </h2><br/>";
        str = str + "<table width=\"500\", border=\"1\" cellpadding=\"5\">";
        str = str + "<tr><th width=\"80\">Cluster </th><th> Members </th></tr>";
        for (int i = 0; i < clustNum; i++) {
            String names = Clustering.GetKMClusterMembers(sb, i + 1); //java start by 0, while R by 1
            str = str + "<tr><td>" + "Cluster " + (i + 1) + "</td><td>" + names + "</td></tr>";
        }
        str = str + "</table>";
        return str;
    }

    public String getSomTxt() {
        int xord = Clustering.GetSOMXdimension(sb);
        int yord = Clustering.GetSOMYdimension(sb);
        String str = "<h2>SOM clustering details: </h2><br/>";
        str = str + "<table width=\"500\", border=\"1\" cellpadding=\"5\">";
        str = str + "<tr><th width=\"80\">Cluster </th><th> Members </th></tr>";
        for (int i = 0; i < xord; i++) {
            for (int j = 0; j < yord; j++) {
                String names = Clustering.GetSOMClusterMembers(sb, i, j);
                str = str + "<tr><td>" + "Cluster (" + i + ", " + j + ")</td><td>" + names + "</td></tr>";
            }
        }
        str = str + "</table>";
        return str;
    }
    
    private boolean doOriginal = false; 
    
    public boolean isdoOriginal() {
        return doOriginal;
    }
    
    public void setdoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
     
   // check box 
    private boolean doEle = false;
    
    public boolean isdoEle() {
        return doEle;
    }

    public void setdoEle(boolean doEle) {
        this.doEle = doEle;
    }
    
    private boolean doPolygon = false;
    
    public boolean isdoPolygon() {
        return doPolygon;
    }

    public void setdoPolygon(boolean doPolygon) {
        this.doPolygon = doPolygon;
    }
    
    private boolean doPath = false;
    
    public boolean isdoPath() {
        return doPath;
    }

    public void setdoPath(boolean doPath) {
        this.doPath = doPath;
    }
    
    
    private boolean doDatum = false;
    
    public boolean isdoDatum() {
        return doDatum;
    }

    public void setdoDatum(boolean doDatum) {
        this.doDatum = doDatum;
    }
    
    
    private boolean doProj = false;
    
    public boolean isdoProj() {
        return doProj;
    }

    public void setdoProj(boolean doProj) {
        this.doProj = doProj;
    }
    
    
    private boolean doUni_point = false;
    
    public boolean isdoUni_point() {
        return doUni_point;
    }

    public void setdoUni_point(boolean doUni_point) {
        this.doUni_point = doUni_point;
    }
    
    
    // textbox 
    private String zoom = "";
    
    public String getZoom() {
        return zoom;
    }

    public void setZoom(String zoom) {
        this.zoom = zoom;
    }
    
    private String rangeA = "";
    
    public String getRangeA() {
        return rangeA;
    }

    public void setRangeA(String rangeA) {
        this.rangeA = rangeA;
    }
     
    private String crs_txt = "";
    
    public String getCrs_txt() {
        return crs_txt;
    }

    public void setCrs_txt(String crs_txt) {
        this.crs_txt = crs_txt;
    }
    
    private String point_size = "";
    
    public String getPoint_size() {
        return point_size;
    }

    public void setPoint_size(String point_size) {
        this.point_size = point_size;
    }
    
    private String path_size = "";
    
    public String getPath_size() {
        return path_size;
    }

    public void setPath_size(String path_size) {
        this.path_size = path_size;
    }
        
    // static dropdown
    private SelectItem[] pointColOpts = null;
    
    public SelectItem[] getPointColOpts(){
        String[] columns = Clustering.colorColumn(sb);
        int columnsLen = columns.length;
        pointColOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            pointColOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return pointColOpts;
    }
    
    private String pointColName = getPointColOpts()[0].getLabel();
    
    public String getPointColName() {
        return pointColName;
    }

    public void setPointColName(String pointColName) {
        this.pointColName = pointColName;
    }
    
    
    private SelectItem[] polygonColOpts = null;
    
    public SelectItem[] getPolygonColOpts(){
        String[] columns = Clustering.varColumn(sb);
        int columnsLen = columns.length;
        polygonColOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            polygonColOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return polygonColOpts;
    }
    
    private String polygonColName = getPolygonColOpts()[0].getLabel();
    
    public String getPolygonColName() {
        return polygonColName;
    }

    public void setPolygonColName(String polygonColName) {
        this.polygonColName = polygonColName;
    }
    
    
    private SelectItem[] pathColOpts = null;
    
    public SelectItem[] getPathColOpts(){
        String[] columns = Clustering.lineColumn(sb);
        int columnsLen = columns.length;
        pathColOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            pathColOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return pathColOpts;
    }
    
    private String pathColName = getPathColOpts()[0].getLabel();
    
    public String getPathColName() {
        return pathColName;
    }

    public void setPathColName(String pathColName) {
        this.pathColName = pathColName;
    }
    
    
    private final SelectItem[] source;
    private String sourcechosen = "NULL";
    
    public SelectItem[] getSource() {
        return source;
    }
    
    public String getSourcechosen() {
        return sourcechosen;
    } 

    public void setSourcechosen(String sourcechosen) {
        this.sourcechosen = sourcechosen;
    }
        
    
    private final SelectItem[] maptype;
    private String maptypechosen = "NULL";
    
    public SelectItem[] getMaptype() {
        return maptype;
    }
    
    public String getMaptypechosen() {
        return maptypechosen;
    } 

    public void setMaptypechosen(String maptypechosen) {
        this.maptypechosen = maptypechosen;
    }
    
    
    private final SelectItem[] crs_option;
    private String crs_optionchosen = "NULL";
    
    public SelectItem[] getCrs_option() {
        return crs_option;
    }
    
    public String getCrs_optionchosen() {
        return crs_optionchosen;
    } 

    public void setCrs_optionchosen(String crs_optionchosen) {
        this.crs_optionchosen = crs_optionchosen;
    }
    
    
    private final SelectItem[] border_col;
    private String border_colchosen = "NULL";
    
    public SelectItem[] getBorder_col() {
        return border_col;
    }
    
    public String getBorder_colchosen() {
        return border_colchosen;
    } 

    public void setBorder_colchosen(String border_colchosen) {
        this.border_colchosen = border_colchosen;
    }
    
    
    private final SelectItem[] color_point;
    private String color_pointchosen = "NULL";
    
    public SelectItem[] getColor_point() {
        return color_point;
    }
    
    public String getColor_pointchosen() {
        return color_pointchosen;
    } 

    public void setColor_pointchosen(String color_pointchosen) {
        this.color_pointchosen = color_pointchosen;
    }

    
    private String fileeleresult = "Elevation.csv";
    private String fileeleresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileeleresult + "\">" + fileeleresult + "</a>";
    //removed "usrname" cause it is not defined in clutering bean?
    public String getFileeleresultpath() {
        return fileeleresultpath;
    }

    public void setFileeleresultpath(String fileeleresultpath) {
        this.fileeleresultpath = fileeleresultpath;
    }
    
    public ClusterBean() {
        source = new SelectItem[2];
        source[0] = new SelectItem("NULL", "Stamen");
        source[1] = new SelectItem("google", "Google");
        
        crs_option = new SelectItem[2];
        crs_option[0] = new SelectItem("NULL", "Not require conversion");
        crs_option[1] = new SelectItem("10TM", "10TM");
        
        border_col = new SelectItem[6];
        border_col[0] = new SelectItem("NULL", "Skyblue");
        border_col[1] = new SelectItem("green", "Green");
        border_col[2] = new SelectItem("turquoise", "Turquoise");
        border_col[3] = new SelectItem("steelblue", "Steelblue");
        border_col[4] = new SelectItem("peach", "Peach");
        border_col[5] = new SelectItem("wheat", "Wheat");
        
        color_point = new SelectItem[6];
        color_point[0] = new SelectItem("NULL", "Blue");
        color_point[1] = new SelectItem("green", "Green");
        color_point[2] = new SelectItem("turquoise", "Turquoise");
        color_point[3] = new SelectItem("steelblue", "Steelblue");
        color_point[4] = new SelectItem("peach", "Peach");
        color_point[5] = new SelectItem("wheat", "Wheat");
        
        maptype = new SelectItem[16];
        maptype[0] = new SelectItem("NULL", "Terrain");
        maptype[1] = new SelectItem("terrain-background", "Terrain-background");
        maptype[2] = new SelectItem("satellite", "Satellite");
        maptype[3] = new SelectItem("roadmap", "Roadmap");
        maptype[4] = new SelectItem("hybrid", "Hybrid");
        maptype[5] = new SelectItem("toner", "Toner");
        maptype[6] = new SelectItem("watercolor", "Watercolor");
        maptype[7] = new SelectItem("terrain-labels", "Terrain-labels");
        maptype[8] = new SelectItem("terrain-lines", "Terrain-lines");
        maptype[9] = new SelectItem("toner-2010", "Toner-2010");
        maptype[10] = new SelectItem("toner-2011", "Toner-2011");
        maptype[11] = new SelectItem("toner-background", "Toner-background");
        maptype[12] = new SelectItem("toner-hybrid", "Toner-hybrid");
        maptype[13] = new SelectItem("toner-labels", "Toner-labels");
        maptype[14] = new SelectItem("toner-lines", "Toner-lines");
        maptype[15] = new SelectItem("toner-lite", "Toner-lite");

    }
    
    
    // ACTION BUTTON // 
    public void spatialvisUpdate_action() {
        Clustering.CreateSpatialvis(sb, doOriginal, doDatum, doProj, crs_txt, crs_optionchosen, sourcechosen, maptypechosen, zoom, rangeA,  doEle, pointColName, polygonColName,
                pathColName, point_size, path_size, border_colchosen, color_pointchosen, doUni_point, sb.getNewImage("ggmap"), "png", 72, "false"); 
    }
}
