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

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "clusterBean")
public class ClusterBean implements Serializable{

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

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
}
