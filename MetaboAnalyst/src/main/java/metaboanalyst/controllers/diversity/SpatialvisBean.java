/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.diversity;

import metaboanalyst.controllers.diversity.*;
import java.io.File;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.DownloadBean;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.Clustering;
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.DiversityUtils;
import metaboanalyst.utils.DataUtils;

/**
 *
 * @author violet
 */
@ManagedBean(name="spatialvisBean") //
public class SpatialvisBean implements Serializable {
    
   private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
   private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
   
   private User usr = sb.getCurrentUser();
   private String usrName = usr.getName();
   
   //public static final String PROP_SAMPLE_PROPERTY = "sampleProperty";
  // check box
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
    
        
    // static dropdown
    private SelectItem[] colorColOpts = null;
    
    public SelectItem[] getColorColOpts(){
        String[] columns = DiversityUtils.colorColumn(sb);
        int columnsLen = columns.length;
        colorColOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            colorColOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return colorColOpts;
    }
    
    private String colorColName = getColorColOpts()[0].getLabel();
    
    public String getColorColName() {
        return colorColName;
    }

    public void setColorColName(String colorColName) {
        this.colorColName = colorColName;
    }
    
    
    private SelectItem[] varColOpts = null;
    
    public SelectItem[] getVarColOpts(){
        String[] columns = DiversityUtils.varColumn(sb);
        int columnsLen = columns.length;
        varColOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            varColOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return varColOpts;
    }
    
    private String varColName = getVarColOpts()[0].getLabel();
    
    public String getVarColName() {
        return varColName;
    }

    public void setVarColName(String varColName) {
        this.varColName = varColName;
    }
    
    
    private SelectItem[] lineColOpts = null;
    
    public SelectItem[] getLineColOpts(){
        String[] columns = DiversityUtils.lineColumn(sb);
        int columnsLen = columns.length;
        lineColOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            lineColOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return lineColOpts;
    }
    
    private String lineColName = getLineColOpts()[0].getLabel();
    
    public String getLineColName() {
        return lineColName;
    }

    public void setLineColName(String lineColName) {
        this.lineColName = lineColName;
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

    
    private String fileeleresult = "Elevation.csv";
    private String fileeleresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileeleresult + "\">" + fileeleresult + "</a>";
    //removed "usrname" cause it is not defined in clutering bean?
    public String getFileeleresultpath() {
        return fileeleresultpath;
    }

    public void setFileeleresultpath(String fileeleresultpath) {
        this.fileeleresultpath = fileeleresultpath;
    }
    
    public SpatialvisBean() {
        source = new SelectItem[2];
        source[0] = new SelectItem("NULL", "Stamen");
        source[1] = new SelectItem("google", "Google");
        
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
        DiversityUtils.CreateSpatialvis(sb, doOriginal, sourcechosen, maptypechosen, zoom, varColName, rangeA, colorColName, doEle, 
                lineColName, doPolygon, doPath, sb.getNewImage("ggmap"), "png", 72, "false"); 
        //DiversityUtils.PlotSpatialmap(sb, doPolygon, doPath, sb.getNewImage("ggmap"), "png", 72, "false");
    }
    
}