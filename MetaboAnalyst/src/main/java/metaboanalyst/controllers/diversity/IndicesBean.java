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
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.DiversityUtils;
import metaboanalyst.utils.DataUtils;

/**
 *
 * @author violet
 */
@ManagedBean(name="indicesBean") //
public class IndicesBean implements Serializable {
    
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
   
    
    // textbox 
//    private String group = "";
//    
//    public String getGroup() {
//        return group;
//    }
//
//    public void setGroup(String group) {
//        this.group = group;
//    }
    
    private SelectItem[] groupColOpts = null;
    
    public SelectItem[] getGroupColOpts(){
        String[] columns = DiversityUtils.IndiceColumn(sb);
        int columnsLen = columns.length;
        groupColOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            groupColOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return groupColOpts;
    }
    
    private String groupColName = getGroupColOpts()[0].getLabel();
    
    public String getGroupColName() {
        return groupColName;
    }

    public void setGroupColName(String groupColName) {
        this.groupColName = groupColName;
    }

    
    // static dropdown
    private final SelectItem[] margin; // in the view, need to present the options //application bean 
    private String marginchosen = "NULL";
            
    public SelectItem[] getMargin() {
        return margin;
    }
    
    public String getMarginchosen() {
        return marginchosen;
    } 

    public void setMarginchosen(String marginchosen) {
        this.marginchosen = marginchosen;
    }
       
    
    private final SelectItem[] color;
    private String colorchosen = "NULL";
    
    public SelectItem[] getColor() {
        return color;
    }
    
    public String getColorchosen() {
        return colorchosen;
    } 

    public void setColorchosen(String colorchosen) {
        this.colorchosen = colorchosen;
    }
        
    
    private final SelectItem[] colorb;
    private String colorbchosen = "NULL";
    
    public SelectItem[] getColorb() {
        return colorb;
    }
    
    public String getColorbchosen() {
        return colorbchosen;
    } 

    public void setColorbchosen(String colorbchosen) {
        this.colorbchosen = colorbchosen;
    }

    
    private String fileshannonresult = "Shannon diversity indices.csv";
    private String fileshannonresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileshannonresult + "\">" + fileshannonresult + "</a>";
    
    public String getFileshannonresultpath() {
        return fileshannonresultpath;
    }

    public void setFileshannonresultpath(String fileshannonresultpath) {
        this.fileshannonresultpath = fileshannonresultpath;
    }
    
    private String filesimpsonresult = "Simpson diversity indices.csv";
    private String filesimpsonresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filesimpsonresult + "\">" + filesimpsonresult + "</a>";
    
    public String getFilesimpsonresultpath() {
        return filesimpsonresultpath;
    }

    public void setFilesimpsonresultpath(String filesimpsonresultpath) {
        this.filesimpsonresultpath = filesimpsonresultpath;
    }
    
    private String fileinvsimpsonresult = "Invsimpson diversity indices.csv";
    private String fileinvsimpsonresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileinvsimpsonresult + "\">" + fileinvsimpsonresult + "</a>";
    
    public String getFileinvsimpsonresultpath() {
        return fileinvsimpsonresultpath;
    }

    public void setFileinvsimpsonresultpath(String fileinvsimpsonresultpath) {
        this.fileinvsimpsonresultpath = fileinvsimpsonresultpath;
    }
    
    private String filefisherresult = "Fisher alpha diversity indices.csv";
    private String filefisherresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filefisherresult + "\">" + filefisherresult + "</a>";
    
    public String getFilefisherresultpath() {
        return filefisherresultpath;
    }

    public void setFilefisherresultpath(String filefisherresultpath) {
        this.filefisherresultpath = filefisherresultpath;
    }
    
    private String filerichallresult = "Species richness all sites included.csv";
    private String filerichallresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filerichallresult + "\">" + filerichallresult + "</a>";
    
    public String getFilerichallresultpath() {
        return filerichallresultpath;
    }

    public void setFilerichallresultpath(String filerichallresultpath) {
        this.filerichallresultpath = filerichallresultpath;
    }
    
    private String filerichresult = "Species richness by selected group.csv";
    private String filerichresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filerichresult + "\">" + filerichresult + "</a>";
    
    public String getFilerichresultpath() {
        return filerichresultpath;
    }

    public void setFilerichresultpath(String filerichresultpath) {
        this.filerichresultpath = filerichresultpath;
    }
    
    private String fileevsharesult = "Species Evenness_shannon indices.csv";
    private String fileevsharesultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileevsharesult + "\">" + fileevsharesult + "</a>";
    
    public String getFileevsharesultpath() {
        return fileevsharesultpath;
    }

    public void setFileevsharesultpath(String fileevsharesultpath) {
        this.fileevsharesultpath = fileevsharesultpath;
    }
    
    private String fileevsimresult = "Species Evenness_simpson indices.csv";
    private String fileevsimresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileevsimresult + "\">" + fileevsimresult + "</a>";
    
    public String getFileevsimresultpath() {
        return fileevsimresultpath;
    }

    public void setFileevsimresultpath(String fileevsimresultpath) {
        this.fileevsimresultpath = fileevsimresultpath;
    }
    
    private String fileevinvresult = "Species Evenness_invshannon indices.csv";
    private String fileevinvresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileevinvresult + "\">" + fileevinvresult + "</a>";
    
    public String getFileevinvresultpath() {
        return fileevinvresultpath;
    }

    public void setFileevinvresultpath(String fileevinvresultpath) {
        this.fileevinvresultpath = fileevinvresultpath;
    }
    
    private String filealpharesult = "Alpha diversity.csv";
    private String filealpharesultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filealpharesult + "\">" + filealpharesult + "</a>";
    
    public String getFilealpharesultpath() {
        return filealpharesultpath;
    }

    public void setFilealpharesultpath(String filealpharesultpath) {
        this.filealpharesultpath = filealpharesultpath;
    }
    
    private String filebetaresult = "Beta diversity.csv";
    private String filebetaresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filebetaresult + "\">" + filebetaresult + "</a>";
    
    public String getFilebetaresultpath() {
        return filebetaresultpath;
    }

    public void setFilebetaresultpath(String filebetaresultpath) {
        this.filebetaresultpath = filebetaresultpath;
    }
    
    private String filegammaresult = "Gamma diversity.csv";
    private String filegammaresultpath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + filegammaresult + "\">" + filegammaresult + "</a>";
    
    public String getFilegammaresultpath() {
        return filegammaresultpath;
    }

    public void setFilegammaresultpath(String filegammaresultpath) {
        this.filegammaresultpath = filegammaresultpath;
    }  
    
   
    public IndicesBean() {      
        margin = new SelectItem[2];
        margin[0] = new SelectItem("NULL", "1");
        margin[1] = new SelectItem("2", "2");
        
        color = new SelectItem[6];
        color[0] = new SelectItem("NULL", "Skyblue");
        color[1] = new SelectItem("gray", "Gray");
        color[2] = new SelectItem("turquoise", "Turquoise");
        color[3] = new SelectItem("slateblue", "Slateblue");
        color[4] = new SelectItem("seagreen", "Seagreen");
        color[5] = new SelectItem("wheat", "Wheat");
        
        colorb = new SelectItem[6];
        colorb[0] = new SelectItem("NULL", "Skyblue");
        colorb[1] = new SelectItem("gray", "Gray");
        colorb[2] = new SelectItem("turquoise", "Turquoise");
        colorb[3] = new SelectItem("slateblue", "Slateblue");
        colorb[4] = new SelectItem("seagreen", "Seagreen");
        colorb[5] = new SelectItem("wheat", "Wheat");
    }
    
    
    // ACTION BUTTON // 
    public void indicesAlphaUpdate_action() {
        DiversityUtils.CreateIndicesDiv(sb, doOriginal, groupColName, marginchosen);       
        DiversityUtils.PlotAlphaDiversity(sb, colorchosen, sb.getNewImage("Alpha_Plot"), "png", 72, "false");
    }
    
    public void indicesBetaUpdate_action() {
        DiversityUtils.CreateIndicesDiv(sb, doOriginal, groupColName, marginchosen);               
        DiversityUtils.PlotBetaDiversity(sb, colorbchosen, sb.getNewImage("Beta_Plot"), "png", 72, "false");
    }
    
}