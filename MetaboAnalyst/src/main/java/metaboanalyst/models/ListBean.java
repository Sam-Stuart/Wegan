/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.models;

import java.io.Serializable;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;

/**
 *
 * @author jianguox
 */
public class ListBean implements Serializable {

    private String name;
    private String geneList = "";
    private String org;
    private String geneIDType;
    private String[] seedList;
    private String rawFileNm = "";
    private int queryNum;
    private int uniqueGeneNum;
    private int seedNum;
    private boolean mapped = false;

    //whether this is the current one (to display on next page)
    private boolean current = true; 
    
    //this is onyl for chord diagram
    private boolean include = true;
    

    public ListBean() {
    }

    public int getQueryNum() {
        return queryNum;
    }

    public void setQueryNum(int queryNum) {
        this.queryNum = queryNum;
    }

    public int getUniqueGeneNum() {
        return uniqueGeneNum;
    }

    public void setUniqueGeneNum(int uiqueGeneNum) {
        this.uniqueGeneNum = uiqueGeneNum;
    }

    public int getSeedNum() {
        return seedNum;
    }

    public void setSeedNum(int seedNum) {
        this.seedNum = seedNum;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getRawFileNm() {
        return rawFileNm;
    }

    public void setRawFileNm(String rawFileNm) {
        this.rawFileNm = rawFileNm;
    }

    public String getGeneList() {
        return geneList;
    }

    public void setGeneList(String geneList) {
        this.geneList = geneList;
    }

    public String getOrg() {
        return org;
    }

    public void setOrg(String org) {
        this.org = org;
    }

    public String getGeneIDType() {
        return geneIDType;
    }

    public void setGeneIDType(String geneIDType) {
        this.geneIDType = geneIDType;
    }

    public String[] getSeedList() {
        return seedList;
    }

    public void setSeedList(String[] seedList) {
        this.seedList = seedList;
    }

    public boolean isMapped() {
        return mapped;
    }

    public void setMapped(boolean mapped) {
        this.mapped = mapped;
    }

    public String getCurrentStatus() {
        if (mapped & seedList.length > 1) {
            return "Ready";
        } else {
            return "Error";
        }
    }

    public boolean isInclude() {
        return include;
    }

    public void setInclude(boolean include) {
        if (this.include != include) {
            String msg = "Dataset: " + name + " is included";
            if (!include) {
                msg = "Dataset: " + name + " is excluded";
            }
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_INFO, "OK", msg));
            this.include = include;
        }
    }
    
    public boolean isCurrent() {
        return current;
    }

    public void setCurrent(boolean current) {
        this.current = current;
    }
}