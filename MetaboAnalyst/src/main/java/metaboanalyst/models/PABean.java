/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.models;

import java.io.Serializable;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author Jeff
 */
public class PABean implements Serializable {

    private String setName;
    private String keggLink;
    private String smpdbLink;
    private int setNum;
    private double expNum;
    private int hitNum;
    private double pVal;
    private double logP;

    public String getMatch(){
        return (hitNum + "/" + setNum);
    }
    
    public double getLogP() {
        return logP;
    }

    public void setLogP(double logP) {
        this.logP = logP;
    }
    private double holmPval;

    public double getHolmPval() {
        return holmPval;
    }

    public void setHolmPval(double holmPval) {
        this.holmPval = holmPval;
    }
    private double fdrPval;
    private double impVal; // importance measure

    public double getImpVal() {
        return impVal;
    }

    public void setImpVal(double impVal) {
        this.impVal = impVal;
    }

    public double getFdrPval() {
        return fdrPval;
    }

    public void setFdrPval(double fdrPval) {
        this.fdrPval = fdrPval;
    }

    public PABean(String setName, String keggLink, String smpdbLink, int setNum, double expNum, int hitNum,
            double pVal, double logP, double bP, double fdrP, double impVal) {
        this.setName = setName;
        this.keggLink = keggLink;
        this.smpdbLink = smpdbLink;
        this.setNum = setNum;
        this.expNum = expNum;
        this.hitNum = hitNum;
        this.pVal = pVal;
        this.logP = logP;
        this.holmPval = bP;
        this.fdrPval = fdrP;
        this.impVal = impVal;
    }

    public double getExpNum() {
        return expNum;
    }

    public void setExpNum(int expNum) {
        this.expNum = expNum;
    }

    public String getKeggLink() {
        return keggLink;
    }

    public void setKeggLink(String keggLink) {
        this.keggLink = keggLink;
    }

    public String getSmpdbLink() {
        return smpdbLink;
    }

    public void setSmpdbLink(String smpdLink) {
        this.smpdbLink = smpdLink;
    }

    public int getHitNum() {
        return hitNum;
    }

    public void setHitNum(int hitNum) {
        this.hitNum = hitNum;
    }

    public int getSetNum() {
        return setNum;
    }

    public void setSetNum(int setNum) {
        this.setNum = setNum;
    }

    public double getPVal() {
        return pVal;
    }

    public String getSetName() {
        return setName;
    }

    public String getDetails(){
        return keggLink+"\n" + smpdbLink;
    }
}
