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
public class MummiBean implements Serializable {

    private String setName;
    private String keggLink;
    private int setNum; //theory total
    private int refNum; //measure total
    private int hitNum;
    private double easeP;
    private double fisherP;
    private double gammaP;

    public String getMatch(){
        return (hitNum + "/" + setNum);
    }

        public int getRefNum() {
        return refNum;
    }

    public void setRefNum(int refNum) {
        this.refNum = refNum;
    }
    
    public double getEaseP() {
        return easeP;
    }

    public void setEaseP(double easeP) {
        this.easeP = easeP;
    }

    public double getFisherP() {
        return fisherP;
    }

    public void setFisherP(double fisherP) {
        this.fisherP = fisherP;
    }

    public double getGammaP() {
        return gammaP;
    }

    public void setGammaP(double gammaP) {
        this.gammaP = gammaP;
    }

    public MummiBean(String setName, String keggLink, int setNum, int refNum, int hitNum,
            double easeP, double hyperP, double gammaP) {
        this.setName = setName;
        this.keggLink = keggLink;
        this.refNum = refNum;
        this.setNum = setNum;
        this.hitNum = hitNum;
        this.easeP = easeP;
        this.fisherP = hyperP;
        this.gammaP = gammaP;
    }


    public String getKeggLink() {
        return keggLink;
    }

    public void setKeggLink(String keggLink) {
        this.keggLink = keggLink;
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

    public String getSetName() {
        return setName;
    }
}
