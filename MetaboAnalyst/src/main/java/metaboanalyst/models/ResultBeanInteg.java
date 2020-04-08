/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.models;

import java.io.Serializable;

/**
 * Results for individual feature Note, maximal 10 data sets, corresponding to
 * ten cols
 *
 * @author Jeff
 */
public class ResultBeanInteg implements Serializable {

    //this ID is for string only for sorting, not embedded hyperlink
    private String ID; //note, hashmap can also have a ID key => hyperlinked gene
    private String name;
    private int hit;
    private int setSize;
    private double pval;
    private double expected;

    public double getExpected() {
        return expected;
    }

    public void setExpected(double expected) {
        this.expected = expected;
    }

    public double getPval() {
        return pval;
    }

    public void setPval(double pval) {
        this.pval = pval;
    }
    private double topoVal;

    public int getHit() {
        return hit;
    }

    public void setHit(int hit) {
        this.hit = hit;
    }

    public double getTopoVal() {
        return topoVal;
    }

    public void setTopoVal(double topoVal) {
        this.topoVal = topoVal;
    }

    public String getID() {
        return ID;
    }

    public void setID(String ID) {
        this.ID = ID;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getSetSize() {
        return setSize;
    }

    public void setSetSize(int setSize) {
        this.setSize = setSize;
    }

    public String getMatch() {
        return (hit + "/" + setSize);
    }

}
