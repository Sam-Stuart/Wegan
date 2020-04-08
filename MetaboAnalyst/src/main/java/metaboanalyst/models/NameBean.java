/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.models;

import java.io.Serializable;

/**
 *
 * @author Jeff
 */
public class NameBean implements Serializable{

    private String name;

    public NameBean() {
    }

    public NameBean(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
    
    private double prob;

    public double getProb() {
        return prob;
    }

    public void setProb(double prob) {
        this.prob = prob;
    }

    public String getCls() {
        return cls;
    }

    public void setCls(String cls) {
        this.cls = cls;
    }
    private String cls;
}
