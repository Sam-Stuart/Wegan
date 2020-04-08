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
public class SampleBean implements Serializable{

    private String name;
    private String group;
    private double adjust =1.0; //sample specific normalization

    public SampleBean() {
    }

    public SampleBean(String name, String group) {
        this.name = name;
        this.group = group;
    }
    public SampleBean(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public String getGroup() {
        return group;
    }
    
    public void setAdjust(double adjust){
        if(adjust==0){
            adjust = 0.0001; //cannot be zero
        }
        this.adjust = adjust;
    }

    public double getAdjust(){
        return adjust;
    }
}
