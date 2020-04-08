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
public class FeatureBean implements Serializable {

    private String name;
    private int uniqID; //unique as rowKey
    private double val1;
    private double val2;
    private double val3;
    private double val4;
    private double val5;
    private double val6;
    private double val7;
    private double val8;
    private String extra;
    private int count = 0;

    public FeatureBean() {

    }

    public void addValue(double val) {
        switch (count) {
            case 0:
                val1 = val;
                count++;
                break;
            case 1:
                val2 = val;
                count++;
                break;
            case 2:
                val3 = val;
                count++;
                break;
            case 3:
                val4 = val;
                count++;
                break;
            case 4:
                val5 = val;
                count++;
                break;
            case 5:
                val6 = val;
                count++;
                break;
            case 6:
                val7 = val;
                count++;
                break;
            default:
                val8 = val;
                break;
        }
    }

    public int getUniqID() {
        return uniqID;
    }

    public void setUniqID(int uniqID) {
        this.uniqID = uniqID;
    }

    public void addName(String nm) {
        name = nm;
    }

    public void addExtra(String val) {
        extra = val;
    }

    public String getExtra() {
        return extra;
    }

    public String getName() {
        return name;
    }

    public double getVal1() {
        return val1;
    }

    public double getVal2() {
        return val2;
    }

    public double getVal3() {
        return val3;
    }

    public double getVal4() {
        return val4;
    }

    public double getVal5() {
        return val5;
    }

    public double getVal6() {
        return val6;
    }

    public double getVal7() {
        return val7;
    }

    public double getVal8() {
        return val8;
    }
    
}
