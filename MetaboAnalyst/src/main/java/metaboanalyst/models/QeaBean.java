/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.models;

import java.io.Serializable;
import java.text.DecimalFormat;

/**
 *
 * @author Jeff
 */
public class QeaBean implements Serializable {

    private String setName;
    private int setNum;
    private int hitNum;
    private double statQ;
    private double expQ;
    private double pVal;
    private double bonPval;
    private double fdrPval;
    DecimalFormat sci5Formater;
    private String style;

    public String getStyle() {
        return style;
    }

    public void setStyle(String style) {
        this.style = style;
    }
    
    public QeaBean(String setName, String style, int setNum, int hitNum, double statQ, double expQ, double pVal, double bonP, double fdrPval) {
        this.setName = setName;
        this.style=style;
        this.setNum = setNum;
        this.hitNum = hitNum;
        this.statQ = statQ;
        this.expQ = expQ;
        this.pVal = pVal;
        this.bonPval = bonP;
        this.fdrPval = fdrPval;
        sci5Formater = new DecimalFormat("0.####E0");

    }

    public int getHitNum() {
        return hitNum;
    }

    public String getSetName() {
        return setName;
    }

    public int getSetNum() {
        return setNum;
    }

    public String getExpQ() {
        if (expQ < 0.00001) {
            return sci5Formater.format(expQ);
        }
        return expQ + "";
    }

    public String getPVal() {
        if (pVal < 0.00001) {
            return sci5Formater.format(pVal);
        }
        return pVal + "";
    }

    public String getStatQ() {
        if (statQ < 0.00001) {
            return sci5Formater.format(statQ);
        }
        return statQ + "";
    }

    public String getBonPval() {
        if (bonPval < 0.00001) {
            return sci5Formater.format(bonPval);
        }
        return bonPval + "";
    }

    public String getFdrPval() {
        if (fdrPval < 0.00001) {
            return sci5Formater.format(fdrPval);
        }
        return fdrPval + "";
    }
}
