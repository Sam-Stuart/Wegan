/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.models;

/**
 *
 * @author xia
 */
public class RcmdBean {
    
    private int step;
    private String cmd;

    public RcmdBean (int step, String cmd){
        this.step = step;
        this.cmd = cmd;
    }
    
    public int getStep() {
        return step;
    }

    public void setStep(int step) {
        this.step = step;
    }

    public String getCmd() {
        return cmd;
    }

    public void setCmd(String cmd) {
        this.cmd = cmd;
    }
    
}
