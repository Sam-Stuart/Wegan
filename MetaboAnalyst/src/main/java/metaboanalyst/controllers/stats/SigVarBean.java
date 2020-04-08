/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.SigVarSelect;
import metaboanalyst.utils.DataUtils;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "sigBean")
public class SigVarBean implements Serializable {

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private boolean nonParSAM = false;

    public boolean isNonParSAM() {
        return nonParSAM;
    }

    public void setNonParSAM(boolean nonParSAM) {
        this.nonParSAM = nonParSAM;
    }

    private String pairedAnal = "FALSE";

    public String getPairedAnal() {
        return pairedAnal;
    }

    public void setPairedAnal(String pairedAnal) {
        this.pairedAnal = pairedAnal;
    }

    private String equalVar = "TRUE";

    public String getEqualVar() {
        return equalVar;
    }

    public void setEqualVar(String equalVar) {
        this.equalVar = equalVar;
    }

    private double delta;
    private double deltaMin = 0;
    private double deltaMax = 0;
    private double step = 0;

    public double getDeltaMin() {
        if (step == 0) {
            setupDelta();
        }
        return deltaMin;
    }

    public double getDeltaMax() {
        if (step == 0) {
            setupDelta();
        }
        return deltaMax;
    }

    public double getStep() {
        if (step == 0) {
            setupDelta();
        }
        return step;
    }

    private void setupDelta() {
        double[] deltas = SigVarSelect.getSAMDeltaRange(sb);
        deltaMin = deltas[0];
        deltaMax = deltas[1];
        step = deltas[2];
    }

    public double getDelta() {
        return SigVarSelect.GetSAMSuggestedDelta(sb);
    }

    public void setDelta(double delta) {
        this.delta = delta;
    }

    public void samBtn1_action() {
        String stat = "d.stat";
        if (!sb.isMultiGroup() && nonParSAM) {
            stat = "wilc.stat";
        }
        SigVarSelect.InitSAM(sb, stat, pairedAnal, equalVar);
        setupDelta();

        SigVarSelect.PlotSAM_FDR(sb, delta, sb.getNewImage("sam_view"), "png", 72);
        SigVarSelect.PlotSAM_Cmpd(sb, sb.getNewImage("sam_imp"), "png", 72, delta);
        sb.updateMsg("OK", "The result is updated!");
    }

    private double alpha;

    public double getAlpha() {
        return SigVarSelect.GetEBAMSuggestedA0(sb);
    }

    public void setAlpha(double alpha) {
        this.alpha = alpha;
    }

    private boolean nonParEBAM = false;

    public boolean isNonParEBAM() {
        return nonParEBAM;
    }

    public void setNonParEBAM(boolean nonParEBAM) {
        this.nonParEBAM = nonParEBAM;
    }

    private double ebamDelta = 0.9;

    public double getEbamDelta() {
        return ebamDelta;
    }

    public void setEbamDelta(double ebamDelta) {
        this.ebamDelta = ebamDelta;
    }

    public void ebamBtn_action() {
        String stat = "z.ebam";
        if (!sb.isMultiGroup() && nonParEBAM) {
            stat = "wilc.ebam";
        }
        SigVarSelect.InitEBAM_Cmpd(sb, stat, alpha, pairedAnal, equalVar);
        SigVarSelect.PlotEBAM_Cmpd(sb, sb.getNewImage("ebam_imp"), "png", 72, ebamDelta);
        sb.updateMsg("OK", "The <b>Result view</b> is updated!");
    }
}
