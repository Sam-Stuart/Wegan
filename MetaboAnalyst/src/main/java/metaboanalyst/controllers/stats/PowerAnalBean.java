/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.File;
import java.io.Serializable;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.ApplicationBean1;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.PowerUtils;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.model.chart.Axis;
import org.primefaces.model.chart.AxisType;
import org.primefaces.model.chart.ChartSeries;
import org.primefaces.model.chart.LineChartModel;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "powerAnalBean")
@SessionScoped
public class PowerAnalBean implements Serializable{

    private final ApplicationBean1 ab = (ApplicationBean1) DataUtils.findBean("applicationBean1");
    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    private LineChartModel lineModel;

    public LineChartModel getLineModel() {
        return lineModel;
    }

    public void setLineModel(LineChartModel lineModel) {
        this.lineModel = lineModel;
    }

    private double fdr = 0.1;

    public double getFdr() {
        return fdr;
    }

    public void setFdr(double fdr) {
        this.fdr = fdr;
    }

    private int smplSize = 200;

    public int getSmplSize() {
        return smplSize;
    }

    public void setSmplSize(int smplSize) {
        if (smplSize < 60) {
            smplSize = 60;
        } else if (smplSize > 1000) {
            smplSize = 1000;
        }

        this.smplSize = smplSize;
    }

    private String selectedContrasts = "NA";

    public String getSelectedContrasts() {
        return selectedContrasts;
    }

    public void setSelectedContrasts(String selectedContrasts) {
        this.selectedContrasts = selectedContrasts;
    }

    private SelectItem[] grpContrasts;

    public SelectItem[] getGroupContrastOpts() {
        if (grpContrasts == null) {
            String[] nms = RDataUtils.getGroupNames(sb.getRConnection());
            int totNum = nms.length * (nms.length - 1) / 2;
            grpContrasts = new SelectItem[totNum];
            int pos = 0;
            for (int m = 0; m < nms.length - 1; m++) {
                String grpA = nms[m];
                for (int n = m + 1; n < nms.length; n++) {
                    String grpB = nms[n];
                    String target = grpA + " vs. " + grpB;
                    grpContrasts[pos] = new SelectItem(target, target);
                    pos++;
                }
            }
        }
        return grpContrasts;
    }

    public void doDefaultPowerAnal() {

        paramBtn_action();
        sb.registerPage("Set parameter");
    }

    public void paramBtn_action() {
        PowerUtils.initPowerAnal(sb.getRConnection(), selectedContrasts);
        PowerUtils.plotPowerStatDiagnostics(sb, sb.getNewImage("power_stat"), "png", 72);
    }

    public String getStatImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("power_stat") + "dpi72.png";
    }

    public String getEffectSizeImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("power_effecsize") + "dpi72.png";
    }

    public String getPowerProfileImg() {
        return ab.getRootContext() + sb.getCurrentUser().getRelativeDir() + File.separator + sb.getCurrentImage("power_profile") + "dpi72.png";
    }

    public String profileBtn_action() {
        //PowerUtils.plotPowerProfile(sb, fdr, sb.getNewImage("power_profile"), "png", 72);
        fdr = 0.1;
        updateModel();
        return "powerview";
    }

    public void updateModel() {
        double fdrOld = fdr;
        fdr = PowerUtils.performPowerProfile(sb.getRConnection(), fdr, smplSize);
        if (fdrOld > fdr) {
            sb.updateMsg("Warning","FDR level has been re-adjusted in order to get meaningful result.");
        }
        
        lineModel = new LineChartModel();
        ChartSeries dots = new ChartSeries();

        double[] pwrs = PowerUtils.plotPowerProfile(sb, fdr, smplSize, sb.getNewImage("power_profile"), "png", 72);
        //double[] pwrs = PowerUtils.getPowerValueY(sb.getRConnection(), fdr);
        int[] smplNum = PowerUtils.getPowerValueX(sb.getRConnection()); //must be called after plotPowerProile
        for (int i = 0; i < pwrs.length; i++) {
            dots.set(smplNum[i], pwrs[i]);
        }
        lineModel.addSeries(dots);
        lineModel.setZoom(true);
        lineModel.setExtender("ext");
        Axis xAxis = lineModel.getAxis(AxisType.X);
        xAxis.setLabel("Sample size (per group) ");
        xAxis.setMin(0);

        Axis yAxis = lineModel.getAxis(AxisType.Y);
        yAxis.setLabel("Predicted Power");
        yAxis.setMin(0.00);
        yAxis.setMax(1.00);
    }
}
