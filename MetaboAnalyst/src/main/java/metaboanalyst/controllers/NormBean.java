/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers;

import metaboanalyst.utils.DataUtils;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.stats.RocAnalBean;
import metaboanalyst.models.SampleBean;
import metaboanalyst.rwrappers.RDataUtils;
import org.rosuda.REngine.Rserve.RConnection;

@ManagedBean(name = "normBean")
@ViewScoped
public class NormBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    private String rowNormOpt = "NULL";
    private String transNormOpt = "NULL";
    private String scaleNormOpt = "NULL";
    private String refSmpl = null;
    private String refGrp = null;
    private String refVar = null;

    public void initPrenormData(){
        RDataUtils.initPrenormData(sb.getRConnection());
    }
    
    public String getRefVar() {
        return refVar;
    }

    public void setRefVar(String refVar) {
        this.refVar = refVar;
        rowNormOpt = "CompNorm";
    }

    public String getRefSmpl() {
        return refSmpl;
    }

    public void setRefSmpl(String refSmpl) {
        this.refSmpl = refSmpl;
        rowNormOpt = "SamplePQN";
    }

    public String getRefGrp() {
        return refGrp;
    }

    public void setRefGrp(String refGrp) {
        this.refGrp = refGrp;
        rowNormOpt = "GroupPQN";
    }

    public String getRowNormOpt() {
        return rowNormOpt;
    }

    public void setRowNormOpt(String rowNormOpt) {
        this.rowNormOpt = rowNormOpt;
    }

    public String getTransNormOpt() {
        return transNormOpt;
    }

    public void setTransNormOpt(String transNormOpt) {
        this.transNormOpt = transNormOpt;
    }

    public String getScaleNormOpt() {
        return scaleNormOpt;
    }

    public void setScaleNormOpt(String scaleNormOpt) {
        this.scaleNormOpt = scaleNormOpt;
    }

    private boolean specNormSpecifed = false;

    public void setSmplSpecNorm() {
        rowNormOpt = "SpecNorm";
        RDataUtils.setSampleNormFactor(sb.getRConnection(), sampleBeans);
        specNormSpecifed = true;
    }

    private List<SampleBean> sampleBeans = null;

    public List<SampleBean> getSampleBeans() {
        if (sampleBeans == null) {
            sampleBeans = RDataUtils.createSampleBeans(sb.getRConnection(), false);
        }
        return sampleBeans;
    }

    private boolean normPerformed = false;

    public boolean isNormPerformed() {
        return normPerformed;
    }

    ///LOUISA ADDED THIS START!!!!!!!!!!!!!!
    private String predictText = "NULL";
        
    public String getPredictText() {
        return predictText;
    }

    public void setPredictText(String predictText) {
        this.predictText = predictText;
    } 
    
    private SelectItem[] assumptionCol = null;
    
    public SelectItem[] getAssupCol(){
        String[] columns = RDataUtils.AssupColumn(sb);
        int columnsLen = columns.length;
        assumptionCol = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            assumptionCol[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return assumptionCol;
    }
    
    private String assumptionColName = getAssupCol()[0].getLabel();
    
    public String getAssumptionColName() {
        return assumptionColName;
    }

    public void setAssumptionColName(String assumptionColName) {
        this.assumptionColName = assumptionColName;
    }
    ///LOUISA ADDED THIS END!!!!!!!!!!!!!!

    
    public void performDataNormalization() {

        //String specNorm = smplSpecNorm ? "T" : "NULL";
        if (rowNormOpt.equals("SpecNorm") && !specNormSpecifed) {
            sb.updateMsg("Error", "You need to manually specify sample specific norm factors!");
            return;
        }
        if (rowNormOpt.equals("CompNorm") && refVar == null) {
            sb.updateMsg("Error", "You need to manually specify a reference variable for normalization");
            return;
        }
        if (rowNormOpt.equals("GroupPQN") && refGrp == null) {
            sb.updateMsg("Error", "You need to manually specify a reference group for normalization");
            return;
        }
        if (rowNormOpt.equals("SamplePQN") && refSmpl == null) {
            sb.updateMsg("Error", "You need to manually specify a reference sample for normalization");
            return;
        }

        RConnection RC = sb.getRConnection();
        String ref = "NA";
        switch (rowNormOpt) {
            case "CompNorm":
                ref = refVar;
                break;
            case "SamplePQN":
                ref = refSmpl;
                break;
            default:
                ref = refGrp;
                break;
        }
        int res = RDataUtils.normalizeData(RC, rowNormOpt, transNormOpt, scaleNormOpt, ref, includeRatio, ratioNumOpt);

        if (res > 0) {
            //plot the new image
            RDataUtils.plotNormSummaryGraph(sb, sb.getNewImage("norm"), "png", 72);
            RDataUtils.plotSampleNormSummaryGraph(sb, sb.getNewImage("snorm"), "png", 72);
            ///LOUISA ADDED THIS START!!!!!!!!!!!!!!
            RDataUtils.shapiroTest(RC);
            RDataUtils.leveneTest(RC, "NULL");
            RDataUtils.ResidualPlot(sb, "NULL", "NULL", sb.getNewImage("resid"), "png", 72);
            RDataUtils.Density_plot(sb, sb.getNewImage("residDen"), "png", 72);
            RDataUtils.Residual_fitPlot(sb, sb.getNewImage("residFit"), "png", 72);
            RDataUtils.QQ_plot(sb, sb.getNewImage("qq"), "png", 72);
            RDataUtils.AssupColumn(sb);
            ///LOUISA ADDED THIS END!!!!!!!!!!!!!!
            //now reset all data analysis to default
            sb.setDataNormed(true);
            sb.resetAnalysis();
            if(sb.getAnalType().equals("roc")){
                RocAnalBean rocAnalBean = (RocAnalBean) DataUtils.findBean("rocAnalBean");
                rocAnalBean.resetData();
            }
            sb.updateMsg("OK", "You can click <b>View Result</b> button to view the effect, or the <b>Proceed</b> button to move to the analysis page!");
            normPerformed = true;
        } else {
            sb.updateMsg("Error", "Unknown error happened during data normalization process!");
        }
    }

    private boolean includeRatio = false;

    public boolean isIncludeRatio() {
        return includeRatio;
    }

    public void setIncludeRatio(boolean includeRatio) {
        this.includeRatio = includeRatio;
    }

    private int ratioNumOpt = 20;

    public int getRatioNumOpt() {
        return ratioNumOpt;
    }

    public void setRatioNumOpt(int ratioNumOpt) {
        this.ratioNumOpt = ratioNumOpt;
    }

    public void setGrpNmOpts(SelectItem[] grpNmOpts) {
        this.grpNmOpts = grpNmOpts;
    }

    private SelectItem[] varNmOpts = null;

    public SelectItem[] getVarNmOpts() {
        if (varNmOpts == null) {
            setupVarNmOpts();
        }
        return varNmOpts;
    }

    private SelectItem[] smplNmOpts = null;

    public SelectItem[] getSmplNmOpts() {
        if (smplNmOpts == null) {
            setupSmplNmOpts();
        }
        return smplNmOpts;
    }

    private void setupVarNmOpts() {
        String[] varNms = RDataUtils.getPrenormFeatureNames(sb.getRConnection());
        int colLen = varNms.length;
        varNmOpts = new SelectItem[colLen];
        for (int i = 0; i < colLen; i++) {
            varNmOpts[i] = new SelectItem(varNms[i], varNms[i]);
        }
    }

    private void setupSmplNmOpts() {
        String[] smplNms = RDataUtils.getPrenormSampleNames(sb.getRConnection());
        int rowLen = smplNms.length;
        smplNmOpts = new SelectItem[rowLen];
        for (int i = 0; i < rowLen; i++) {
            smplNmOpts[i] = new SelectItem(smplNms[i], smplNms[i]);
        }
    }

    public SelectItem[] getGrpNmOpts() {
        if (grpNmOpts == null) {
            setupGrpNmOpts();
        }
        return grpNmOpts;
    }

    private SelectItem[] grpNmOpts = null;

    private void setupGrpNmOpts() {
        if (!sb.isRegresion()) {
            String[] grpNms = RDataUtils.getPrenormGroupNames(sb.getRConnection());
            int grpLen = grpNms.length;
            grpNmOpts = new SelectItem[grpLen];
            for (int i = 0; i < grpLen; i++) {
                grpNmOpts[i] = new SelectItem(grpNms[i], grpNms[i]);
            }
        } else {
            grpNmOpts = new SelectItem[]{new SelectItem("NULL", "<Not set>")};
        }
    }

    
    
    public void performAutoNormalization() {
        RConnection RC = sb.getRConnection();
//        String[] buttons = RDataUtils.AutoNormOptions(RC);
        String res = RDataUtils.autoNormalize(RC);
        String msg = RDataUtils.getCurrentMsg(RC);
        if (res == null) {
            sb.updateMsg("Error", "Unknown error happened during data normalization process!");
        } else {
            rowNormOpt = res;
            sb.updateMsg("OK", "Click the <b>Submit</b> button to implement the suggested normalization method!");
        }            
    }
 
    
    ///LOUISA ADDED THIS!!!!!!!!!!!!!!
    public void leveneUpdate_action() {
        RConnection RC = sb.getRConnection();
        RDataUtils.leveneTest(RC, predictText);
    }
    public void ResidPlotUpdate_action() {
        RDataUtils.ResidualPlot(sb, assumptionColName, predictText, sb.getNewImage("resid"), "png", 72);    
    }
}
