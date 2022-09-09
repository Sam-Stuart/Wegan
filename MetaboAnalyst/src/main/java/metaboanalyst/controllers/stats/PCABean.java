/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.stats;

import java.io.File;
import java.io.Serializable;
import java.util.Arrays;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.models.User;
import metaboanalyst.rwrappers.ChemoMetrics;
import metaboanalyst.rwrappers.OAUtils;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;
import org.primefaces.context.RequestContext;
import org.primefaces.model.chart.Axis;
import org.primefaces.model.chart.AxisType;
import org.primefaces.model.chart.LineChartModel;
import org.primefaces.model.chart.LineChartSeries;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "pcaBean")
public class PCABean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    private int pcaPairNum = 2;

    public int getPcaPairNum() {
        return pcaPairNum;
    }

    public void setPcaPairNum(int pcaPairNum) {
        this.pcaPairNum = pcaPairNum;
    }

    public SelectItem[] getPcaPCs() {
//        int pcNums = ChemoMetrics.GetMaxPCACompNumber(sb) - 2;
        int pcNums = ChemoMetrics.GetMaxPCACompNumber(sb);
        System.out.print("in PcaPCS... pcNums : ");
        System.out.print(pcNums);
        SelectItem[] items = new SelectItem[pcNums];
        for (int i = 0; i < pcNums; i++) {
            int pcNum = i + 2;
            items[i] = new SelectItem(pcNum, pcNum + "");
        }
        return items;
    }

    public SelectItem[] getPcaAllPCs() {
//        int pcNums = ChemoMetrics.GetMaxPCACompNumber(sb) - 1;
        int pcNums = ChemoMetrics.GetMaxPCACompNumber(sb);
        SelectItem[] items = new SelectItem[pcNums];
        for (int i = 0; i < pcNums; i++) {
            int pcNum = i + 1;
            items[i] = new SelectItem(pcNum, pcNum + "");
        }
        return items;
    }
        
    public void pcaPairBtn_action() {
        ChemoMetrics.InitPCA(sb, doOriginal);
        ChemoMetrics.PlotPCAPairSummary(sb, pcaPairsColorOpts, sb.getNewImage("pca_pair"), "png", 72, pcaPairNum);
    }

    private boolean doOriginal = false; 
    
    public boolean isDoOriginal() {
        return doOriginal;
    }
    
    public void setDoOriginal(boolean doOriginal) {
        this.doOriginal = doOriginal;
    }
    
    private boolean greyScale = false;

    public boolean isGreyScale() {
        return greyScale;
    }

    public void setGreyScale(boolean greyScale) {
        this.greyScale = greyScale;
    }

    private boolean diffShapes = true;

    public boolean isDiffShapes() {
        return diffShapes;
    }

    public void setDiffShapes(boolean diffShapes) {
        this.diffShapes = diffShapes;
    }

    private int pcaScreeNum = 2;

    public int getPcaScreeNum() {
        return pcaScreeNum;
    }

    public void setPcaScreeNum(int pcaScreeNum) {
        this.pcaScreeNum = pcaScreeNum;
    }

    public String pcaScreeBtn_action() {
        ChemoMetrics.InitPCA(sb, doOriginal);
        ChemoMetrics.PlotPCAScree(sb, sb.getNewImage("pca_scree"), "png", 72, pcaScreeNum);
        RequestContext.getCurrentInstance().scrollTo("ac:form2:screePane");
        return null;
    }

    private boolean displayNames = false;

    public boolean isDisplayNames() {
        return displayNames;
    }

    public void setDisplayNames(boolean displayNames) {
        this.displayNames = displayNames;
    }

    private boolean displayFeatNames = false;

    public boolean isDisplayFeatNames() {
        return displayFeatNames;
    }

    public void setDisplayFeatNames(boolean displayFeatNames) {
        this.displayFeatNames = displayFeatNames;
    }

    private boolean displayConfs = false;

    public boolean isDisplayConfs() {
        return displayConfs;
    }

    public void setDisplayConfs(boolean displayConfs) {
        this.displayConfs = displayConfs;
    }

    private int pcaScoreX = 1;
    private int pcaScoreY = 2;

    public int getPcaScoreX() {
        return pcaScoreX;
    }

    public void setPcaScoreX(int pcaScoreX) {
        this.pcaScoreX = pcaScoreX;
    }

    public int getPcaScoreY() {
        return pcaScoreY;
    }

    public void setPcaScoreY(int pcaScoreY) {
        this.pcaScoreY = pcaScoreY;
    }

    private boolean ellipseOpts = false; 
    
    public boolean isEllipseOpts() {
        return ellipseOpts;
    }
    
    public void setEllipseOpts(boolean ellipseOpts) {
        this.ellipseOpts = ellipseOpts;
    }
        
    
    private boolean var_arrowsOpts = false; 
    
    public boolean isVar_arrowsOpts() {
        return var_arrowsOpts;
    }
    
    public void setVar_arrowsOpts(boolean var_arrowsOpts) {
        this.var_arrowsOpts = var_arrowsOpts;
    }
    
    
        
    private boolean env_arrowsOpts = false; 
    
    public boolean isEnv_arrowsOpts() {
        return env_arrowsOpts;
    }
    
    public void setEnv_arrowsOpts(boolean env_arrowsOpts) {
        this.env_arrowsOpts = env_arrowsOpts;
    }
    
         
    private boolean env_centOpts = false; 
    
    public boolean isEnv_centOpts() {
        return env_centOpts;
    }
    
    public void setEnv_centOpts(boolean env_centOpts) {
        this.env_centOpts = env_centOpts;
    }
     
         
    private boolean sampleNamesOpts = false; 
    
    public boolean isSampleNamesOpts() {
        return sampleNamesOpts;
    }
    
    public void setSampleNamesOpts(boolean sampleNamesOpts) {
        this.sampleNamesOpts = sampleNamesOpts;
    }

    private boolean noPointsOpts = false; 
    
    public boolean isNoPointsOpts() {
        return noPointsOpts;
    }
    
    public void setNoPointsOpts(boolean noPointsOpts) {
        this.noPointsOpts = noPointsOpts;
    }
    
   private String pcaPairsColorOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.pcaPairsColorPaletteOpts
    
    public String getPcaPairsColorOpts() {
        return pcaPairsColorOpts;
    }

    public void setPcaPairsColorOpts(String pcaPairsColorOpts) {
        this.pcaPairsColorOpts = pcaPairsColorOpts;
    }
    
   private String ordColorOpts = "NULL"; //FUNCTION CORRESPONDS WITH applicationBean1.ordColorPaletteOpts
    
    public String getOrdColorOpts() {
        return ordColorOpts;
    }

    public void setOrdColorOpts(String ordColorOpts) {
        this.ordColorOpts = ordColorOpts;
    }    
    private SelectItem[] pcaMetaColumnOpts = null;
    
    public SelectItem[] getPcaMetaColumnOpts(){
        String[] columns = OAUtils.GetPCAMetaColumns(sb);
        int columnsLen = columns.length;
        pcaMetaColumnOpts = new SelectItem[columnsLen];
        List<String> columnNames = Arrays.asList(columns);
        for (int i = 0; i < columnsLen; i++) {
            pcaMetaColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
        }
        return pcaMetaColumnOpts;
    }
    
    private String pcaMetaColumnName = getPcaMetaColumnOpts()[0].getLabel();
    
    public String getPcaMetaColumnName() {
        return pcaMetaColumnName;
    }

    public void setPcaMetaColumnName(String pcaMetaColumnName) {
        this.pcaMetaColumnName = pcaMetaColumnName;
    }

    
    public String pcaScore2dBtn_action() {
        if (pcaScoreX == pcaScoreY) {
            sb.updateMsg("Error", "X and Y axes are of the same PC");
        } else {
            double conf = 0.95;
            if (!displayConfs) {
                conf = 0;
            }
            int showNames = 0;
            if (displayNames) {
                showNames = 1;
            }

            int useGreyScale = 0;
            if (greyScale) {
                useGreyScale = 1;
            }
            ChemoMetrics.InitPCA(sb, doOriginal);
            ChemoMetrics.PlotPCA2DScore(sb, ellipseOpts, var_arrowsOpts, env_arrowsOpts, env_centOpts, sampleNamesOpts, pcaPairsColorOpts, pcaMetaColumnName, noPointsOpts, sb.getNewImage("pca_score2d"), "png", 72);
            RequestContext.getCurrentInstance().scrollTo("ac:form3:score2dPane");
        }
        return null;
    }

    private int pcaScore3dX = 1;
    private int pcaScore3dY = 2;
    private int pcaScore3dZ = 3;
    private int rotationAngle = 40;

    public int getPcaScore3dX() {
        return pcaScore3dX;
    }

    public void setPcaScore3dX(int pcaScore3dX) {
        this.pcaScore3dX = pcaScore3dX;
    }

    public int getPcaScore3dY() {
        return pcaScore3dY;
    }

    public void setPcaScore3dY(int pcaScore3dY) {
        this.pcaScore3dY = pcaScore3dY;
    }

    public int getPcaScore3dZ() {
        return pcaScore3dZ;
    }

    public void setPcaScore3dZ(int pcaScore3dZ) {
        this.pcaScore3dZ = pcaScore3dZ;
    }

    public int getRotationAngle() {
        return rotationAngle;
    }

    public void setRotationAngle(int rotationAngle) {
        this.rotationAngle = rotationAngle;
    }

    public String pcaScore3dBtn_action() {
        if (pcaScore3dX == pcaScore3dY || pcaScore3dX == pcaScore3dZ || pcaScore3dY == pcaScore3dZ) {
            sb.updateMsg("Error", "Detected the same PC on two axes!");
        } else {
            //ChemoMetrics.PlotPCA3DScore(sb, sb.getNewImage("pca_score3d"), "png", 72, pcaScore3dX, pcaScore3dY, pcaScore3dZ, rotationAngle);
            ChemoMetrics.InitPCA(sb, doOriginal);
            ChemoMetrics.PlotPCA3DScore(sb, sb.getNewImage("pca_score3d"), "json", 72, pcaScore3dX, pcaScore3dY, pcaScore3dZ);
            //  RequestContext.getCurrentInstance().scrollTo("ac:form4:score3dPane");
        }
        return null;
    }

    private int pcaLoadX = 1;
    private int pcaLoadY = 2;

    public int getPcaLoadX() {
        return pcaLoadX;
    }

    public void setPcaLoadX(int pcaLoadX) {
        this.pcaLoadX = pcaLoadX;
    }

    public int getPcaLoadY() {
        return pcaLoadY;
    }

    public void setPcaLoadY(int pcaLoadY) {
        this.pcaLoadY = pcaLoadY;
    }

    private String loadPlotOpt = "scatter";

    public String getLoadPlotOpt() {
        return loadPlotOpt;
    }

    public void setLoadPlotOpt(String loadPlotOpt) {
        this.loadPlotOpt = loadPlotOpt;
    }

    public void pcaLoadBtn_action() {
        if (pcaLoadX == pcaLoadY) {
            sb.updateMsg("Error", "Detected the same PC on two axes!");
        } else {
            ChemoMetrics.InitPCA(sb, doOriginal);
            ChemoMetrics.PlotPCALoading(sb, sb.getNewImage("pca_loading"), "png", 72, pcaLoadX, pcaLoadY, loadPlotOpt, displayFeatNames ? 1 : 0);
            updateLoadModel();
        }
    }

    private int pcaBiplotX = 1;
    private int pcaBiplotY = 2;

    public int getPcaBiplotX() {
        return pcaBiplotX;
    }

    public void setPcaBiplotX(int pcaBiplotX) {
        this.pcaBiplotX = pcaBiplotX;
    }

    public int getPcaBiplotY() {
        return pcaBiplotY;
    }

    public void setPcaBiplotY(int pcaBiplotY) {
        this.pcaBiplotY = pcaBiplotY;
    }

    public String pcaBiplotBtn_action() {
        if (pcaBiplotX == pcaBiplotY) {
            sb.updateMsg("Error", "Detected the same PC on two axes!");
        } else {
            ChemoMetrics.InitPCA(sb, doOriginal);
            ChemoMetrics.PlotPCABiplot(sb, sb.getNewImage("pca_biplot"), "png", 72, pcaBiplotX, pcaBiplotY);
            RequestContext.getCurrentInstance().scrollTo("ac:form6:biplotPane");
        }
        return null;
    }

    /*
     Methods for interactive scatter plotting 
     */
    private double loadMinX, loadMaxX, loadMinY, loadMaxY;

    public double getLoadMinX() {
        return loadMinX;
    }

    public void setLoadMinX(double loadMinX) {
        this.loadMinX = loadMinX;
    }

    public double getLoadMaxX() {
        return loadMaxX;
    }

    public void setLoadMaxX(double loadMaxX) {
        this.loadMaxX = loadMaxX;
    }

    public double getLoadMinY() {
        return loadMinY;
    }

    public void setLoadMinY(double loadMinY) {
        this.loadMinY = loadMinY;
    }

    public double getLoadMaxY() {
        return loadMaxY;
    }

    public void setLoadMaxY(double loadMaxY) {
        this.loadMaxY = loadMaxY;
    }

    public LineChartModel getLdModel() {
        if (ldModel == null) {
            updateLoadModel();
        }
        return ldModel;
    }

    public void setLdModel(LineChartModel ldModel) {
        this.ldModel = ldModel;
    }

    public void setLoadMaxX(int loadMaxX) {
        this.loadMaxX = loadMaxX;
    }

    private LineChartModel ldModel;

    public void updateLoadModel() {
        RConnection RC = sb.getRConnection();
        ldModel = new LineChartModel();
        LineChartSeries dots = new LineChartSeries();
        double[][] mat = RDataUtils.getPCALoadMat(RC); //must be called first
        for (double[] row : mat) {
            dots.set(row[0], row[1]);
        }

        String[] nodeIDs = RDataUtils.getPCALoadCmpds(RC);
        sb.setFeatureLabels(nodeIDs);
        double[] cmpdNodeIndex = RDataUtils.getPCALoadCmpdInxs(RC);
        sb.setFeatureInx(cmpdNodeIndex);

        double[] rgx = RDataUtils.getPCALoadAxesSpec(RC);
        loadMinX = rgx[0];
        loadMaxX = rgx[1];
        loadMinY = rgx[2];
        loadMaxY = rgx[3];

        dots.setShowLine(false);
        dots.setMarkerStyle("filledCircle', size:'9.0");
        ldModel.addSeries(dots);

        ldModel.setZoom(true);
        ldModel.setExtender("ext");
        Axis xAxis = ldModel.getAxis(AxisType.X);
        xAxis.setLabel("Loadings " + pcaLoadX);
        xAxis.setMin(loadMinX);
        xAxis.setMax(loadMaxX);

        Axis yAxis = ldModel.getAxis(AxisType.Y);
        yAxis.setLabel("Loadings " + pcaLoadY);
        yAxis.setMin(loadMinY);
        yAxis.setMax(loadMaxY);

    }

    public void flipPCA() {
//        ChemoMetrics.FlipPCA(sb, flipOpt);
        ChemoMetrics.PlotPCAPairSummary(sb, "NULL", sb.getNewImage("pca_pair"), "png", 72, 2);
        ChemoMetrics.PlotPCAScree(sb, sb.getNewImage("pca_scree"), "png", 72, 2);
        ChemoMetrics.PlotPCA2DScore(sb, false, false, false, false, false, "NULL", "NULL", false, sb.getCurrentImage("pca_score2d"), "png", 72);
        ChemoMetrics.PlotPCALoading(sb, sb.getNewImage("pca_loading"), "png", 72, 1, 2, "scatter", 1);  // setLoadingTable(pcImpInx);
        ChemoMetrics.PlotPCABiplot(sb, sb.getNewImage("pca_biplot"), "png", 72, 1, 2);
        // ChemoMetrics.PlotPCA3DScore(sb, sb.getCurrentImage("pca_score3d"), "png", 72, 1, 2, 3, 40);
        ChemoMetrics.PlotPCA3DScore(sb, sb.getNewImage("pca_score3d"), "json", 72, 1, 2, 3);
    }
    
    private String flipOpt="y";

    public String getFlipOpt() {
        return flipOpt;
    }

    public void setFlipOpt(String flipOtp) {
        this.flipOpt = flipOtp;
    }
    
    
    //TABLES FOR DOWNLOAD
    private User usr = sb.getCurrentUser();
    private String usrName = usr.getName();

    private String file2DSampleScores = "pca_sample_scores.csv";
    private String file2DSampleScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file2DSampleScores + "\">" + file2DSampleScores + "</a>";
 
    public String getFile2DSampleScoresPath() {
        return file2DSampleScoresPath;
    }
        
    public void setFile2DSampleScoresPath(String file2DSampleScoresPath) {
        this.file2DSampleScoresPath = file2DSampleScoresPath;
    } 
    
    
    private String file2DColScores = "pca_variable_scores.csv";
    private String file2DColScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file2DColScores + "\">" + file2DColScores + "</a>";
 
    public String getFile2DColScoresPath() {
        return file2DColScoresPath;
    }
        
    public void setFile2DColScoresPath(String file2DColScoresPath) {
        this.file2DColScoresPath = file2DColScoresPath;
    } 
    
    
    private String file2DEnvScores = "pca_constraining_variable_scores.csv"; //TABLE SHOULD ONLY POP UP IF ENVDATA IS UPLOADED
    private String file2DEnvScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file2DEnvScores + "\">" + file2DEnvScores + "</a>";
 
    public String getFile2DEnvScoresPath() {
        return file2DEnvScoresPath;
    }
        
    public void setFile2DEnvScoresPath(String file2DEnvScoresPath) {
        this.file2DEnvScoresPath = file2DEnvScoresPath;
    } 
    
    
    private String file3DSampleScores = "pca_3D_sample_scores.csv";
    private String file3DSampleScoresPath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + file3DSampleScores + "\">" + file3DSampleScores + "</a>";
 
    public String getFile3DSampleScoresPath() {
        return file3DSampleScoresPath;
    }
        
    public void setFile3DSampleScoresPath(String file3DSampleScoresPath) {
        this.file3DSampleScoresPath = file3DSampleScoresPath;
    } 
        
    
    private String fileScree = "pca_scree_data.csv";
    private String fileScreePath = "<a target='_blank' href = \"/MetaboAnalyst/resources/users/" + usrName + File.separator + fileScree + "\">" + fileScree + "</a>";
 
    public String getFileScreePath() {
        return fileScreePath;
    }
        
    public void setFileScreePath(String fileScreePath) {
        this.fileScreePath = fileScreePath;
    } 
 
}