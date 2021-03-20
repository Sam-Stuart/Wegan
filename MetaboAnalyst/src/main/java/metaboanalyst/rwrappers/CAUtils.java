/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

import javax.faces.model.SelectItem;
import metaboanalyst.rwrappers.RDataUtils;
import org.primefaces.model.chart.Axis;
import org.primefaces.model.chart.AxisType;
import org.primefaces.model.chart.LineChartModel;
import org.primefaces.model.chart.LineChartSeries;
import org.rosuda.REngine.REXPMismatchException;

/**
 *
 * @author Jeff
 */
public class CAUtils {

    public static boolean CreateLinearModel(SessionBean1 sb, String facA, String facB) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "lin.reg.anal.one(NA" + ", \"" + facA + "\", \"" + facB + "\", weights=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    public static boolean PlotLinearCA(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "plot.linReg1(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            //sb.addGraphicsCMD("corr_linear", rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }

    public static void CreatePenalizedModel(SessionBean1 sb, String method, String facA, Boolean weights) {
        System.out.println(method);
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "pen.reg.anal(NA" + ", \"" + method + "\", \"" + facA + "\", weights=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            //sb.addGraphicsCMD("corr_penalized", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotPenalizedCA(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "plot.pred.penReg(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_penalized", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
 
    public static void PlotPenalizedCVCA(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "plot.cv.penReg(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_penalized2", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void CreatePolynomialModel(SessionBean1 sb, String facA, String facB) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "poly.reg.anal(NA" + ", \"" + facA + "\", \"" + facB + "\", weights=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            //sb.addGraphicsCMD("corr_penalized", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }   
    
    public static boolean PlotPolynomialCA(SessionBean1 sb, String degree, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "plot.polyReg(NA, degree=NULL" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD(imgName, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }

    public static boolean PlotPolynomialPredictCA(SessionBean1 sb, int degree, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "plot.pred.polyReg(NA, degree=NULL" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD(imgName, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }    
    
    public static void PlotMultivariateCA(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "plot.linRegMulti.diagnostic(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD(imgName, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void CreateMultivariateModel(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "lin.reg.anal.multi(NA)";
            RCenter.recordRCommand(RC, rCommand);
            //sb.addGraphicsCMD("corr_penalized", rCommand);
            RC.voidEval(rCommand); // Need to change this to return R string 
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void CreateMLModel(SessionBean1 sb, String model) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "reg.machine.anal(NA" + ", \"" + model + "\" )";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void CreateSVMModel(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "reg.svm.anal(NA)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotSVMCA(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "plot.pred.svmReg(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD(imgName, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotMLCA(SessionBean1 sb, String model, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "plot.pred.MLReg(NA" + ", \"" + model + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD(imgName, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
    
    
    /////// ------------ Correlation helper functions --------------- //////////////
    
    // correlation_linear.R
    public static String[] GetDataColumns(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "lin.reg.columns(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(CAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    public static String[] GetLinearCAResults(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "lin.reg.get.results(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(CAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    public static String[] GetPolynomialColumns(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "poly.numeric.columns(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(CAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    public static String[] GetPolyDegrees(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Poly.Reg.Degrees(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(CAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    public static String[] GetPolyCAResults(SessionBean1 sb, int degree){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "poly.reg.get.results(NA, " + degree + ")";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(CAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    public static String[] GetPenalizedColumns(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "pen.numeric.columns(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(CAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    public static String[] GetPenalizedCAResults(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "penal.reg.get.results(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(CAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
}
