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

//    public static boolean CreateLinearModel(SessionBean1 sb, String facA, String facB) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "lin.reg.anal.one(NA" + ", \"" + facA + "\", \"" + facB + "\", weights=NULL)";
//            RCenter.recordRCommand(RC, rCommand);
//            RC.voidEval(rCommand);
//            return true;
//        } catch (RserveException rse) {
//            System.out.println(rse);
//            return false;
//        }
//    }


    public static boolean CreateLinearModel(SessionBean1 sb,
            String facA, String facB, Boolean data) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "lin.reg.anal(NA" + ", \"" 
                    + facA + "\", \""
                    + facB + "\", \""
                    + data + "\" )";
// String rCommand = "lin.reg.anal(NA" + ", \"" + facA + "\", \"" + facB + "\", weights=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    

public static boolean PlotLinearCA(SessionBean1 sb,
        String facA, String facB, Boolean data,
        String col_dots, String col_line, Boolean plot_ci, 
        Boolean plot_eq, Boolean plot_rsq, Boolean plot_rsq_adj, 
                String plot_title, String plot_xlab, String plot_ylab, 
                String imgName, String format, int dpi) {
    try {
            RConnection RC = sb.getRConnection();
            String rCommand = "lin.reg.plot(NA" + ", \"" 
                    + facA + "\", \""
                    + facB + "\", \""
                    + data + "\", \""
                    + col_dots + "\", \""
                    + col_line + "\", \"" 
                    + plot_ci + "\", \"" 
                    + plot_eq + "\", \"" 
                    + plot_rsq + "\", \"" 
                    + plot_rsq_adj + "\", \"" 
                    + plot_title + "\", \"" 
                    + plot_xlab + "\", \"" 
                    + plot_ylab + "\", \"" 
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_linear", rCommand);
            RC.voidEval(rCommand);
//            RC.eval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }    
    
public static boolean PlotLinearPredictCA(SessionBean1 sb,
        String facA, String facB,
        Boolean data, 
        String col_dots, String col_line, Boolean plot_ci, 
        Boolean plot_eq, Boolean plot_rsq, Boolean plot_rsq_adj,
        String plot_title, String plot_xlab, String plot_ylab, 
                String imgName, String format, int dpi) {
    try {
            RConnection RC = sb.getRConnection();
            String rCommand = "lin.pred.plot(NA" + ", \"" 
                    + facA + "\", \""
                    + facB + "\", \""
                    + data + "\", \"" 
                    + col_dots + "\", \"" 
                    + col_line + "\", \"" 
                    + plot_ci + "\", \"" 
                    + plot_eq + "\", \"" 
                    + plot_rsq + "\", \"" 
                    + plot_rsq_adj + "\", \"" 
                    + plot_title + "\", \"" 
                    + plot_xlab + "\", \"" 
                    + plot_ylab + "\", \"" 
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_linear_pred", rCommand);
            RC.voidEval(rCommand);
//            RC.eval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }    

   
public static boolean PlotLinearNormResidCA(SessionBean1 sb,
        String facA, String facB,
        Boolean data, 
        String col_dots, String col_line, 
        String plot_title, String plot_xlab, String plot_ylab, 
                String imgName, String format, int dpi) {
    try {
            RConnection RC = sb.getRConnection();
            String rCommand = "lin.qq.plot(NA" + ", \"" 
                    + facA + "\", \""
                    + facB + "\", \""
                    + data + "\", \"" 
                    + col_dots + "\", \"" 
                    + col_line + "\", \"" 
                    + plot_title + "\", \"" 
                    + plot_xlab + "\", \"" 
                    + plot_ylab + "\", \"" 
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_linear_normres", rCommand);
            RC.voidEval(rCommand);
//            RC.eval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }    

public static boolean PlotLinearResidFitCA(SessionBean1 sb,
        String facA, String facB,
        Boolean data, 
        String col_dots, String col_line, 
        String plot_title, String plot_xlab, String plot_ylab, 
                String imgName, String format, int dpi) {
    try {
            RConnection RC = sb.getRConnection();
            String rCommand = "lin.resfit.plot(NA" + ", \"" 
                    + facA + "\", \""
                    + facB + "\", \""
                    + data + "\", \"" 
                    + col_dots + "\", \"" 
                    + col_line + "\", \"" 
                    + plot_title + "\", \"" 
                    + plot_xlab + "\", \"" 
                    + plot_ylab + "\", \"" 
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_linear_resfit", rCommand);
            RC.voidEval(rCommand);
//            RC.eval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }

 
//    public static void ConvertLinearJSONCA(SessionBean1 sb, String which_plot) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "lin.reg.plot.json(NA" + ", \"" + which_plot + "\" )";
//            RCenter.recordRCommand(RC, rCommand);
////            sb.addGraphicsCMD(imgName, rCommand);
//            RC.voidEval(rCommand);
//        } catch (RserveException rse) {
//            System.out.println(rse);
//        }
//    }    

//    public static boolean PlotLinearCA(SessionBean1 sb, String imgName, String format, int dpi) {
//        try {
//            RConnection RC = sb.getRConnection();
//            String rCommand = "plot.linReg1(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("corr_linear", rCommand);
//            RC.voidEval(rCommand);
//            return true;
//        } catch (RserveException rse) {
//            System.out.println(rse);
//            return false;
//        }
//    }

//        public static boolean PlotLinearCA(SessionBean1 sb, String facA, String facB, String color, Boolean weights, Boolean data, Boolean plot_eq, Boolean plot_rsq, Boolean plot_rsq_adj,  String imgName, String format, int dpi) {
//        public static boolean PlotLinearCA(SessionBean1 sb, String facA, String facB, 
//                Boolean data, String col_dots, String col_line, Boolean plot_ci, Boolean plot_eq, 
//                Boolean plot_rsq, Boolean plot_rsq_adj, String plot_title, 
//                String plot_xlab, String plot_ylab, String imgName, String format, int dpi) {
//    try {
//            RConnection RC = sb.getRConnection();
////            String rCommand = "lin.reg.plot(NA" + ", \"" 
////                    + facA + "\", \""
////                    + facB + "\", \""
////                    + data + "\", \""                    
////                    + col_dots + "\", \""
////                    + col_line + "\", \""
////                    + plot_ci + "\", \""
////                    + plot_eq + "\", \""
////                    + plot_rsq + "\", \""
////                    + plot_rsq_adj + "\", \""
////                    + plot_title + "\", \""
////                    + plot_xaxis + "\", \""
////                    + plot_yaxis + "\", \""
//////                    + "\", imgName)";
////                    + imgName + "\", \"" 
////                    + format + "\", "
////                    + dpi 
////                    + ", width=NA)";
//            String rCommand = "lin.reg.plot(NA" + ", \"" + facA + "\", \"" + facB + "\", \"" + data + "\", \"" + col_dots + "\", \"" + col_line + "\", \"" + plot_ci + "\", \"" + plot_eq + "\", \"" + plot_rsq + "\", \"" + plot_rsq_adj + "\", \"" + plot_title + "\", \"" + plot_xlab + "\", \"" + plot_ylab + "\", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
//            RCenter.recordRCommand(RC, rCommand);
//            sb.addGraphicsCMD("corr_linear", rCommand);
//            RC.voidEval(rCommand);
////            RC.eval(rCommand);
//            return true;
//        } catch (RserveException rse) {
//            System.out.println(rse);
//            return false;
//        }
//    }
    
// PENALIZED
    public static void CreatePenalizedModel(SessionBean1 sb, String method, String facA, Boolean data) {
        System.out.println(method);
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "pen.reg.anal(NA" + ", \"" + method + "\", \"" + facA + "\", \"" + data + "\" )";
            RCenter.recordRCommand(RC, rCommand);
            //sb.addGraphicsCMD("corr_penalized", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
 
    
    public static void PlotPenalizedCA(SessionBean1 sb, Boolean data, 
             String col_dots, String col_line, Boolean plot_ci, 
                String plot_title, String plot_xlab, String plot_ylab,
            String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "pen.pred.plot(NA" + ", \"" 
                    + data + "\", \""                    
                    + col_dots + "\", \""
                    + col_line + "\", \""
                    + plot_ci + "\", \""
                    + plot_title + "\", \""
                    + plot_xlab + "\", \""
                    + plot_ylab + "\", \""
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_penalized", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
 
    public static void PlotPenalizedCVCA(SessionBean1 sb, Boolean data,
             String col_dots, String col_line, 
                String plot_title, String plot_xlab, String plot_ylab,
            String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "pen.cv.plot(NA" + ", \"" 
                    + data + "\", \""                    
                    + col_dots + "\", \""
                    + col_line + "\", \""
                    + plot_title + "\", \""
                    + plot_xlab + "\", \""
                    + plot_ylab + "\", \""
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_penalized2", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
//    POLYNOMIAL
    public static void CreatePolynomialModel(SessionBean1 sb, String facA, String facB, Boolean data) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "poly.reg.anal(NA" + ", \"" + facA + "\", \"" + facB + "\", \"" + data + "\" )";
            RCenter.recordRCommand(RC, rCommand);
            //sb.addGraphicsCMD("corr_poly", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }   
    
    public static boolean PlotPolynomialCA(SessionBean1 sb, String degree, 
           String facA, String facB, Boolean data,
        String col_dots, String col_line, Boolean plot_ci, 
        Boolean plot_eq, Boolean plot_rsq, Boolean plot_rsq_adj, 
                String plot_title, String plot_xlab, String plot_ylab,
            String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
//            String rCommand = "poly.reg.plot(NA, degree=NULL" + ", \"" 
            String rCommand = "poly.reg.plot(NA" + ", \"" + degree + "\", \""
                    + facA + "\", \"" 
                    + facB + "\", \"" 
                    + data + "\", \"" 
                    + col_dots + "\", \"" 
                    + col_line + "\", \"" 
                    + plot_ci + "\", \"" 
                    + plot_eq + "\", \"" 
                    + plot_rsq + "\", \"" 
                    + plot_rsq_adj + "\", \"" 
                    + plot_title + "\", \"" 
                    + plot_xlab + "\", \"" 
                    + plot_ylab + "\", \"" 
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_poly", rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }
    
    
    public static boolean PlotPolynomialPredictCA(SessionBean1 sb, String degree, 
        String facA, String facB, Boolean data,
             String col_dots, String col_line, Boolean plot_ci, 
                String plot_title, String plot_xlab, String plot_ylab,
            String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
//            String rCommand = "poly.pred.plot(NA, degree=NULL" + ", \"" 
            String rCommand = "poly.pred.plot(NA" + ", \"" 
                    + degree + "\", \"" 
                    + facA + "\", \"" 
                    + facB + "\", \"" 
                    + data + "\", \"" 
                    + col_dots + "\", \"" 
                    + col_line + "\", \"" 
                    + plot_ci + "\", \"" 
                    + plot_title + "\", \"" 
                    + plot_xlab + "\", \"" 
                    + plot_ylab + "\", \"" 
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_poly_pred", rCommand);
            RC.voidEval(rCommand);
            return true;
        } catch (RserveException rse) {
            System.out.println(rse);
            return false;
        }
    }    
 
//    MULTIVARIATE  lin.reg.anal.multi plot.pred.linRegMulti  plot.relaimpo.linRegMulti plot.pred.linRegMulti
    //    multi.reg.anal mSetObj=NA, facA="NULL",predtext="NULL", data="false" 
    public static void CreateMultivariateModel(SessionBean1 sb, 
            String facA, String predtext, Boolean data) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "multi.reg.anal(NA" + ", \"" 
                    + facA + "\", \"" 
                    + predtext + "\", \"" 
                    + data + "\" )"; 
            RCenter.recordRCommand(RC, rCommand);
            //sb.addGraphicsCMD("corr_multivariate", rCommand);
            RC.voidEval(rCommand); // Need to change this to return R string 
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
        
    
    public static void PlotMultivariateCA(SessionBean1 sb, 
            String facA, String predtext, Boolean data,
             String col_dots, String col_line, Boolean plot_ci, 
                String plot_title, String plot_xlab, String plot_ylab,
            String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "multi.pred.plot(NA" + ", \"" 
                    + facA + "\", \"" 
                    + predtext + "\", \"" 
                    + data + "\", \"" 
                    + col_dots + "\", \"" 
                    + col_line + "\", \"" 
                    + plot_ci + "\", \"" 
                    + plot_title + "\", \"" 
                    + plot_xlab + "\", \"" 
                    + plot_ylab + "\", \"" 
                    + imgName + "\", \"" 
                    + format + "\", " 
                    + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_multi_pred", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotMultivariateCoeffCA(SessionBean1 sb,
            String facA, String predtext, Boolean data,
             String plot_palette, Boolean plot_label, 
                String plot_title, String plot_xlab, String plot_ylab,
            String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "multi.relaimpo.plot(NA" + ", \"" 
                    + facA + "\", \"" 
                    + predtext + "\", \"" 
                    + data + "\", \"" 
                    + plot_palette + "\", \"" 
                    + plot_label + "\", \"" 
                    + plot_title + "\", \"" 
                    + plot_xlab + "\", \"" 
                    + plot_ylab + "\", \"" 
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_multi_relaimpo", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotMultivariateRelativeCA(SessionBean1 sb, 
             String facA, String predtext, Boolean data,
             String plot_palette, Boolean plot_label, 
                String plot_title, String plot_xlab, String plot_ylab,
            String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "multi.relaimpo.plot(NA" + ", \"" 
                    + facA + "\", \"" 
                    + predtext + "\", \"" 
                    + data + "\", \"" 
                    + plot_palette + "\", \"" 
                    + plot_label + "\", \"" 
                    + plot_title + "\", \"" 
                    + plot_xlab + "\", \"" 
                    + plot_ylab + "\", \"" 
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_multi_relaimpo", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }    
    
//MACHINE LEARNING (ML)
    public static void CreateSVMModel(SessionBean1 sb, String facA, String facB) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "svm.reg.anal(NA" + ", \"" + facA + "\", \"" + facB + "\" )";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotSVMCA(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "svm.pred.plot(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_svm", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
        public static void CreateRFModel(SessionBean1 sb, String facA, String facB) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "rf.reg.anal(NA" + ", \"" + facA + "\", \"" + facB + "\" )";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotRFCA(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "rf.pred.plot(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_rf", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotRFRelativeCA(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "rf.relimpo.plot(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_rf_relaimpo", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    public static void PlotRFErrorCA(SessionBean1 sb, String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "rf.error.plot(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_rf_error", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
 
// LOGISTIC   
    
        public static void CreateLogisticModel(SessionBean1 sb,
                String facA, String predtext, String type,
                String preference, String ordertext) {
        try {
            RConnection RC = sb.getRConnection();
//            String rCommand = "log.reg.anal(NA)";
            String rCommand = "log.reg.anal(NA" + ", \"" 
                    + facA + "\", \"" 
                    + predtext + "\", \"" 
                    + type + "\", \"" 
                    + preference + "\", \"" 
                    + ordertext + "\" )";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
        
    public static void PlotLogisticEffectCA(SessionBean1 sb,
            String type, 
//            String facA, Boolean data,
             Boolean plot_ci, 
             String plot_title, String plot_xlab, String plot_ylab,
             Boolean plot_xangle, String plot_palette, Boolean plot_leg_horiz, String plot_leg_pos,
            String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "log.effects.plot(NA" + ", \"" 
                    + type + "\", \""  
//                    + facA + "\", \"" 
//                    + data + "\", \"" 
                    + plot_ci + "\", \"" 
                    + plot_title + "\", \"" 
                    + plot_xlab + "\", \"" 
                    + plot_ylab + "\", \"" 
                    
                    + plot_xangle + "\", \"" 
                    + plot_palette + "\", \"" 
                    + plot_leg_horiz + "\", \"" 
                    + plot_leg_pos + "\", \"" 
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_log_eff", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void PlotLogisticROCCA(SessionBean1 sb,
            String type, String plot_palette, String plot_title,
            String imgName, String format, int dpi) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "log.ROC.plot(NA" + ", \"" + type + "\", \""  
                    + type + "\", \"" 
                    + plot_palette + "\", \"" 
                    + plot_title + "\", \"" 
                    + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_log_roc", rCommand);
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
    
    public static String[] GetCatDataColumns(SessionBean1 sb){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "factor.columns(NA)";
            RCenter.recordRCommand(RC, rCommand);
            return RC.eval(rCommand).asStrings();
        } catch (RserveException rse) {
            System.out.println(rse);
        } catch (REXPMismatchException ex) {
            Logger.getLogger(CAUtils.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    
    public static String[] GetCatLevelDataColumns(SessionBean1 sb, String facA){
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "log.response.levels(NA" + ", \"" + facA + "\")";
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
