/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

import javax.faces.model.SelectItem;
import metaboanalyst.rwrappers.RDataUtils;
import org.primefaces.model.chart.Axis;
import org.primefaces.model.chart.AxisType;
import org.primefaces.model.chart.LineChartModel;
import org.primefaces.model.chart.LineChartSeries;

/**
 *
 * @author Jeff
 */
public class CAUtils {

    public static void PlotLinearCA(SessionBean1 sb, String imgName, String format, int dpi) {
        System.out.println("HELLO RIGHT BEFORE PLOT LINEAR");
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotLinearGraph(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            //sb.addGraphicsCMD("corr_linear", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        System.out.println("HELLO RIGHT AFTER PLOT LINEAR");
    }
    public static void PlotLinearTableCA(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "PlotLinearTable(NA, weight=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    public static void PlotLinearModelTESTCA(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "lin.reg.anal.one(NA, weight=NULL)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    public static void PlotTESTLinearCA(SessionBean1 sb, String imgName, String format, int dpi) {
        System.out.println("HELLO RIGHT BEFORE PLOT LINEAR");
        try {
            RConnection RC = sb.getRConnection();
            
            String rCommand = "plot.linReg1(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("corr_linear", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        System.out.println("HELLO RIGHT AFTER PLOT LINEAR");
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
    
    public static void CreatePenalizedModel(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "pen.reg.anal(NA)";
            RCenter.recordRCommand(RC, rCommand);
            //sb.addGraphicsCMD("corr_penalized", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
}
