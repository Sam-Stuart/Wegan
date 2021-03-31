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
 * @author Leif
 */
public class PlottingUtils {
    
    
    public static void PlotlinearGraph(SessionBean1 sb, String imgName, String format, int dpi, String type, int numlines,
            String colors, int weights, int pchs, String xlab, String ylab, String title, String facA, String facB) {
        try {
            // Calling function from plotting.r
            RConnection RC = sb.getRConnection();
            String rCommand = "plotLinearFunction(NA" + ", \"" + imgName + "\", \"" + format + "\", "
                    + dpi + ", width=NA, \"" + type + "\", \"" + numlines + "\", \"" + colors + "\", \"" + weights + "\", \""  + pchs + "\", \"" 
                    + xlab + "\", \""  + ylab + "\", \""  + title + "\", \""  + facA + "\", \""  + facB + "\")";
            RCenter.recordRCommand(RC, rCommand);
            
            sb.addGraphicsCMD("lin", rCommand);
            
            
            RC.voidEval(rCommand);
            
            
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    // linear get data colums n
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
    
}
