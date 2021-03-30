/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;

/**
 *
 * @author Louisa
 */
public class OAUtils {

    public static void CreateNMDSOA(SessionBean1 sb) {
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "oa.NMDS(NA, distance, abundance, metaData, envData, p.max.var, p.max.env)";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
        
    public static void PlotNMDSscreeOA(SessionBean1 sb, String imgName, String format, int dpi) {
        System.out.println("HELLO RIGHT BEFORE PLOT NMDS SCREE");
        try {
            RConnection RC = sb.getRConnection();
            String rCommand = "Plot.NDMS.scree(NA" + ", \"" + imgName + "\", \"" + format + "\", " + dpi + ", width=NA)";
            RCenter.recordRCommand(RC, rCommand);
            sb.addGraphicsCMD("ord_nmds_scree", rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
        System.out.println("HELLO RIGHT AFTER PLOT NMDS SCREE");
    }
    
    
}
