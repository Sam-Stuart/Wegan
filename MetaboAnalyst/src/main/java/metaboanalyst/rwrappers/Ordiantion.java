/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.rwrappers;

import metaboanalyst.controllers.SessionBean1;
import org.rosuda.REngine.REXPMismatchException;
import org.rosuda.REngine.Rserve.RConnection;
import org.rosuda.REngine.Rserve.RserveException;
/**
 *
 * @author Sam Stuart
 */
public class Ordiantion {

    public static void InitPCA(SessionBean1 sb, boolean origData) {
        try {
            String inputData = null;
            String ext = null;
            String rCommand = "PCA.Anal(NA" + ", \"" + origData + "\")";
            RConnection RC = sb.getRConnection();
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    
    
    
    public static void InitDCA(SessionBean1 sb) {
        try {
            
            //String inputData = null;
            //String ext = null;
            
            
            
            
            //String rCommand = "testDCA()";
            
            //String rCommand = "DCAWegan(\"" + inputData + "\", \"" + sb.getPath2()+ "\", \"" + ext + "\"   )";

            
            
            RConnection RC = sb.getRConnection();
            String rCommand = "transferToJSON()";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }

    public static void InitDiversity(SessionBean1 sb) {
        try {
            
            //String inputData = null;
            //String ext = null;
            
            
            
            
            //String rCommand = "testDCA()";
            
            //String rCommand = "DCAWegan(\"" + inputData + "\", \"" + sb.getPath2()+ "\", \"" + ext + "\"   )";

            
            
            RConnection RC = sb.getRConnection();
            String rCommand = "transferToJSON()";
            RCenter.recordRCommand(RC, rCommand);
            RC.voidEval(rCommand);
        } catch (RserveException rse) {
            System.out.println(rse);
        }
    }
    

}
