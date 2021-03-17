/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.utils;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.Set;
import java.util.Date;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import javax.faces.context.FacesContext;
import metaboanalyst.models.User;
import metaboanalyst.controllers.SessionBean1;
import org.primefaces.model.DefaultTreeNode;
import org.primefaces.model.TreeNode;
import org.primefaces.model.UploadedFile;

/**
 *
 * @author Jeff
 */
public class DataUtils {

    @SuppressWarnings("unchecked")
    public static <T> T findBean(String beanName) {
        FacesContext context = FacesContext.getCurrentInstance();
        return (T) context.getApplication().evaluateExpressionGet(context, "#{" + beanName + "}", Object.class);
    }

    //find node based on the ID from tree, note, only search for three layers
    public static TreeNode getSelectedNode(TreeNode naviTree, String pageID) {
        Iterator<TreeNode> i = naviTree.getChildren().iterator();
        while (i.hasNext()) {
            TreeNode nd = i.next();
            if (nd.getData().toString().equals(pageID)) {
                return nd;
            }
            if (!nd.isLeaf()) {
                Iterator<TreeNode> i2 = nd.getChildren().iterator();
                while (i2.hasNext()) {
                    TreeNode nd2 = i2.next();
                    if (nd2.getData().toString().equals(pageID)) {
                        return nd2;
                    }
                    if (!nd2.isLeaf()) {
                        Iterator<TreeNode> i3 = nd2.getChildren().iterator();
                        while (i3.hasNext()) {
                            TreeNode nd3 = i3.next();
                            if (nd3.getData().toString().equals(pageID)) {
                                return nd3;
                            }
                        }
                    }
                }
            }
        }
        return null;
    }

    public static String readTextFile(String filePath) {

        BufferedReader br = null;
        String text = "";
        String line = "";
        try {
            br = new BufferedReader(new FileReader(filePath));
            while ((line = br.readLine()) != null) {
                // use comma as separator
                //String[] country = line.split(splitBy);
                line = line.replace("\t", "  ");
                text = text + "\n" + line;
            }

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (br != null) {
                try {
                    br.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return text;
    }

    //return all filenames of the given folder used by loading all R scripts
    public static ArrayList<String> getAllFileNames(String folderPath, boolean absPath) {
        File folder = new File(folderPath);
        File files[] = folder.listFiles();
        ArrayList<String> fileNames = new ArrayList();
        if (absPath) {
            for (int i = 0; i < files.length; i++) {
                fileNames.add(folderPath + File.separator + files[i].getName());
            }
        } else {
            for (int i = 0; i < files.length; i++) {
                fileNames.add(files[i].getName());
            }
        }
        return fileNames;
    }

    //Note: for Class uploadedFile
    public static String getJustFileName(String uploadedFileName) {
        int index = uploadedFileName.lastIndexOf('/');
        String justFileName;
        if (index >= 0) {
            justFileName = uploadedFileName.substring(index + 1);
        } else {
            // Try backslash
            index = uploadedFileName.lastIndexOf('\\');
            if (index >= 0) {
                justFileName = uploadedFileName.substring(index + 1);
            } else { // No forward or back slashes
                justFileName = uploadedFileName;
            }
        }
        return justFileName;
    }

    public static void copyFile(File in, File out) {
        try {
            FileInputStream fis = new FileInputStream(in);
            FileOutputStream fos = new FileOutputStream(out);
            copyInputStream(fis, fos);
        } catch (IOException e) {
        }
    }

    private static void copyInputStream(InputStream in, OutputStream out) throws IOException {
        byte[] buffer = new byte[1024];
        int len;
        while ((len = in.read(buffer)) >= 0) {
            out.write(buffer, 0, len);
        }
        in.close();
        out.close();
    }

    public static void createZipFile(File[] files, String path) {
        // Create a buffer for reading the files
        byte[] buf = new byte[18024];

        try {
            // Create the ZIP file
            String outFilename = path + File.separator + "Download.zip";
            ZipOutputStream out = new ZipOutputStream(new FileOutputStream(outFilename));

            // Compress the files
            for (int i = 0; i < files.length; i++) {
                FileInputStream in = new FileInputStream(files[i]);
                // Add ZIP entry to output stream.
                out.putNextEntry(new ZipEntry(files[i].getName()));
                int len;
                while ((len = in.read(buf)) > 0) {
                    out.write(buf, 0, len);
                }
                out.closeEntry();
                in.close();
            }
            out.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static String setupTable(String lbl, double[][] sigmat, String[] rownames, String[] colnames) {

        if (rownames == null || rownames.length == 0) {
            return ("No significant feature was identified");
        } else {
            String str = "<table border=\"1\" cellpadding=\"5\">";
            str = str + "<tr><th>" + lbl + "</th>";
            for (int i = 0; i < colnames.length; i++) {
                str = str + "<th>" + colnames[i] + "</th>";
            }
            str = str + "</tr>";
            for (int i = 0; i < rownames.length; i++) {
                str = str + "<tr><td>" + rownames[i] + "</td>";
                for (int j = 0; j < colnames.length; j++) {
                    str = str + "<td>" + sigmat[i][j] + "</td>";
                }
                str = str + "</tr>";
            }
            str = str + "</table>";
            return str;
        }
    }

    // note since Rserver only return [][] for double, we can get double[][] and String[] and combine them here
    // note: the colnames include the last name for the extraCol
    public static String setupTable(String lbl, double[][] sigmat, String[] extraCol, String[] rownames, String[] colnames) {

        if (rownames == null || rownames.length == 0) {
            return ("No significant feature was identified");
        } else {
            String str = "<table border=\"1\" cellpadding=\"5\">";
            str = str + "<tr><th>" + lbl + "</th>";
            for (int i = 0; i < colnames.length; i++) {
                str = str + "<th>" + colnames[i] + "</th>";
            }

            //remember colnames here is longer than matrix
            int col_len = colnames.length - 1;
            str = str + "</tr>";
            for (int i = 0; i < rownames.length; i++) {
                str = str + "<tr><td>" + rownames[i] + "</td>";
                for (int j = 0; j < col_len; j++) {
                    str = str + "<td>" + sigmat[i][j] + "</td>";
                }
                str = str + "<td>" + extraCol[i] + "</td></tr>";
            }
            str = str + "</table>";
            return str;
        }
    }

    public static ArrayList<String> getQueryNames(String text) {
        try {
            ArrayList<String> nmVec = new ArrayList();
            StringTokenizer st = new StringTokenizer(text, "\n");
            while (st.hasMoreTokens()) {
                String line = st.nextToken();
                line = line.trim();//remove both leading and end space
                if (line.length() == 0) { //empty line
                    continue;
                }

                if (line.indexOf(";") > 0) {
                    nmVec.addAll(Arrays.asList(line.split(";")));
                } else {
                    nmVec.add(line);
                }

            }
            return nmVec;
        } catch (NumberFormatException e) {
            e.printStackTrace();
            return null;
        }
    }

    //parse String between one layer of HTML tag
    public static String getStringHTMLTag(String htmlString) {
        Pattern pattern = Pattern.compile("<([A-Za-z][A-Za-z0-9]*)\\b[^>]*>(.*?)</\\1>");
        Matcher m = pattern.matcher(htmlString);
        if (m.find()) {
            return (m.group(2));
        }

        return htmlString;
    }

    public static String[] getQueryNames(String text, String sep) {
        return getNamesArray(text, sep);
    }

    public static String[] getQueryNames(UploadedFile uploadedFile, String sep) {
        try {
            return getNamesArray(convertStreamToString(uploadedFile.getInputstream()), sep);
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    //compound names need to be  need to be one name per column
    public static String[] getNamesArray(String content, String sep) {
        ArrayList<String> nmVec = new ArrayList();
        if (sep == null) {
            sep = System.getProperty("line.separator");
        }
        //seperate by line separator or ";"
        StringTokenizer st = new StringTokenizer(content, sep);

        while (st.hasMoreTokens()) {
            String line = st.nextToken();
            line = cleanString(line);//remove both leading and end space
            if (line.length() == 0) { //empty line
                continue;
            }
            nmVec.add(line);
        }
        //remove duplicates, if any
        Set<String> mySet = new LinkedHashSet(nmVec);
        
        return mySet.toArray(new String[0]);
    }

    public static String convertStreamToString(java.io.InputStream is) {
        //java.util.Scanner s = new java.util.Scanner(is).useDelimiter("\\A");
        return new java.util.Scanner(is).useDelimiter("\\A").next();
        //return s.hasNext() ? s.next() : "";
    }

    // clear string spaces and punctuations
    public static String cleanString(String s) {
        s.replaceAll("^\\s+", ""); //remove leading space
        s.replaceAll("\\s+$", ""); //remove trailing space
        s = s.replaceAll("[^a-zA-Z0-9)]$", ""); //remove last one if not character/number/) (i.e. punctuation)
        return s;
    }

    public static boolean cropImage(String convertPath, String imgPath, String targetPath, int x, int y, int width, int height, int quality) {

        if (quality < 0 || quality > 100) {
            quality = 75;
        }

        ArrayList command = new ArrayList(10);

        // note: CONVERT_PROG is a class variable that stores the location of ImageMagick's convert command
        // need to supply full path to the covert command
        command.add(convertPath);
        command.add("-crop");
        command.add(width + "x" + height + "+" + x + "+" + y);

        command.add("-quality");
        command.add("" + quality);
        command.add("+repage");
        command.add(imgPath);
        command.add(targetPath);

        return myExec((String[]) command.toArray(new String[1]));
    }

    public static ArrayList<String> getFileNames(String folderPath, boolean absPath) {
        File folder = new File(folderPath);
        File files[] = folder.listFiles();
        ArrayList<String> fileNames = new ArrayList();
        if (absPath) {
            for (int i = 0; i < files.length; i++) {
                fileNames.add(folderPath + File.separator + files[i].getName());
            }
        } else {
            for (int i = 0; i < files.length; i++) {
                fileNames.add(files[i].getName());
            }
        }
        return fileNames;
    }

    /**
     * Tries to exec the command, waits for it to finsih, logs errors if exit
     * status is nonzero, and returns true if exit status is 0 (success).
     *
     * @param command Description of the Parameter
     * @return Description of the Return Value
     */
    private static boolean myExec(String[] command) {
        Process proc;
        try {
            //System.out.println("Trying to execute command " + Arrays.asList(command));
            proc = Runtime.getRuntime().exec(command);
        } catch (Exception e) {
            e.printStackTrace();
            System.out.println("IOException while trying to execute " + Arrays.toString(command));
            return false;
        }
        //System.out.println("Got process object, waiting to return.");
        int exitStatus;
        while (true) {
            try {
                exitStatus = proc.waitFor();
                break;
            } catch (java.lang.InterruptedException e) {
                System.out.println("Interrupted: Ignoring and waiting");
            }
        }
        if (exitStatus != 0) {
            System.out.println("Error executing command: " + exitStatus);
        }
        return (exitStatus == 0);
    }

    public static String uploadFile(UploadedFile file, SessionBean1 sb, String outFileNm, boolean onPublicServer) {

        if (file == null || file.getSize() == 0) {
            sb.updateMsg("Error", "Empty file?");
            return null;
        }

        if (onPublicServer & file.getSize() > 50000000) {
            sb.updateMsg("Error", "The file size exceeds limit: 50M");
            return null;
        }

        String fileName = file.getFileName();

        if (fileName.endsWith(".csv") | fileName.endsWith(".txt") | fileName.endsWith(".zip")) {
            try {
                String homeDir = sb.getCurrentUser().getHomeDir();
                InputStream in = file.getInputstream();
                if (outFileNm == null) {
                    outFileNm = fileName;
                }
                OutputStream out = new FileOutputStream(new File(homeDir + File.separator + outFileNm));
                byte[] buffer = new byte[1024];
                int len;
                while ((len = in.read(buffer)) >= 0) {
                    out.write(buffer, 0, len);
                }
                in.close();
                out.close();

            } catch (IOException e) {
                System.out.println(e.getMessage());
            }
        } else {
            sb.updateMsg("Error", "Only tab delimited (.txt) or comma separated (.csv) files are accepted");
            return null;
        }
        return fileName;
    }

    public static void deleteFile(User usr, String filename) {
        File f1 = new File(usr.getHomeDir() + "/" + filename);
        if (f1.exists()) {
            boolean sucess = f1.delete();
            if (!sucess) {
                System.out.println("=== Delete file - " + filename + " failed.");
            }
        }
    }

    //a utility function to remove the old user folders
    //called everytime a new user folder is created, default 12 hours
    public static void deleteFilesOlderThanNdays(String dirWay) {

        File directory = new File(dirWay);
        //Calendar cal = Calendar.getInstance();
        //cal.add(Calendar.DAY_OF_MONTH, -1);
        //long purgeTime = cal.getTimeInMillis();
        long currentTime = new Date().getTime();
        long purgeTime = 12 * 60 * 60 * 1000;
        if (directory.exists()) {
            File[] listFiles = directory.listFiles();
            for (File listFile : listFiles) {
                if (listFile.getName().startsWith("guest")) {
                    if (currentTime - listFile.lastModified() > purgeTime) {
                        if (!deleteDir(listFile.getAbsolutePath())) {
                            System.err.println("Unable to delete file: " + listFile);
                        }
                    }
                }
            }
        }
    }

    //a utility function to kill long running Rserver process 
    //Note 1) spare the mother (the oldest one) 
    //     2) default 2 hour    
    // Only works on Linux !!!! Mac does not recognize --sort=start_time
    // filter need to apply in order day filter, hour filter and minutes filter (not used)
    //for filter based on minutes (20min)
    //String[] rmMinLongCmd = new String[]{bashPath, "-c", "ps -eo pid,etime,args --sort=start_time | grep 'Rserve'| grep -v 'grep' | tail -n +2 | grep '[0-9][0-9]:[0-9][0-9]'| awk 'substr($2,1,(index($2,\":\")-1))-20>=0' | awk '{print $1}' | xargs kill -9"};
    // System.out.println("Process__ :: " + bashPath);
    // System.out.println(Arrays.toString(rmDayLongCmd));
    public static void killLongRunningRservProcesses(String bashPath) {
        String[] rmDayLongCmd = new String[]{bashPath, "-c", "ps -eo pid,etime,args --sort=start_time | grep 'Rserve'| grep -v 'grep' | tail -n +2 | awk 'substr($2,1,index($2,\"-\")-1)>0' | awk '{print $1}' | xargs kill -9"};
        if (myExec(rmDayLongCmd)) {
            System.out.println("Successfully removed day long running process!");
        } else {
            System.err.println("No day long process found!");
        }

        String[] rmHourLongCmd = new String[]{bashPath, "-c", "ps -eo pid,etime,args --sort=start_time | grep 'Rserve'| grep -v 'grep' | tail -n +2 | grep '[0-9][0-9]:[0-9][0-9]:[0-9][0-9]'| awk 'substr($2,1,index($2,\":\")-2)>=0' | awk '{print $1}' | xargs kill -9"};
        if (myExec(rmHourLongCmd)) {
            System.out.println("Successfully removed hour long running process!");
        } else {
            System.err.println("No 1 hour long process found!");
        }

        //need to clean the /tmp regularly as server won't reboot often
        String[] cleanTmpCmd = new String[]{bashPath, "-c", "find /tmp -name 'guest*' -mmin +180 -exec rm -rf {} +"};
        if (myExec(cleanTmpCmd)) {
            System.out.println("Successfully cleaned the tmp folder!");
        } else {
            System.err.println("Failed to clean the folder!");
        }
    }

    //use Unix command to remvoe (non-empty) folder
    public static boolean deleteDir(String fdPath) {
        //first make sure they are removable 
        ArrayList command = new ArrayList(4);
        command.add("chmod");
        command.add("-R");
        command.add("777");
        command.add(fdPath);
        boolean res = myExec((String[]) command.toArray(new String[1]));
        
        if (res) {
            command = new ArrayList(3);
            command.add("rm");
            command.add("-r");
            command.add(fdPath);
            return myExec((String[]) command.toArray(new String[1]));
        } else {
            return false;
        }
    }

    public static boolean generateReportCMD(String rScriptPath, String RScriptHome, String userDir) {
        String s = null;
        try {
            //System.setProperty("user.dir", userDir);
            //String sysCmd = cmdPath + " " + userDir + "/Analysis_Report.tex";
            //System.out.println("===========" + sysCmd);
            String sysCmd = rScriptPath + " " + RScriptHome + "/_pdf_cmd.sh" + " " + RScriptHome + "/_sweave_cmd.R" + " " + userDir;
            //System.out.println("===========" + sysCmd);
            Process p = Runtime.getRuntime().exec(sysCmd);

            BufferedReader stdInput = new BufferedReader(new InputStreamReader(p.getInputStream()));
            BufferedReader stdError = new BufferedReader(new InputStreamReader(p.getErrorStream()));
            // read the output from the command
            System.out.println("Here is the standard output of the command:\n");
            while ((s = stdInput.readLine()) != null) {
                System.out.println(s);
            }

            // read any errors from the attempted command
            System.out.println("Here is the standard error of the command (if any):\n");
            while ((s = stdError.readLine()) != null) {
                System.out.println(s);
            }

            p.waitFor();

        } catch (Exception e) {
            System.out.println("exception happened - here's what I know: ");
            e.printStackTrace();
            return false;
        }
        return true;
    }

    public static TreeNode createNaviTree(String type) {
        TreeNode naviTree = new DefaultTreeNode("Root", null);
        TreeNode upNode = new DefaultTreeNode("Upload", naviTree);
        if (type.equalsIgnoreCase("utils")) {
            addUtilNodes(naviTree);
        }else if (type.equalsIgnoreCase("nmds")) {
            //System.out.println("You need to define the navigation tree for this analysis type: " + type);
            addProcNodes(naviTree, type);
            TreeNode normNode = new DefaultTreeNode("Normalization", naviTree);
        }
        else if (type.equalsIgnoreCase("mummichog")) {
            addMummichogNodes(naviTree);
        } else if (type.equalsIgnoreCase("network")) {
            addMNetworkNodes(naviTree);
        } else if (type.equalsIgnoreCase("metadata")) {
            addMetaAnalNodes(naviTree);
        } else {
            if (type.equalsIgnoreCase("pathinteg")) {
                addPathIntegNodes(naviTree);
            } else {
                addProcNodes(naviTree, type);
                TreeNode normNode = new DefaultTreeNode("Normalization", naviTree);
                if (type.equalsIgnoreCase("stat")) {
                    addStatNodes(naviTree);
                } else if (type.equalsIgnoreCase("enrich")) {
                    addEnrichNodes(naviTree);
                } else if (type.equalsIgnoreCase("pathway")) {
                    addPathNodes(naviTree);
                } else if (type.equalsIgnoreCase("time")) {
                    addTimeNodes(naviTree);
                } else if (type.equalsIgnoreCase("power")) {
                    addPowerNodes(naviTree);
                } else if (type.equalsIgnoreCase("roc")) {
                    addRocNodes(naviTree);
                } else if (type.equalsIgnoreCase("dispersal")) {
                    addDispersalNodes(naviTree);
                } else if (type.equalsIgnoreCase("plotting")) {
                    addPlottingNodes(naviTree);
                } else {
                    System.out.println("You need to define the navigation tree for this analysis type: " + type);
                }
            }
        }
        TreeNode dn_node = new DefaultTreeNode("Download", naviTree);
        TreeNode exitNd = new DefaultTreeNode("Exit", naviTree);
        return naviTree;
    }

    private static void addProcNodes(TreeNode parent, String mode) {
        TreeNode processNode = new DefaultTreeNode("Processing", parent);
        processNode.setSelectable(false);
        switch (mode) {
            case "enrich": {
                TreeNode nodep2 = new DefaultTreeNode("Name check", processNode);
                TreeNode nodep3 = new DefaultTreeNode("Conc. check", processNode);
                break;
            }
            case "pathway": {
                TreeNode nodep2 = new DefaultTreeNode("Name check", processNode);
                break;
            }
            default:
                TreeNode nodep1 = new DefaultTreeNode("Pre-process", processNode);
                break;
        }
        TreeNode nodep4 = new DefaultTreeNode("Data check", processNode);
        TreeNode nodep5 = new DefaultTreeNode("Missing value", processNode);
        TreeNode nodep6 = new DefaultTreeNode("Data filter", processNode);
        TreeNode nodep7 = new DefaultTreeNode("Data editor", processNode);
        TreeNode nodep8 = new DefaultTreeNode("Image options", processNode);
    }

    private static void addStatNodes(TreeNode parent) {
        TreeNode analNode = new DefaultTreeNode("Statistics", parent);
        TreeNode nodea1 = new DefaultTreeNode("Fold change", analNode);
        TreeNode nodea2 = new DefaultTreeNode("T-test", analNode);
        TreeNode nodea3 = new DefaultTreeNode("Volcano plot", analNode);
        TreeNode nodea4 = new DefaultTreeNode("ANOVA", analNode);
        TreeNode nodea5 = new DefaultTreeNode("Correlations", analNode);
        TreeNode nodea6 = new DefaultTreeNode("PatternHunter", analNode);
        TreeNode nodea7 = new DefaultTreeNode("PCA", analNode);
        TreeNode nodea8 = new DefaultTreeNode("PLSDA", analNode);
        TreeNode nodea801 = new DefaultTreeNode("sPLSDA", analNode);
        TreeNode nodea81 = new DefaultTreeNode("OrthoPLSDA", analNode);
        TreeNode nodea9 = new DefaultTreeNode("SAM", analNode);
        TreeNode nodea10 = new DefaultTreeNode("EBAM", analNode);
        TreeNode nodea11 = new DefaultTreeNode("Dendrogram", analNode);
        TreeNode nodea12 = new DefaultTreeNode("Heatmap", analNode);
        TreeNode nodea13 = new DefaultTreeNode("SOM", analNode);
        TreeNode nodea14 = new DefaultTreeNode("K-means", analNode);
        TreeNode nodea15 = new DefaultTreeNode("RandomForest", analNode);
        TreeNode nodea16 = new DefaultTreeNode("SVM", analNode);
    }

    private static void addEnrichNodes(TreeNode parent) {
        TreeNode enrichNode = new DefaultTreeNode("Enrichment", parent);
        enrichNode.setSelectable(false);
        TreeNode nodee1 = new DefaultTreeNode("Set parameter", enrichNode);
        TreeNode nodee2 = new DefaultTreeNode("View result", enrichNode);
    }

    private static void addPathNodes(TreeNode parent) {
        TreeNode pathNode = new DefaultTreeNode("Pathway", parent);
        pathNode.setSelectable(false);
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", pathNode);
        TreeNode node_p2 = new DefaultTreeNode("View result", pathNode);
    }

    private static void addTimeNodes(TreeNode parent) {
        TreeNode tsNode = new DefaultTreeNode("Time Series", parent);
        //tsNode.setSelectable(false);
        TreeNode tsNode1 = new DefaultTreeNode("iPCA", tsNode);
        TreeNode tsNode2 = new DefaultTreeNode("Heatmap2", tsNode);
        TreeNode tsNode3 = new DefaultTreeNode("ANOVA2", tsNode);
        TreeNode tsNode4 = new DefaultTreeNode("ASCA", tsNode);
        TreeNode tsNode5 = new DefaultTreeNode("MEBA", tsNode);
    }

    private static void addPowerNodes(TreeNode parent) {
        TreeNode pathNode = new DefaultTreeNode("Power Analysis", parent);
        pathNode.setSelectable(false);
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", pathNode);
        TreeNode node_p2 = new DefaultTreeNode("View result", pathNode);
    }

    private static void addPathIntegNodes(TreeNode parent) {
        TreeNode pathNode = new DefaultTreeNode("Integrative Analysis", parent);
        TreeNode node_p0 = new DefaultTreeNode("ID map", pathNode);
        pathNode.setSelectable(false);
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", pathNode);
        TreeNode node_p2 = new DefaultTreeNode("Overview", pathNode);
        TreeNode node_p3 = new DefaultTreeNode("View result", pathNode);
    }

    private static void addMNetworkNodes(TreeNode parent) {
        TreeNode nodep4 = new DefaultTreeNode("ID mapping", parent);
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", parent);
        TreeNode node_p2 = new DefaultTreeNode("Network viewer", parent);
    }

    private static void addMetaAnalNodes(TreeNode parent) {
        TreeNode nodep4 = new DefaultTreeNode("Meta analysis", parent);
        TreeNode node_p1 = new DefaultTreeNode("Result table", parent);
        TreeNode node_p2 = new DefaultTreeNode("Venn diagram", parent);
    }

    private static void addMummichogNodes(TreeNode parent) {
        TreeNode nodep4 = new DefaultTreeNode("Data check", parent);
        TreeNode node_p11 = new DefaultTreeNode("Analysis options", parent);
        TreeNode node_p1 = new DefaultTreeNode("Set parameter", parent);
        TreeNode node_p3 = new DefaultTreeNode("View result", parent);
        TreeNode node_p4 = new DefaultTreeNode("Metabolic network", parent);
    }

    private static void addRocNodes(TreeNode parent) {
        TreeNode rocNode = new DefaultTreeNode("ROC Analysis", parent);
        //rocNode.setSelectable(false);
        TreeNode node_p1 = new DefaultTreeNode("Univariate", rocNode);
        TreeNode node_p11 = new DefaultTreeNode("ROC detail", node_p1);

        TreeNode node_p2 = new DefaultTreeNode("Multivariate", rocNode);
        node_p2.setSelectable(false);
        TreeNode node_p3 = new DefaultTreeNode("Set parameter", node_p2);
        TreeNode node_p4 = new DefaultTreeNode("Explorer", node_p2);

        TreeNode node_p5 = new DefaultTreeNode("Tester", rocNode);
        node_p5.setSelectable(false);
        TreeNode node_p6 = new DefaultTreeNode("Builder", node_p5);
        TreeNode node_p7 = new DefaultTreeNode("Evaluator", node_p5);
    }
    private static void addDispersalNodes(TreeNode parent) {
        TreeNode dispersalNode = new DefaultTreeNode("Dispersal", parent);
        TreeNode noded1 = new DefaultTreeNode("Biogeographical Dispersal", dispersalNode);
        TreeNode noded2 = new DefaultTreeNode("Beals Smoothing", dispersalNode);
        TreeNode noded3 = new DefaultTreeNode("Beta Dispersal", dispersalNode);
    }
    
    private static void addPlottingNodes(TreeNode parent) {
        TreeNode plottingNode = new DefaultTreeNode("Plotting", parent);
        TreeNode noded1 = new DefaultTreeNode("Linear Graph", plottingNode);
        TreeNode noded2 = new DefaultTreeNode("Boxplot", plottingNode);
        TreeNode noded3 = new DefaultTreeNode("Bar Graph", plottingNode);
        TreeNode noded4 = new DefaultTreeNode("Scatter Plot", plottingNode);
        TreeNode noded5 = new DefaultTreeNode("Histogram", plottingNode);
        TreeNode noded6 = new DefaultTreeNode("Pie chart", plottingNode);
    }

    private static void addUtilNodes(TreeNode parent) {
        //TreeNode uNode = new DefaultTreeNode("Utilities", parent);
        //uNode.setSelectable(false);

        TreeNode convertNode = new DefaultTreeNode("ID Conversion", parent);
        convertNode.setSelectable(false);
        TreeNode idInputNode = new DefaultTreeNode("ID Input", convertNode);
        TreeNode resNode = new DefaultTreeNode("Map Result", convertNode);

        TreeNode batchNode = new DefaultTreeNode("Batch Effect", parent);
        batchNode.setSelectable(false);
        TreeNode inputNode = new DefaultTreeNode("Batch Upload", batchNode);
        TreeNode viewNode = new DefaultTreeNode("Batch View", batchNode);

        TreeNode powerNode = new DefaultTreeNode("Lipidomics", parent);
    }

    //give a string vector ["a", "b", "c"], return a single string 
    public static String createStringVector(String[] nms) {
        StringBuilder sb = new StringBuilder();
        for (String s : nms) {
            sb.append(s);
            sb.append("\",\"");
        }
        String res = sb.toString();
        //trim the last comma and quote
        //System.out.println(res + "====");
        res = res.substring(0, res.length() - 2);
        res = "c(\"" + res + ")";
        return (res);
    }

}
