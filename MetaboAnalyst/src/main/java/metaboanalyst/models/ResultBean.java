/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package metaboanalyst.models;

import java.io.Serializable;

/**
 *
 * @author Jeff
 */

public class ResultBean implements Serializable{

 // two column result table
    private String fileNameA;
    private String fileNameB;

    public String getFileNameA() {
        return fileNameA;
    }

    public String getFileNameB() {
        return fileNameB;
    }

    public ResultBean(String fileNameA, String fileNameB) {
        this.fileNameA = fileNameA;
        this.fileNameB = fileNameB;
    }
}
