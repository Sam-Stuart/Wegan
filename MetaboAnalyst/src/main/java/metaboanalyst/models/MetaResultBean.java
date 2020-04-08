/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.models;

import java.io.Serializable;
import java.util.HashMap;

/**
 * Results for individual feature Note, maximal 10 data sets, corresponding to
 * ten cols
 *
 * @author Jeff
 */
public class MetaResultBean implements Serializable {

    //this ID is for string only for sorting, not embedded hyperlink
    private String ID; //note, hashmap can also have a ID key => hyperlinked gene
    private String name;
    private HashMap<String, String> contentMap;

    public MetaResultBean(String ID) {
        this.ID = ID;
        contentMap = new HashMap<String, String>();
    }

    public String getID() {
        return ID;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setValue(String property, String value) {
        contentMap.put(property, value);
    }

    public String getValue(String property) {
        return (contentMap.get(property));
    }
}
