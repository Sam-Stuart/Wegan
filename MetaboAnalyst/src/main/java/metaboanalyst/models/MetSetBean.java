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
public class MetSetBean implements Serializable {

    private String name;
    private String members, geneMembers, metaboliteMembers;

    private String refs;

    public MetSetBean(String nm, String memb, String rfs) {
        name = nm;
        members = memb;
        refs = rfs;
    }

    public MetSetBean(String nm, String mema, String memb, String rfs) {
        name = nm;
        geneMembers = mema;
        metaboliteMembers = memb;
        refs = rfs;
    }

    public String getMembers() {
        return members;
    }

    public String getGeneMembers() {
        return geneMembers;
    }

    public void setGeneMembers(String geneMembers) {
        this.geneMembers = geneMembers;
    }

    public String getMetaboliteMembers() {
        return metaboliteMembers;
    }

    public void setMetaboliteMembers(String metaboliteMembers) {
        this.metaboliteMembers = metaboliteMembers;
    }

    public String getName() {
        return name;
    }

    public String getRefs() {
        return refs;
    }

}
