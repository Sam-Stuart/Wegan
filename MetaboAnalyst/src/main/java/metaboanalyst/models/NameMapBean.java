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
public class NameMapBean implements Serializable {

    private String query;
    private String hit;
    private String symbol; //gene
    private String name; //gene
    private String ko; //kegg ortholog id
    private String hmdb_id;
    private String kegg_id;
    private String pubchem_id;
    private String chebi_id;
    private String metlin_id;
    private String details;
    private boolean selected = false;

    public boolean isSelected() {
        return selected;
    }

    public void setSelected(boolean selected) {
        this.selected = selected;
    }

    public NameMapBean() {
    }

    public String getKo() {
        return ko;
    }

    public void setKo(String ko) {
        this.ko = ko;
    }
    
    public String getChebi_id() {
        return chebi_id;
    }

    public void setChebi_id(String chebi_id) {
        this.chebi_id = chebi_id;
    }

    public String getHmdb_id() {
        return hmdb_id;
    }

    public void setHmdb_id(String hmdb_id) {
        this.hmdb_id = hmdb_id;
    }

    public String getHit() {
        return hit;
    }

    public void setHit(String hit) {
        this.hit = hit;
    }

    public String getKegg_id() {
        return kegg_id;
    }

    public void setKegg_id(String kegg_id) {
        this.kegg_id = kegg_id;
    }

    public String getMetlin_id() {
        return metlin_id;
    }

    public void setMetlin_id(String metlin_id) {
        this.metlin_id = metlin_id;
    }

    public String getPubchem_id() {
        return pubchem_id;
    }

    public void setPubchem_id(String pubchem_id) {
        this.pubchem_id = pubchem_id;
    }

    public String getQuery() {
        return query;
    }

    public void setQuery(String query) {
        this.query = query;
    }

    public String getSymbol() {
        return symbol;
    }

    public void setSymbol(String symbol) {
        this.symbol = symbol;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDetails() {
        return details;
    }

    public void setDetails(String details) {
        this.details = details;
    }
}
