/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.meta;

import java.io.Serializable;

/**
 *
 * @author jasminechong1
 */
public class MetaMapBean implements Serializable {
    
    private String name;
    private String denum;
    
    public MetaMapBean() {
    }
    
     public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
    
    public String getDenum() {
        return denum;
    }

    public void setDenum(String denum) {
        this.denum = denum;
    }
    
}
