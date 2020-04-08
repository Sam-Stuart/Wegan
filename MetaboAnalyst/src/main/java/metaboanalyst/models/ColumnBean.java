/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.models;

import java.io.Serializable;

/**
 *
 * @author jianguox
 */
public class ColumnBean implements Serializable {

    private String name = "";
    private boolean visible = false;

    public ColumnBean(boolean visible, String name) {
        this.visible = visible;
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public boolean isVisible() {
        return visible;
    }
}
