/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import metaboanalyst.models.ColorBean;
import metaboanalyst.rwrappers.RDataUtils;
import metaboanalyst.utils.DataUtils;


/**
 *
 * @author jianguox
 */
@ManagedBean(name = "styleBean")
@ViewScoped
public class StyleBean implements Serializable{

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");
    private List<ColorBean> colorBeanLists = new ArrayList();
    
    public List<ColorBean> getColorBeanLists() {
        return colorBeanLists;
    }

    public void setupColorPicker() {
        colorBeanLists.clear();
        String[] grpNms = RDataUtils.getGroupNames(sb.getRConnection());
        if (grpNms != null && grpNms.length > 0) {
            for (String grpNm : grpNms) {
                ColorBean cb = new ColorBean(grpNm);
                cb.setShapeType(0);
                colorBeanLists.add(cb);
            }
        }
    }

    public void setColorBeanLists(List<ColorBean> colorBeanLists) {
        this.colorBeanLists = colorBeanLists;
    }

    public void updateColorScheme() {
        updateGraphSetting();
        sb.updateMsg("OK", "You need to go back to update the images to see the effect.");
    }

    private void updateGraphSetting() {
        int grpNum = colorBeanLists.size();
        if (grpNum > 0) {
            String[] cols = new String[grpNum];
            int[] sps = new int[grpNum];
            for (int i = 0; i < grpNum; i++) {
                String col = colorBeanLists.get(i).getColorPopup();
                int sp = colorBeanLists.get(i).getShapeType();
                if (col == null || col.trim().isEmpty()) {
                    sb.updateMsg("Warning:", "Some group does not have a color specified. No color changes will be applied.");
                    col="NA";
                } 
                cols[i] = "#" + col;
                
                if (sp < 0 | sp > 25) {
                    sb.updateMsg("Warning:", "Some invalid color codes found. No color changes will be applied.");
                    sp = 0;
                }
                sps[i] = sp;
            }
            RDataUtils.updateGraphSettings(sb.getRConnection(), cols, sps);
        }
    }
}
