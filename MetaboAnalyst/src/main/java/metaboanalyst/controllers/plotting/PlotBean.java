/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Beans/Bean.java to edit this template
 */
package metaboanalyst.controllers.plotting;

import java.util.Arrays;
import java.util.List;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.rwrappers.PlottingUtils;
import metaboanalyst.utils.DataUtils;

/**
 *
 * @author hieunguyen
 */
public abstract class PlotBean {

    protected SessionBean1 sb = DataUtils.findBean("sessionBean1");
    protected SelectItem[] textFontSizeOpts;
    protected SelectItem[] axisFontSizeOpts;
    protected String labx;
    protected String laby;
    protected String title;
    protected Integer chosenAxisTextSize;
    protected Integer chosenTitleTextSize;
    protected SelectItem[] factorBoxColumnOpts;
    protected SelectItem[] numericBoxColumnOpts;
    protected boolean data;

    protected PlotBean() {
        SelectItem[] fontSizeOpts = new SelectItem[5];
        fontSizeOpts[0] = new SelectItem(12, "Extra Small");
        fontSizeOpts[1] = new SelectItem(16, "Small");
        fontSizeOpts[2] = new SelectItem(20, "Medium");
        fontSizeOpts[3] = new SelectItem(24, "Large");
        fontSizeOpts[4] = new SelectItem(32, "Extra Large");

//        Make two clones of this for text and axis
        this.textFontSizeOpts = fontSizeOpts.clone();
        this.axisFontSizeOpts = fontSizeOpts.clone();
    }

    public SelectItem[] getFactorBoxColumnOpts() {
        String[] columns = PlottingUtils.GetFactorDataColumnsBoxPlt(sb);

        if (columns != null) {
            int columnsLen = columns.length;
            factorBoxColumnOpts = new SelectItem[columnsLen];
            List<String> columnNames = Arrays.asList(columns);
            for (int i = 0; i < (columnsLen); i++) {
                factorBoxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
            }
            return factorBoxColumnOpts;
        }

        return new SelectItem[0];
    }

    public SelectItem[] getNumericBoxColumnOpts() {
        String[] columns = PlottingUtils.GetNumericDataColumnsBoxPlt(sb);

        if (columns != null) {
            int columnsLen = columns.length;
            numericBoxColumnOpts = new SelectItem[columnsLen];
            List<String> columnNames = Arrays.asList(columns);
            for (int i = 0; i < (columnsLen); i++) {
                numericBoxColumnOpts[i] = new SelectItem(columnNames.get(i), columnNames.get(i));
            }
            return numericBoxColumnOpts;
        }

        return new SelectItem[0];
    }

    public String getLabx() {
        return labx;
    }

    public void setLabx(String labx) {
        this.labx = labx;
    }

    public String getLaby() {
        return laby;
    }

    public void setLaby(String laby) {
        this.laby = laby;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public Integer getChosenAxisTextSize() {
        return chosenAxisTextSize;
    }

    public void setChosenAxisTextSize(Integer chosenAxisTextSize) {
        this.chosenAxisTextSize = chosenAxisTextSize;
    }

    public Integer getChosenTitleTextSize() {
        return chosenTitleTextSize;
    }

    public void setChosenTitleTextSize(Integer chosenTitleTextSize) {
        this.chosenTitleTextSize = chosenTitleTextSize;
    }

    public SelectItem[] getTextFontSizeOpts() {
        return textFontSizeOpts;
    }

    public SelectItem[] getAxisFontSizeOpts() {
        return axisFontSizeOpts;
    }

    public boolean isData() {
        return data;
    }

    public void setData(boolean data) {
        this.data = data;
    }

    protected abstract void button_action();
}
