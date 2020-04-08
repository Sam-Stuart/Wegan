/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.controllers.meta;

import metaboanalyst.models.ColumnModel;
import metaboanalyst.models.MetaResultBean;
import metaboanalyst.rwrappers.RAnalUtils;
import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import javax.faces.application.FacesMessage;
import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.faces.context.FacesContext;
import javax.faces.model.SelectItem;
import metaboanalyst.controllers.SessionBean1;
import metaboanalyst.utils.DataUtils;
import org.rosuda.REngine.Rserve.RConnection;

/**
 *
 * @author jianguox
 */
@ManagedBean(name = "metaResBean")
@ViewScoped
public class MetaResBean implements Serializable {

    private final SessionBean1 sb = (SessionBean1) DataUtils.findBean("sessionBean1");

    RConnection RC = sb.getRConnection();

    private List<MetaResultBean> resBeans;
    private List<ColumnModel> columns = new ArrayList<>();

    public MetaResBean() {
        if (resBeans == null) {
            resBeans = new ArrayList<>();
            populateResBeans();
        }
    }

    private String[] ids = null;

    private void populateResBeans() {
        resBeans.clear();
        columns.clear();

        String[] columnKeys = RAnalUtils.getMetaResColNames(RC);
        ids = RAnalUtils.getMetaResGeneIDs(RC);
        double[][] resMat = RAnalUtils.getMetaResMatrix(RC, indField);
        columns.add(new ColumnModel("ID", "ID", "string")); //add entrez ID column
        for (int n = 0; n < columnKeys.length; n++) {
            String columnKey = columnKeys[n];
            columns.add(new ColumnModel(columnKey, columnKey, "double"));
        }

        tableFields = new SelectItem[columns.size()];
        for (int k = 0; k < tableFields.length; k++) {
            ColumnModel cm = columns.get(k);
            tableFields[k] = new SelectItem(cm.getHeader(), cm.getHeader() + "  ");
        }
        sortField = tableFields[tableFields.length - 1].getValue().toString();

        //set up object list
        // due to memory/performance issue, cannot display more than 5000 sig genes
        // otherwise java.lang.ArrayIndexOutOfBoundsException: 5000
        if (ids != null && ids.length > 0) {
            for (int i = 0; i < ids.length; i++) {
                //we use gene symbol as ID for sorting purpose as actual 
                //gene symbol is hyper link
                if (i >= 5000) {
                    break;
                }
                MetaResultBean rb = new MetaResultBean(ids[i]);
                rb.setName(ids[i]);
                rb.setValue("ID", ids[i]);
                for (int m = 0; m < columnKeys.length; m++) {
                    rb.setValue(columnKeys[m], resMat[i][m] + "");
                }
                resBeans.add(rb);
            }
        }
    }

    public List<MetaResultBean> getResBeans() {
        return resBeans;
    }

    public List<ColumnModel> getColumns() {
        return columns;
    }

    private String sortField;

    public String getSortField() {
        return sortField;
    }

    public void setSortField(String sortField) {
        if (sortField != null) {
            this.sortField = sortField;
        }
    }
    private String sortOrder = "asc";

    public String getSortOrder() {
        return sortOrder;
    }

    public void setSortOrder(String sortOrder) {
        if (sortOrder != null) {
            this.sortOrder = sortOrder;
        }
    }
    private SelectItem[] tableFields;

    public SelectItem[] getTableFields() {
        return tableFields;
    }
    private MetaResultBean selectedFeature;

    public MetaResultBean getSelectedFeature() {
        return selectedFeature;
    }

    public void setSelectedFeature(MetaResultBean selectedData) {
        this.selectedFeature = selectedData;
        String geneID = selectedFeature.getID();
        RAnalUtils.plotSelectedFeature(RC, geneID);
        currentFeatureImg = sb.getCurrentUser().getRelativeDir() + File.separator + geneID + ".png";
    }

    private String currentFeatureImg;

    public String getCurrentFeatureImg() {
        return currentFeatureImg;
    }

    private String indField = "fc";

    public String getIndField() {
        return indField;
    }

    public void setIndField(String indField) {
        if (indField != null && !this.indField.equals(indField)) {
            this.indField = indField;
            populateResBeans();
        }
    }

    //sort table based on user selected field and order
    public void updateTable() {

        //first found it is number or string
        String type = "string";
        for (int k = 0; k < columns.size(); k++) {
            ColumnModel cm = columns.get(k);
            if (cm.getHeader().equals(sortField)) {
                type = cm.getType();
                break;
            }
        }
        if (type.equalsIgnoreCase("string")) {
            if (sortField.equalsIgnoreCase("ID")) {
                Collections.sort(resBeans, MyLinkCOMPARATOR);
            } else {
                Collections.sort(resBeans, MyStringCOMPARATOR);
            }
        } else {
            Collections.sort(resBeans, MyNumberCOMPARATOR);
        }
    }

    //String compare
    private Comparator<MetaResultBean> MyStringCOMPARATOR = new Comparator<MetaResultBean>() {
        @Override
        public int compare(MetaResultBean rb1, MetaResultBean rb2) {
            try {
                Object value1 = rb1.getValue(sortField);
                Object value2 = rb2.getValue(sortField);
                int value = ((Comparable) value1).compareTo(value2);
                return sortOrder.equals("asc") ? value : -1 * value;
            } catch (Exception e) {
                throw new RuntimeException();
            }
        }
    };
    //hyerlink then use its value ID
    private Comparator<MetaResultBean> MyLinkCOMPARATOR = new Comparator<MetaResultBean>() {
        @Override
        public int compare(MetaResultBean rb1, MetaResultBean rb2) {
            try {
                Object value1 = rb1.getName();
                Object value2 = rb2.getName();
                //System.out.println(value1 + " " + value2 + "========");
                int value = ((Comparable) value1).compareTo(value2);
                return sortOrder.equals("asc") ? value : -1 * value;
            } catch (Exception e) {
                throw new RuntimeException();
            }
        }
    };
    private final Comparator<MetaResultBean> MyNumberCOMPARATOR = new Comparator<MetaResultBean>() {
        @Override
        public int compare(MetaResultBean rb1, MetaResultBean rb2) {
            try {
                Double value1 = Double.parseDouble(rb1.getValue(sortField));
                Double value2 = Double.parseDouble(rb2.getValue(sortField));
                int value = value1.compareTo(value2);
                return sortOrder.equals("asc") ? value : -1 * value;
            } catch (Exception e) {
                throw new RuntimeException();
            }
        }
    };

    private String searchTerm = "";

    public String getSearchTerm() {
        return searchTerm;
    }

    public void setSearchTerm(String searchTerm) {
        this.searchTerm = searchTerm;
    }

    public void searchTable() {
        searchTerm = searchTerm.trim();

        int index = -1;
        for (int i = 0; i < resBeans.size(); i++) {
            if (resBeans.get(i).getName().equalsIgnoreCase(searchTerm)) {
                index = i;
                break;
            }
        }
        if (index == -1) {
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_INFO, "No result", "Make sure the feature name is spelled correctly."));
        } else {
            MetaResultBean res = resBeans.get(index);
            resBeans.remove(index);
            resBeans.add(0, res);
            FacesContext.getCurrentInstance().addMessage(null,
                    new FacesMessage(FacesMessage.SEVERITY_INFO, "ID Found", "The feature is ranked first in the table."));
        }
    }

}
