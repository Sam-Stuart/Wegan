/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package metaboanalyst.models;

import java.util.List;
import javax.faces.model.ListDataModel;
import org.primefaces.model.SelectableDataModel;

/**
 *
 * @author jianguox
 */
public class NetsDataModel extends ListDataModel<NetBean> implements SelectableDataModel<NetBean> {

    public NetsDataModel() {
    }

    public NetsDataModel(List<NetBean> data) {
        super(data);
    }

    public List<NetBean> getNetsData() {
        return (List<NetBean>) getWrappedData();
    }

    @Override
    public NetBean getRowData(String rowKey) {
        //In a real app, a more efficient way like a query by rowKey should be implemented to deal with huge data  
        List<NetBean> nets = (List<NetBean>) getWrappedData();
        for (NetBean net : nets) {
            if (net.getName().equals(rowKey)) {
                return net;
            }
        }
        return null;
    }

    @Override
    public Object getRowKey(NetBean net) {
        return net.getName();
    }

}
