/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package bpelmonitor.model;

//import bpelmonitor.model.ActivityStatus.InstanceStatus;
import java.io.Serializable;
import javax.xml.namespace.QName;

/**
 *
 * @author mbhasin
 */
public class InstanceFilter implements Serializable {

    private QName bpName;
    private InstanceStatus2 status;
    private String subFilter;

    public InstanceFilter(QName businessProcessQName) {
        this.bpName = businessProcessQName;
    }

    /**
     * @return the bpName
     */
    public QName getBpName() {
        return bpName;
    }

    /**
     * @param bpName the bpName to set
     */
    public void setBpName(QName bpName) {
        this.bpName = bpName;
    }

    /**
     * @return the status
     */
    public InstanceStatus2 getStatus() {
        return status;
    }

    public void setStatus(InstanceStatus2 instanceStatus) {
        this.status = instanceStatus;
    }

    /**
     * @return the bpNameAsString
     */
    public String getBpNameAsString() {
        return bpName.toString();
    }

    /**
     * @return the subFilter
     */
    public String getSubFilter() {
        return subFilter;
    }

    /**
     * @param subFilter the subFilter to set
     */
    public void setSubFilter(String subFilter) {
        this.subFilter = subFilter;
    }

    public static enum InstanceStatus2 {
        RUNNING,
        COMPLETED,
        FAULTED,
        SUSPENDED,
        TERMINATED
    };
}


