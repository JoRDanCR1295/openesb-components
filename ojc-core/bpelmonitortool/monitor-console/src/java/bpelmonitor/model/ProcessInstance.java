/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package bpelmonitor.model;

import java.io.Serializable;
import javax.xml.namespace.QName;

/**
 *
 * @author mbhasin
 */
public class ProcessInstance implements Serializable {
    private String id;
    private String businessProcessId;
    private QName businessProcessQName;
    private String startTimestamp;
    private String lastUpdateTimestamp;
    private String completedTimestamp;
    private String status;

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @param id the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the businessProcessId
     */
    public String getBusinessProcessId() {
        return businessProcessId;
    }

    /**
     * @param businessProcessId the businessProcessId to set
     */
    public void setBusinessProcessId(String businessProcessId) {
        this.businessProcessId = businessProcessId;
    }

    /**
     * @return the startTimestamp
     */
    public String getStartTimestamp() {
        return startTimestamp;
    }

    /**
     * @param startTimestamp the startTimestamp to set
     */
    public void setStartTimestamp(String startTimestamp) {
        this.startTimestamp = startTimestamp;
    }

    /**
     * @return the lastUpdateTimestamp
     */
    public String getLastUpdateTimestamp() {
        return lastUpdateTimestamp;
    }

    /**
     * @param lastUpdateTimestamp the lastUpdateTimestamp to set
     */
    public void setLastUpdateTimestamp(String lastUpdateTimestamp) {
        this.lastUpdateTimestamp = lastUpdateTimestamp;
    }

    /**
     * @return the completedTimestamp
     */
    public String getCompletedTimestamp() {
        return completedTimestamp;
    }

    /**
     * @param completedTimestamp the completedTimestamp to set
     */
    public void setCompletedTimestamp(String completedTimestamp) {
        this.completedTimestamp = completedTimestamp;
    }

    /**
     * @return the status
     */
    public String getStatus() {
        return status;
    }

    /**
     * @param status the status to set
     */
    public void setStatus(String status) {
        this.status = status;
    }

    /**
     * @return the businessProcessQName
     */
    public QName getBusinessProcessQName() {
        return businessProcessQName;
    }

    /**
     * @param businessProcessQName the businessProcessQName to set
     */
    public void setBusinessProcessQName(QName businessProcessQName) {
        this.businessProcessQName = businessProcessQName;
    }
}
