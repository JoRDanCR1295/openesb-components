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
public class DashboardEntry implements Serializable {

    private String applicaitonName;
    private String businessProcessName;
    private QName businessProcessQName;
    private String liveInstnces;
    private String completedInstances;
    private String lastInstanceProcessedTime;
    private String suspendedInstances;
    private String faultedInstances;
    private String terminatedInstances;
    private String kpi;
    private boolean selected;

    /**
     * @return the applicaitonName
     */
    public String getApplicaitonName() {
        return applicaitonName;
    }

    /**
     * @param applicaitonName the applicaitonName to set
     */
    public void setApplicaitonName(String applicaitonName) {
        this.applicaitonName = applicaitonName;
    }

    /**
     * @return the businessProcessName
     */
    public String getBusinessProcessName() {
        return businessProcessName;
    }

    /**
     * @param businessProcessName the businessProcessName to set
     */
    public void setBusinessProcessName(String businessProcessName) {
        this.businessProcessName = businessProcessName;
    }

    /**
     * @return the liveInstnces
     */
    public String getLiveInstnces() {
        return liveInstnces;
    }

    /**
     * @param liveInstnces the liveInstnces to set
     */
    public void setLiveInstnces(String liveInstnces) {
        this.liveInstnces = liveInstnces;
    }

    /**
     * @return the suspendedInstances
     */
    public String getSuspendedInstances() {
        return suspendedInstances;
    }

    /**
     * @param suspendedInstances the suspendedInstances to set
     */
    public void setSuspendedInstances(String suspendedInstances) {
        this.suspendedInstances = suspendedInstances;
    }

    /**
     * @return the KPI
     */
    public String getKpi() {
        return kpi;
    }

    /**
     * @param KPI the KPI to set
     */
    public void setKpi(String kpi) {
        this.kpi = kpi;
    }

    /**
     * @return the faultedInstances
     */
    public String getFaultedInstances() {
        return faultedInstances;
    }

    /**
     * @param faultedInstances the faultedInstances to set
     */
    public void setFaultedInstances(String faultedInstances) {
        this.faultedInstances = faultedInstances;
    }

    /**
     * @return the terminatedInstances
     */
    public String getTerminatedInstances() {
        return terminatedInstances;
    }

    /**
     * @param terminatedInstances the terminatedInstances to set
     */
    public void setTerminatedInstances(String terminatedInstances) {
        this.terminatedInstances = terminatedInstances;
    }

    /**
     * @return the lastInstanceProcessedTime
     */
    public String getLastInstanceProcessedTime() {
        return lastInstanceProcessedTime;
    }

    /**
     * @param lastInstanceProcessedTime the lastInstanceProcessedTime to set
     */
    public void setLastInstanceProcessedTime(String lastInstanceProcessedTime) {
        this.lastInstanceProcessedTime = lastInstanceProcessedTime;
    }

    /**
     * @return the selected
     */
    public boolean isSelected() {
        return selected;
    }

    /**
     * @param selected the selected to set
     */
    public void setSelected(boolean selected) {
        this.selected = selected;
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

    /**
     * @return the completedInstances
     */
    public String getCompletedInstances() {
        return completedInstances;
    }

    /**
     * @param completedInstances the completedInstances to set
     */
    public void setCompletedInstances(String completedInstances) {
        this.completedInstances = completedInstances;
    }

}
