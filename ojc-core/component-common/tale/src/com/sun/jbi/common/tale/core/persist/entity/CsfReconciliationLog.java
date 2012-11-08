/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;
import java.util.Date;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "CSF_RECONCILIATION_LOG")
@NamedQueries({@NamedQuery(name = "CsfReconciliationLog.findByAppName", query = "SELECT c FROM CsfReconciliationLog c WHERE c.csfReconciliationLogPK.appName = :appName"), @NamedQuery(name = "CsfReconciliationLog.findByAppMesgId", query = "SELECT c FROM CsfReconciliationLog c WHERE c.csfReconciliationLogPK.appMesgId = :appMesgId"), @NamedQuery(name = "CsfReconciliationLog.findByUnitName", query = "SELECT c FROM CsfReconciliationLog c WHERE c.csfReconciliationLogPK.unitName = :unitName"), @NamedQuery(name = "CsfReconciliationLog.findByProjectName", query = "SELECT c FROM CsfReconciliationLog c WHERE c.projectName = :projectName"), @NamedQuery(name = "CsfReconciliationLog.findByModuleName", query = "SELECT c FROM CsfReconciliationLog c WHERE c.moduleName = :moduleName"), @NamedQuery(name = "CsfReconciliationLog.findByAppType", query = "SELECT c FROM CsfReconciliationLog c WHERE c.appType = :appType"), @NamedQuery(name = "CsfReconciliationLog.findByServiceName", query = "SELECT c FROM CsfReconciliationLog c WHERE c.serviceName = :serviceName"), @NamedQuery(name = "CsfReconciliationLog.findByInCount", query = "SELECT c FROM CsfReconciliationLog c WHERE c.inCount = :inCount"), @NamedQuery(name = "CsfReconciliationLog.findByCopyCount", query = "SELECT c FROM CsfReconciliationLog c WHERE c.copyCount = :copyCount"), @NamedQuery(name = "CsfReconciliationLog.findByCreateCount", query = "SELECT c FROM CsfReconciliationLog c WHERE c.createCount = :createCount"), @NamedQuery(name = "CsfReconciliationLog.findByOutCount", query = "SELECT c FROM CsfReconciliationLog c WHERE c.outCount = :outCount"), @NamedQuery(name = "CsfReconciliationLog.findByFilterCount", query = "SELECT c FROM CsfReconciliationLog c WHERE c.filterCount = :filterCount"), @NamedQuery(name = "CsfReconciliationLog.findByErrorCount", query = "SELECT c FROM CsfReconciliationLog c WHERE c.errorCount = :errorCount"), @NamedQuery(name = "CsfReconciliationLog.findByMesgDatestamp", query = "SELECT c FROM CsfReconciliationLog c WHERE c.mesgDatestamp = :mesgDatestamp"), @NamedQuery(name = "CsfReconciliationLog.findByAppDatestamp", query = "SELECT c FROM CsfReconciliationLog c WHERE c.appDatestamp = :appDatestamp"), @NamedQuery(name = "CsfReconciliationLog.findByInsertDateTime", query = "SELECT c FROM CsfReconciliationLog c WHERE c.insertDateTime = :insertDateTime")})
public class CsfReconciliationLog implements Serializable {
    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected CsfReconciliationLogPK csfReconciliationLogPK;
    @Column(name = "PROJECT_NAME")
    private String projectName;
    @Column(name = "MODULE_NAME")
    private String moduleName;
    @Column(name = "APP_TYPE")
    private String appType;
    @Column(name = "SERVICE_NAME")
    private String serviceName;
    @Column(name = "IN_COUNT")
    private Integer inCount;
    @Column(name = "COPY_COUNT")
    private Integer copyCount;
    @Column(name = "CREATE_COUNT")
    private Integer createCount;
    @Column(name = "OUT_COUNT")
    private Integer outCount;
    @Column(name = "FILTER_COUNT")
    private Integer filterCount;
    @Column(name = "ERROR_COUNT")
    private Integer errorCount;
    @Column(name = "MESG_DATESTAMP")
    @Temporal(TemporalType.TIMESTAMP)
    private Date mesgDatestamp;
    @Column(name = "APP_DATESTAMP")
    @Temporal(TemporalType.TIMESTAMP)
    private Date appDatestamp;
    @Column(name = "INSERT_DATE_TIME")
    @Temporal(TemporalType.TIMESTAMP)
    private Date insertDateTime;

    public CsfReconciliationLog() {
    }

    public CsfReconciliationLog(CsfReconciliationLogPK csfReconciliationLogPK) {
        this.csfReconciliationLogPK = csfReconciliationLogPK;
    }

    public CsfReconciliationLog(String appName, String appMesgId, String unitName) {
        this.csfReconciliationLogPK = new CsfReconciliationLogPK(appName, appMesgId, unitName);
    }

    public CsfReconciliationLogPK getCsfReconciliationLogPK() {
        return csfReconciliationLogPK;
    }

    public void setCsfReconciliationLogPK(CsfReconciliationLogPK csfReconciliationLogPK) {
        this.csfReconciliationLogPK = csfReconciliationLogPK;
    }

    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public String getModuleName() {
        return moduleName;
    }

    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }

    public String getAppType() {
        return appType;
    }

    public void setAppType(String appType) {
        this.appType = appType;
    }

    public String getServiceName() {
        return serviceName;
    }

    public void setServiceName(String serviceName) {
        this.serviceName = serviceName;
    }

    public Integer getInCount() {
        return inCount;
    }

    public void setInCount(Integer inCount) {
        this.inCount = inCount;
    }

    public Integer getCopyCount() {
        return copyCount;
    }

    public void setCopyCount(Integer copyCount) {
        this.copyCount = copyCount;
    }

    public Integer getCreateCount() {
        return createCount;
    }

    public void setCreateCount(Integer createCount) {
        this.createCount = createCount;
    }

    public Integer getOutCount() {
        return outCount;
    }

    public void setOutCount(Integer outCount) {
        this.outCount = outCount;
    }

    public Integer getFilterCount() {
        return filterCount;
    }

    public void setFilterCount(Integer filterCount) {
        this.filterCount = filterCount;
    }

    public Integer getErrorCount() {
        return errorCount;
    }

    public void setErrorCount(Integer errorCount) {
        this.errorCount = errorCount;
    }

    public Date getMesgDatestamp() {
        return mesgDatestamp;
    }

    public void setMesgDatestamp(Date mesgDatestamp) {
        this.mesgDatestamp = mesgDatestamp;
    }

    public Date getAppDatestamp() {
        return appDatestamp;
    }

    public void setAppDatestamp(Date appDatestamp) {
        this.appDatestamp = appDatestamp;
    }

    public Date getInsertDateTime() {
        return insertDateTime;
    }

    public void setInsertDateTime(Date insertDateTime) {
        this.insertDateTime = insertDateTime;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (csfReconciliationLogPK != null ? csfReconciliationLogPK.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfReconciliationLog)) {
            return false;
        }
        CsfReconciliationLog other = (CsfReconciliationLog) object;
        if ((this.csfReconciliationLogPK == null && other.csfReconciliationLogPK != null) || (this.csfReconciliationLogPK != null && !this.csfReconciliationLogPK.equals(other.csfReconciliationLogPK))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfReconciliationLog[csfReconciliationLogPK=" + csfReconciliationLogPK + "]";
    }

}
