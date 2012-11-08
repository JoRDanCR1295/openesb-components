/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.UUID;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "CSF_CME_LOG")
@NamedQueries({@NamedQuery(name = "CsfCmeLog.findByMesgId", query = "SELECT c FROM CsfCmeLog c WHERE c.mesgId = :mesgId"), @NamedQuery(name = "CsfCmeLog.findByMesgDatestamp", query = "SELECT c FROM CsfCmeLog c WHERE c.mesgDatestamp = :mesgDatestamp"), @NamedQuery(name = "CsfCmeLog.findByAppMesgId", query = "SELECT c FROM CsfCmeLog c WHERE c.appMesgId = :appMesgId"), @NamedQuery(name = "CsfCmeLog.findByAppDatestamp", query = "SELECT c FROM CsfCmeLog c WHERE c.appDatestamp = :appDatestamp"), @NamedQuery(name = "CsfCmeLog.findByProjectName", query = "SELECT c FROM CsfCmeLog c WHERE c.projectName = :projectName"), @NamedQuery(name = "CsfCmeLog.findByAppType", query = "SELECT c FROM CsfCmeLog c WHERE c.appType = :appType"), @NamedQuery(name = "CsfCmeLog.findByAppName", query = "SELECT c FROM CsfCmeLog c WHERE c.appName = :appName"), @NamedQuery(name = "CsfCmeLog.findByServiceName", query = "SELECT c FROM CsfCmeLog c WHERE c.serviceName = :serviceName"), @NamedQuery(name = "CsfCmeLog.findByModuleName", query = "SELECT c FROM CsfCmeLog c WHERE c.moduleName = :moduleName"), @NamedQuery(name = "CsfCmeLog.findByUnitName", query = "SELECT c FROM CsfCmeLog c WHERE c.unitName = :unitName"), @NamedQuery(name = "CsfCmeLog.findByInstanceName", query = "SELECT c FROM CsfCmeLog c WHERE c.instanceName = :instanceName"), @NamedQuery(name = "CsfCmeLog.findByInsertDateTime", query = "SELECT c FROM CsfCmeLog c WHERE c.insertDateTime = :insertDateTime"), @NamedQuery(name = "CsfCmeLog.findByServerType", query = "SELECT c FROM CsfCmeLog c WHERE c.serverType = :serverType"), @NamedQuery(name = "CsfCmeLog.findByServerName", query = "SELECT c FROM CsfCmeLog c WHERE c.serverName = :serverName"), @NamedQuery(name = "CsfCmeLog.findByHostName", query = "SELECT c FROM CsfCmeLog c WHERE c.hostName = :hostName"), @NamedQuery(name = "CsfCmeLog.findByLogicalhostName", query = "SELECT c FROM CsfCmeLog c WHERE c.logicalhostName = :logicalhostName"), @NamedQuery(name = "CsfCmeLog.findByEnvironmentName", query = "SELECT c FROM CsfCmeLog c WHERE c.environmentName = :environmentName"), @NamedQuery(name = "CsfCmeLog.findByComponentType", query = "SELECT c FROM CsfCmeLog c WHERE c.componentType = :componentType"), @NamedQuery(name = "CsfCmeLog.findByComponentName", query = "SELECT c FROM CsfCmeLog c WHERE c.componentName = :componentName")})
public class CsfCmeLog implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "MESG_ID", nullable = false)
    private String mesgId;
    @Column(name = "MESG_DATESTAMP")
    @Temporal(TemporalType.TIMESTAMP)
    private Date mesgDatestamp;
    @Column(name = "APP_MESG_ID")
    private String appMesgId;
    @Column(name = "APP_DATESTAMP")
    @Temporal(TemporalType.TIMESTAMP)
    private Date appDatestamp;
    @Column(name = "PROJECT_NAME")
    private String projectName;
    @Column(name = "APP_TYPE", nullable = false)
    private String appType;
    @Column(name = "APP_NAME")
    private String appName;
    @Column(name = "SERVICE_NAME", nullable = false)
    private String serviceName;
    @Column(name = "MODULE_NAME", nullable = false)
    private String moduleName;
    @Column(name = "UNIT_NAME", nullable = false)
    private String unitName;
    @Column(name = "INSTANCE_NAME")
    private String instanceName;    
    @Column(name = "INSERT_DATE_TIME")
    @Temporal(TemporalType.TIMESTAMP)
    private Date insertDateTime;
    @Column(name = "SERVER_TYPE")
    private String serverType;
    @Column(name = "SERVER_NAME")
    private String serverName;
    @Column(name = "HOST_NAME")
    private String hostName;
    @Column(name = "LOGICALHOST_NAME")
    private String logicalhostName;
    @Column(name = "ENVIRONMENT_NAME")
    private String environmentName;
    @Column(name = "COMPONENT_TYPE")
    private String componentType;
    @Column(name = "COMPONENT_NAME")
    private String componentName;

    /* start relationships */
    @OneToMany(mappedBy="csfCmeLog", cascade = { CascadeType.ALL })
    private Collection<MsgTracking> msgTrackings = new HashSet<MsgTracking> ();
    
    @OneToOne(mappedBy="csfCmeLog", cascade = { CascadeType.ALL })
    private ExchangeInfo exchangeInfo;
    
    @OneToOne(mappedBy="csfCmeLog", cascade = { CascadeType.ALL })
    private CsfLoggerLog csfLoggerLog;
    
    @OneToOne(mappedBy="csfCmeLog", cascade = { CascadeType.ALL })
    private CsfPayloadLog csfPayloadLog;
    
    @OneToOne(mappedBy="csfCmeLog", cascade = { CascadeType.ALL })
    private CsfAlerterLog csfAlerterLog;
    
    @OneToOne(mappedBy="csfCmeLog", cascade = { CascadeType.ALL })
    private CsfErrorLog csfErrorLog;
    
    @OneToOne(mappedBy="csfCmeLog", cascade = { CascadeType.ALL })
    private CsfUserAuditLog csfUserAuditLog;
    
    @OneToMany(mappedBy="csfCmeLog", cascade = { CascadeType.ALL })
    private Collection<CsfUserFields> csfUserFieldss = new HashSet<CsfUserFields> ();
    
    /* end relationships */
    
    public CsfCmeLog() {
    }

    public CsfCmeLog(String appType, String serviceName, String moduleName, String unitName) {
        this.mesgId = UUID.randomUUID().toString();
        this.appType = appType;
        this.serviceName = serviceName;
        this.moduleName = moduleName;
        this.unitName = unitName;
    }

    public String getMesgId() {
        return mesgId;
    }

    public void setMesgId(String mesgId) {
        this.mesgId = mesgId;
    }

    public Date getMesgDatestamp() {
        return mesgDatestamp;
    }

    public void setMesgDatestamp(Date mesgDatestamp) {
        this.mesgDatestamp = mesgDatestamp;
    }

    public String getAppMesgId() {
        return appMesgId;
    }

    public void setAppMesgId(String appMesgId) {
        this.appMesgId = appMesgId;
    }

    public Date getAppDatestamp() {
        return appDatestamp;
    }

    public void setAppDatestamp(Date appDatestamp) {
        this.appDatestamp = appDatestamp;
    }

    public String getProjectName() {
        return projectName;
    }

    public void setProjectName(String projectName) {
        this.projectName = projectName;
    }

    public String getAppType() {
        return appType;
    }

    public void setAppType(String appType) {
        this.appType = appType;
    }

    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public String getServiceName() {
        return serviceName;
    }

    public void setServiceName(String serviceName) {
        this.serviceName = serviceName;
    }

    public String getModuleName() {
        return moduleName;
    }

    public void setModuleName(String moduleName) {
        this.moduleName = moduleName;
    }

    public String getUnitName() {
        return unitName;
    }

    public void setUnitName(String unitName) {
        this.unitName = unitName;
    }

    public String getInstanceName() {
        return instanceName;
    }

    public void setInstanceName(String instanceName) {
        this.instanceName = instanceName;
    }

    public Date getInsertDateTime() {
        return insertDateTime;
    }

    public void setInsertDateTime(Date insertDateTime) {
        this.insertDateTime = insertDateTime;
    }

    public String getServerType() {
        return serverType;
    }

    public void setServerType(String serverType) {
        this.serverType = serverType;
    }

    public String getServerName() {
        return serverName;
    }

    public void setServerName(String serverName) {
        this.serverName = serverName;
    }

    public String getHostName() {
        return hostName;
    }

    public void setHostName(String hostName) {
        this.hostName = hostName;
    }

    public String getLogicalhostName() {
        return logicalhostName;
    }

    public void setLogicalhostName(String logicalhostName) {
        this.logicalhostName = logicalhostName;
    }

    public String getEnvironmentName() {
        return environmentName;
    }

    public void setEnvironmentName(String environmentName) {
        this.environmentName = environmentName;
    }

    public String getComponentType() {
        return componentType;
    }

    public void setComponentType(String componentType) {
        this.componentType = componentType;
    }

    public String getComponentName() {
        return componentName;
    }

    public void setComponentName(String componentName) {
        this.componentName = componentName;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (mesgId != null ? mesgId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfCmeLog)) {
            return false;
        }
        CsfCmeLog other = (CsfCmeLog) object;
        if ((this.mesgId == null && other.mesgId != null) || (this.mesgId != null && !this.mesgId.equals(other.mesgId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfCmeLog[mesgId=" + mesgId + "]";
    }

    /**
     * @return the msgTrackings
     */
    public Collection<MsgTracking> getMsgTrackings() {
        return msgTrackings;
    }

    /**
     * @param msgTrackings the msgTrackings to set
     */
    public void setMsgTrackings(Collection<MsgTracking> msgTrackings) {
        this.msgTrackings = msgTrackings;
    }

    /**
     * @return the exchangeInfo
     */
    public ExchangeInfo getExchangeInfo() {
        return exchangeInfo;
    }

    /**
     * @param exchangeInfo the exchangeInfo to set
     */
    public void setExchangeInfo(ExchangeInfo exchangeInfo) {
        this.exchangeInfo = exchangeInfo;
    }

    /**
     * @return the csfLoggerLog
     */
    public CsfLoggerLog getCsfLoggerLog() {
        return csfLoggerLog;
    }

    /**
     * @param csfLoggerLog the csfLoggerLog to set
     */
    public void setCsfLoggerLog(CsfLoggerLog csfLoggerLog) {
        this.csfLoggerLog = csfLoggerLog;
    }

    /**
     * @return the csfPayloadLog
     */
    public CsfPayloadLog getCsfPayloadLog() {
        return csfPayloadLog;
    }

    /**
     * @param csfPayloadLog the csfPayloadLog to set
     */
    public void setCsfPayloadLog(CsfPayloadLog csfPayloadLog) {
        this.csfPayloadLog = csfPayloadLog;
    }

    /**
     * @return the csfAlerterLog
     */
    public CsfAlerterLog getCsfAlerterLog() {
        return csfAlerterLog;
    }

    /**
     * @param csfAlerterLog the csfAlerterLog to set
     */
    public void setCsfAlerterLog(CsfAlerterLog csfAlerterLog) {
        this.csfAlerterLog = csfAlerterLog;
    }

    /**
     * @return the csfErrorLog
     */
    public CsfErrorLog getCsfErrorLog() {
        return csfErrorLog;
    }

    /**
     * @param csfErrorLog the csfErrorLog to set
     */
    public void setCsfErrorLog(CsfErrorLog csfErrorLog) {
        this.csfErrorLog = csfErrorLog;
    }

    /**
     * @return the csfUserAuditLog
     */
    public CsfUserAuditLog getCsfUserAuditLog() {
        return csfUserAuditLog;
    }

    /**
     * @param csfUserAuditLog the csfUserAuditLog to set
     */
    public void setCsfUserAuditLog(CsfUserAuditLog csfUserAuditLog) {
        this.csfUserAuditLog = csfUserAuditLog;
    }

    /**
     * @return the csfUserFieldss
     */
    public Collection<CsfUserFields> getCsfUserFieldss() {
        return csfUserFieldss;
    }

    /**
     * @param csfUserFieldss the csfUserFieldss to set
     */
    public void setCsfUserFieldss(Collection<CsfUserFields> csfUserFieldss) {
        this.csfUserFieldss = csfUserFieldss;
    }

}
