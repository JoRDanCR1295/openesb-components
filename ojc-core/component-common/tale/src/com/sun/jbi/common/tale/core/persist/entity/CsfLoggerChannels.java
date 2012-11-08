/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;
import java.util.Date;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
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
@Table(name = "CSF_LOGGER_CHANNELS")
@NamedQueries({@NamedQuery(name = "CsfLoggerChannels.findByLoggerChannelCode", query = "SELECT c FROM CsfLoggerChannels c WHERE c.loggerChannelCode = :loggerChannelCode"), @NamedQuery(name = "CsfLoggerChannels.findByChannelType", query = "SELECT c FROM CsfLoggerChannels c WHERE c.channelType = :channelType"), @NamedQuery(name = "CsfLoggerChannels.findByFileName", query = "SELECT c FROM CsfLoggerChannels c WHERE c.fileName = :fileName"), @NamedQuery(name = "CsfLoggerChannels.findByActiveFlag", query = "SELECT c FROM CsfLoggerChannels c WHERE c.activeFlag = :activeFlag"), @NamedQuery(name = "CsfLoggerChannels.findByCreateId", query = "SELECT c FROM CsfLoggerChannels c WHERE c.createId = :createId"), @NamedQuery(name = "CsfLoggerChannels.findByCreateDateTime", query = "SELECT c FROM CsfLoggerChannels c WHERE c.createDateTime = :createDateTime"), @NamedQuery(name = "CsfLoggerChannels.findByLastModId", query = "SELECT c FROM CsfLoggerChannels c WHERE c.lastModId = :lastModId"), @NamedQuery(name = "CsfLoggerChannels.findByLastModDateTime", query = "SELECT c FROM CsfLoggerChannels c WHERE c.lastModDateTime = :lastModDateTime")})
public class CsfLoggerChannels implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "LOGGER_CHANNEL_CODE", nullable = false)
    private Integer loggerChannelCode;
    @Column(name = "CHANNEL_TYPE")
    private String channelType;
    @Column(name = "FILE_NAME")
    private String fileName;
    @Column(name = "ACTIVE_FLAG")
    private Character activeFlag;
    @Column(name = "CREATE_ID")
    private String createId;
    @Column(name = "CREATE_DATE_TIME")
    @Temporal(TemporalType.TIMESTAMP)
    private Date createDateTime;
    @Column(name = "LAST_MOD_ID")
    private String lastModId;
    @Column(name = "LAST_MOD_DATE_TIME")
    @Temporal(TemporalType.TIMESTAMP)
    private Date lastModDateTime;

    public CsfLoggerChannels() {
    }

    public CsfLoggerChannels(Integer loggerChannelCode) {
        this.loggerChannelCode = loggerChannelCode;
    }

    public Integer getLoggerChannelCode() {
        return loggerChannelCode;
    }

    public void setLoggerChannelCode(Integer loggerChannelCode) {
        this.loggerChannelCode = loggerChannelCode;
    }

    public String getChannelType() {
        return channelType;
    }

    public void setChannelType(String channelType) {
        this.channelType = channelType;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public Character getActiveFlag() {
        return activeFlag;
    }

    public void setActiveFlag(Character activeFlag) {
        this.activeFlag = activeFlag;
    }

    public String getCreateId() {
        return createId;
    }

    public void setCreateId(String createId) {
        this.createId = createId;
    }

    public Date getCreateDateTime() {
        return createDateTime;
    }

    public void setCreateDateTime(Date createDateTime) {
        this.createDateTime = createDateTime;
    }

    public String getLastModId() {
        return lastModId;
    }

    public void setLastModId(String lastModId) {
        this.lastModId = lastModId;
    }

    public Date getLastModDateTime() {
        return lastModDateTime;
    }

    public void setLastModDateTime(Date lastModDateTime) {
        this.lastModDateTime = lastModDateTime;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (loggerChannelCode != null ? loggerChannelCode.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfLoggerChannels)) {
            return false;
        }
        CsfLoggerChannels other = (CsfLoggerChannels) object;
        if ((this.loggerChannelCode == null && other.loggerChannelCode != null) || (this.loggerChannelCode != null && !this.loggerChannelCode.equals(other.loggerChannelCode))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfLoggerChannels[loggerChannelCode=" + loggerChannelCode + "]";
    }

}
