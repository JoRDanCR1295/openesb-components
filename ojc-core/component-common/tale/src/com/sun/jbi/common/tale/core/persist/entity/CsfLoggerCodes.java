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
@Table(name = "CSF_LOGGER_CODES")
@NamedQueries({@NamedQuery(name = "CsfLoggerCodes.findByLoggerCode", query = "SELECT c FROM CsfLoggerCodes c WHERE c.loggerCode = :loggerCode"), @NamedQuery(name = "CsfLoggerCodes.findByLoggerLabel", query = "SELECT c FROM CsfLoggerCodes c WHERE c.loggerLabel = :loggerLabel"), @NamedQuery(name = "CsfLoggerCodes.findByLoggerCategory", query = "SELECT c FROM CsfLoggerCodes c WHERE c.loggerCategory = :loggerCategory"), @NamedQuery(name = "CsfLoggerCodes.findByLoggerLevel", query = "SELECT c FROM CsfLoggerCodes c WHERE c.loggerLevel = :loggerLevel"), @NamedQuery(name = "CsfLoggerCodes.findByLoggerDescription", query = "SELECT c FROM CsfLoggerCodes c WHERE c.loggerDescription = :loggerDescription"), @NamedQuery(name = "CsfLoggerCodes.findByLoggerChannelCode", query = "SELECT c FROM CsfLoggerCodes c WHERE c.loggerChannelCode = :loggerChannelCode"), @NamedQuery(name = "CsfLoggerCodes.findByActiveFlag", query = "SELECT c FROM CsfLoggerCodes c WHERE c.activeFlag = :activeFlag"), @NamedQuery(name = "CsfLoggerCodes.findByCreateId", query = "SELECT c FROM CsfLoggerCodes c WHERE c.createId = :createId"), @NamedQuery(name = "CsfLoggerCodes.findByCreateDateTime", query = "SELECT c FROM CsfLoggerCodes c WHERE c.createDateTime = :createDateTime"), @NamedQuery(name = "CsfLoggerCodes.findByLastModId", query = "SELECT c FROM CsfLoggerCodes c WHERE c.lastModId = :lastModId"), @NamedQuery(name = "CsfLoggerCodes.findByLastModDateTime", query = "SELECT c FROM CsfLoggerCodes c WHERE c.lastModDateTime = :lastModDateTime")})
public class CsfLoggerCodes implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "LOGGER_CODE", nullable = false)
    private Integer loggerCode;
    @Column(name = "LOGGER_LABEL")
    private String loggerLabel;
    @Column(name = "LOGGER_CATEGORY")
    private String loggerCategory;
    @Column(name = "LOGGER_LEVEL")
    private String loggerLevel;
    @Column(name = "LOGGER_DESCRIPTION")
    private String loggerDescription;
    @Column(name = "LOGGER_CHANNEL_CODE")
    private Integer loggerChannelCode;
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

    public CsfLoggerCodes() {
    }

    public CsfLoggerCodes(Integer loggerCode) {
        this.loggerCode = loggerCode;
    }

    public Integer getLoggerCode() {
        return loggerCode;
    }

    public void setLoggerCode(Integer loggerCode) {
        this.loggerCode = loggerCode;
    }

    public String getLoggerLabel() {
        return loggerLabel;
    }

    public void setLoggerLabel(String loggerLabel) {
        this.loggerLabel = loggerLabel;
    }

    public String getLoggerCategory() {
        return loggerCategory;
    }

    public void setLoggerCategory(String loggerCategory) {
        this.loggerCategory = loggerCategory;
    }

    public String getLoggerLevel() {
        return loggerLevel;
    }

    public void setLoggerLevel(String loggerLevel) {
        this.loggerLevel = loggerLevel;
    }

    public String getLoggerDescription() {
        return loggerDescription;
    }

    public void setLoggerDescription(String loggerDescription) {
        this.loggerDescription = loggerDescription;
    }

    public Integer getLoggerChannelCode() {
        return loggerChannelCode;
    }

    public void setLoggerChannelCode(Integer loggerChannelCode) {
        this.loggerChannelCode = loggerChannelCode;
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
        hash += (loggerCode != null ? loggerCode.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfLoggerCodes)) {
            return false;
        }
        CsfLoggerCodes other = (CsfLoggerCodes) object;
        if ((this.loggerCode == null && other.loggerCode != null) || (this.loggerCode != null && !this.loggerCode.equals(other.loggerCode))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfLoggerCodes[loggerCode=" + loggerCode + "]";
    }

}
