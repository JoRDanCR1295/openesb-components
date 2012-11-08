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
@Table(name = "CSF_ERROR_CODES")
@NamedQueries({@NamedQuery(name = "CsfErrorCodes.findByErrorCode", query = "SELECT c FROM CsfErrorCodes c WHERE c.errorCode = :errorCode"), @NamedQuery(name = "CsfErrorCodes.findByErrorLabel", query = "SELECT c FROM CsfErrorCodes c WHERE c.errorLabel = :errorLabel"), @NamedQuery(name = "CsfErrorCodes.findByErrorCategory", query = "SELECT c FROM CsfErrorCodes c WHERE c.errorCategory = :errorCategory"), @NamedQuery(name = "CsfErrorCodes.findByErrorLevel", query = "SELECT c FROM CsfErrorCodes c WHERE c.errorLevel = :errorLevel"), @NamedQuery(name = "CsfErrorCodes.findByErrorDescription", query = "SELECT c FROM CsfErrorCodes c WHERE c.errorDescription = :errorDescription"), @NamedQuery(name = "CsfErrorCodes.findByAuthorizeFlag", query = "SELECT c FROM CsfErrorCodes c WHERE c.authorizeFlag = :authorizeFlag"), @NamedQuery(name = "CsfErrorCodes.findByLoggerFlag", query = "SELECT c FROM CsfErrorCodes c WHERE c.loggerFlag = :loggerFlag"), @NamedQuery(name = "CsfErrorCodes.findByAlerterFlag", query = "SELECT c FROM CsfErrorCodes c WHERE c.alerterFlag = :alerterFlag"), @NamedQuery(name = "CsfErrorCodes.findByReplayFlag", query = "SELECT c FROM CsfErrorCodes c WHERE c.replayFlag = :replayFlag"), @NamedQuery(name = "CsfErrorCodes.findByPersistFlag", query = "SELECT c FROM CsfErrorCodes c WHERE c.persistFlag = :persistFlag"), @NamedQuery(name = "CsfErrorCodes.findByEncodeFlag", query = "SELECT c FROM CsfErrorCodes c WHERE c.encodeFlag = :encodeFlag"), @NamedQuery(name = "CsfErrorCodes.findByAlerterCode", query = "SELECT c FROM CsfErrorCodes c WHERE c.alerterCode = :alerterCode"), @NamedQuery(name = "CsfErrorCodes.findByLoggerCode", query = "SELECT c FROM CsfErrorCodes c WHERE c.loggerCode = :loggerCode"), @NamedQuery(name = "CsfErrorCodes.findByPersistMode", query = "SELECT c FROM CsfErrorCodes c WHERE c.persistMode = :persistMode"), @NamedQuery(name = "CsfErrorCodes.findByEncodeMode", query = "SELECT c FROM CsfErrorCodes c WHERE c.encodeMode = :encodeMode"), @NamedQuery(name = "CsfErrorCodes.findByActiveFlag", query = "SELECT c FROM CsfErrorCodes c WHERE c.activeFlag = :activeFlag"), @NamedQuery(name = "CsfErrorCodes.findByCreateId", query = "SELECT c FROM CsfErrorCodes c WHERE c.createId = :createId"), @NamedQuery(name = "CsfErrorCodes.findByCreateDateTime", query = "SELECT c FROM CsfErrorCodes c WHERE c.createDateTime = :createDateTime"), @NamedQuery(name = "CsfErrorCodes.findByLastModId", query = "SELECT c FROM CsfErrorCodes c WHERE c.lastModId = :lastModId"), @NamedQuery(name = "CsfErrorCodes.findByLastModDateTime", query = "SELECT c FROM CsfErrorCodes c WHERE c.lastModDateTime = :lastModDateTime")})
public class CsfErrorCodes implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "ERROR_CODE", nullable = false)
    private Integer errorCode;
    @Column(name = "ERROR_LABEL")
    private String errorLabel;
    @Column(name = "ERROR_CATEGORY")
    private String errorCategory;
    @Column(name = "ERROR_LEVEL")
    private String errorLevel;
    @Column(name = "ERROR_DESCRIPTION")
    private String errorDescription;
    @Column(name = "AUTHORIZE_FLAG")
    private Character authorizeFlag;
    @Column(name = "LOGGER_FLAG")
    private Character loggerFlag;
    @Column(name = "ALERTER_FLAG")
    private Character alerterFlag;
    @Column(name = "REPLAY_FLAG")
    private Character replayFlag;
    @Column(name = "PERSIST_FLAG")
    private Character persistFlag;
    @Column(name = "ENCODE_FLAG")
    private Character encodeFlag;
    @Column(name = "ALERTER_CODE", nullable = false)
    private int alerterCode;
    @Column(name = "LOGGER_CODE")
    private Integer loggerCode;
    @Column(name = "PERSIST_MODE")
    private String persistMode;
    @Column(name = "ENCODE_MODE")
    private String encodeMode;
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

    public CsfErrorCodes() {
    }

    public CsfErrorCodes(Integer errorCode) {
        this.errorCode = errorCode;
    }

    public CsfErrorCodes(Integer errorCode, int alerterCode) {
        this.errorCode = errorCode;
        this.alerterCode = alerterCode;
    }

    public Integer getErrorCode() {
        return errorCode;
    }

    public void setErrorCode(Integer errorCode) {
        this.errorCode = errorCode;
    }

    public String getErrorLabel() {
        return errorLabel;
    }

    public void setErrorLabel(String errorLabel) {
        this.errorLabel = errorLabel;
    }

    public String getErrorCategory() {
        return errorCategory;
    }

    public void setErrorCategory(String errorCategory) {
        this.errorCategory = errorCategory;
    }

    public String getErrorLevel() {
        return errorLevel;
    }

    public void setErrorLevel(String errorLevel) {
        this.errorLevel = errorLevel;
    }

    public String getErrorDescription() {
        return errorDescription;
    }

    public void setErrorDescription(String errorDescription) {
        this.errorDescription = errorDescription;
    }

    public Character getAuthorizeFlag() {
        return authorizeFlag;
    }

    public void setAuthorizeFlag(Character authorizeFlag) {
        this.authorizeFlag = authorizeFlag;
    }

    public Character getLoggerFlag() {
        return loggerFlag;
    }

    public void setLoggerFlag(Character loggerFlag) {
        this.loggerFlag = loggerFlag;
    }

    public Character getAlerterFlag() {
        return alerterFlag;
    }

    public void setAlerterFlag(Character alerterFlag) {
        this.alerterFlag = alerterFlag;
    }

    public Character getReplayFlag() {
        return replayFlag;
    }

    public void setReplayFlag(Character replayFlag) {
        this.replayFlag = replayFlag;
    }

    public Character getPersistFlag() {
        return persistFlag;
    }

    public void setPersistFlag(Character persistFlag) {
        this.persistFlag = persistFlag;
    }

    public Character getEncodeFlag() {
        return encodeFlag;
    }

    public void setEncodeFlag(Character encodeFlag) {
        this.encodeFlag = encodeFlag;
    }

    public int getAlerterCode() {
        return alerterCode;
    }

    public void setAlerterCode(int alerterCode) {
        this.alerterCode = alerterCode;
    }

    public Integer getLoggerCode() {
        return loggerCode;
    }

    public void setLoggerCode(Integer loggerCode) {
        this.loggerCode = loggerCode;
    }

    public String getPersistMode() {
        return persistMode;
    }

    public void setPersistMode(String persistMode) {
        this.persistMode = persistMode;
    }

    public String getEncodeMode() {
        return encodeMode;
    }

    public void setEncodeMode(String encodeMode) {
        this.encodeMode = encodeMode;
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
        hash += (errorCode != null ? errorCode.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfErrorCodes)) {
            return false;
        }
        CsfErrorCodes other = (CsfErrorCodes) object;
        if ((this.errorCode == null && other.errorCode != null) || (this.errorCode != null && !this.errorCode.equals(other.errorCode))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfErrorCodes[errorCode=" + errorCode + "]";
    }

}
