/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToOne;
import javax.persistence.Table;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "CSF_USER_AUDIT_LOG")
@NamedQueries({@NamedQuery(name = "CsfUserAuditLog.findByMesgId", query = "SELECT c FROM CsfUserAuditLog c WHERE c.mesgId = :mesgId"), @NamedQuery(name = "CsfUserAuditLog.findByUserId", query = "SELECT c FROM CsfUserAuditLog c WHERE c.userId = :userId"), @NamedQuery(name = "CsfUserAuditLog.findByActionType", query = "SELECT c FROM CsfUserAuditLog c WHERE c.actionType = :actionType"), @NamedQuery(name = "CsfUserAuditLog.findByActionResult", query = "SELECT c FROM CsfUserAuditLog c WHERE c.actionResult = :actionResult")})
public class CsfUserAuditLog implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "MESG_ID", nullable = false)
    private String mesgId;
    @Column(name = "USER_ID", nullable = false)
    private String userId;
    @Column(name = "ACTION_TYPE")
    private String actionType;
    @Column(name = "ACTION_RESULT")
    private String actionResult;

    /* relationship */
    @OneToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false)
    private CsfCmeLog csfCmeLog;
    
    public CsfUserAuditLog() {
    }

    public CsfUserAuditLog(String mesgId) {
        this.mesgId = mesgId;
    }

    public CsfUserAuditLog(String mesgId, String userId) {
        this.mesgId = mesgId;
        this.userId = userId;
    }
    
    public CsfUserAuditLog(CsfCmeLog csfCmeLog, String userId) {
        this.csfCmeLog = csfCmeLog;
        this.mesgId = csfCmeLog.getMesgId();
        this.userId = userId;
    }

    public String getMesgId() {
        return mesgId;
    }

    public void setMesgId(String mesgId) {
        this.mesgId = mesgId;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getActionType() {
        return actionType;
    }

    public void setActionType(String actionType) {
        this.actionType = actionType;
    }

    public String getActionResult() {
        return actionResult;
    }

    public void setActionResult(String actionResult) {
        this.actionResult = actionResult;
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
        if (!(object instanceof CsfUserAuditLog)) {
            return false;
        }
        CsfUserAuditLog other = (CsfUserAuditLog) object;
        if ((this.mesgId == null && other.mesgId != null) || (this.mesgId != null && !this.mesgId.equals(other.mesgId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfUserAuditLog[mesgId=" + mesgId + "]";
    }

    /**
     * @return the csfCmeLog
     */
    public CsfCmeLog getCsfCmeLog() {
        return csfCmeLog;
    }

    /**
     * @param csfCmeLog the csfCmeLog to set
     */
    public void setCsfCmeLog(CsfCmeLog csfCmeLog) {
        this.csfCmeLog = csfCmeLog;
    }

}
