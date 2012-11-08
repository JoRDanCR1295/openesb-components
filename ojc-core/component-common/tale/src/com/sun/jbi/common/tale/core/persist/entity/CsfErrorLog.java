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
@Table(name = "CSF_ERROR_LOG")
@NamedQueries({@NamedQuery(name = "CsfErrorLog.findByMesgId", query = "SELECT c FROM CsfErrorLog c WHERE c.mesgId = :mesgId"), @NamedQuery(name = "CsfErrorLog.findByErrorCode", query = "SELECT c FROM CsfErrorLog c WHERE c.errorCode = :errorCode"), @NamedQuery(name = "CsfErrorLog.findByErrorDetails", query = "SELECT c FROM CsfErrorLog c WHERE c.errorDetails = :errorDetails"), @NamedQuery(name = "CsfErrorLog.findByDisplayMesg", query = "SELECT c FROM CsfErrorLog c WHERE c.displayMesg = :displayMesg")})
public class CsfErrorLog implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "MESG_ID", nullable = false)
    private String mesgId;
    @Column(name = "ERROR_CODE", nullable = false)
    private int errorCode;
    @Column(name = "ERROR_DETAILS")
    private String errorDetails;
    @Column(name = "DISPLAY_MESG")
    private String displayMesg;

    /* relationship */
    @OneToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false)
    private CsfCmeLog csfCmeLog;
    
    public CsfErrorLog() {
    }

    public CsfErrorLog(String mesgId) {
        this.mesgId = mesgId;
    }

    public CsfErrorLog(String mesgId, int errorCode) {
        this.mesgId = mesgId;
        this.errorCode = errorCode;
    }
    
    public CsfErrorLog(CsfCmeLog csfCmeLog, int errorCode) {
        this.csfCmeLog = csfCmeLog;
        this.mesgId = csfCmeLog.getMesgId();
        this.errorCode = errorCode;
    }

    public String getMesgId() {
        return mesgId;
    }

    public void setMesgId(String mesgId) {
        this.mesgId = mesgId;
    }

    public int getErrorCode() {
        return errorCode;
    }

    public void setErrorCode(int errorCode) {
        this.errorCode = errorCode;
    }

    public String getErrorDetails() {
        return errorDetails;
    }

    public void setErrorDetails(String errorDetails) {
        this.errorDetails = errorDetails;
    }

    public String getDisplayMesg() {
        return displayMesg;
    }

    public void setDisplayMesg(String displayMesg) {
        this.displayMesg = displayMesg;
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
        if (!(object instanceof CsfErrorLog)) {
            return false;
        }
        CsfErrorLog other = (CsfErrorLog) object;
        if ((this.mesgId == null && other.mesgId != null) || (this.mesgId != null && !this.mesgId.equals(other.mesgId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfErrorLog[mesgId=" + mesgId + "]";
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
