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
@Table(name = "CSF_LOGGER_LOG")
@NamedQueries({@NamedQuery(name = "CsfLoggerLog.findByMesgId", query = "SELECT c FROM CsfLoggerLog c WHERE c.mesgId = :mesgId"), @NamedQuery(name = "CsfLoggerLog.findByLoggerCode", query = "SELECT c FROM CsfLoggerLog c WHERE c.loggerCode = :loggerCode"), @NamedQuery(name = "CsfLoggerLog.findByLogDetails", query = "SELECT c FROM CsfLoggerLog c WHERE c.logDetails = :logDetails"), @NamedQuery(name = "CsfLoggerLog.findByDisplayMesg", query = "SELECT c FROM CsfLoggerLog c WHERE c.displayMesg = :displayMesg")})
public class CsfLoggerLog implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "MESG_ID", nullable = false)
    private String mesgId;
    @Column(name = "LOGGER_CODE", nullable = false)
    private int loggerCode;
    @Column(name = "LOG_DETAILS")
    private String logDetails;
    @Column(name = "DISPLAY_MESG")
    private String displayMesg;

    /* relationship */
    @OneToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false)
    private CsfCmeLog csfCmeLog;
    
    public CsfLoggerLog() {
    }

    public CsfLoggerLog(String mesgId) {
        this.mesgId = mesgId;
    }

    public CsfLoggerLog(String mesgId, int loggerCode) {
        this.mesgId = mesgId;
        this.loggerCode = loggerCode;
    }
    
    public CsfLoggerLog(CsfCmeLog csfCmeLog, int loggerCode) {
        this.csfCmeLog = csfCmeLog;
        this.mesgId = csfCmeLog.getMesgId();
        this.loggerCode = loggerCode;
    }

    public String getMesgId() {
        return mesgId;
    }

    public void setMesgId(String mesgId) {
        this.mesgId = mesgId;
    }

    public int getLoggerCode() {
        return loggerCode;
    }

    public void setLoggerCode(int loggerCode) {
        this.loggerCode = loggerCode;
    }

    public String getLogDetails() {
        return logDetails;
    }

    public void setLogDetails(String logDetails) {
        this.logDetails = logDetails;
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
        if (!(object instanceof CsfLoggerLog)) {
            return false;
        }
        CsfLoggerLog other = (CsfLoggerLog) object;
        if ((this.mesgId == null && other.mesgId != null) || (this.mesgId != null && !this.mesgId.equals(other.mesgId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfLoggerLog[mesgId=" + mesgId + "]";
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
