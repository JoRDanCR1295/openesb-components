/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "CSF_ALERTER_LOG")
@NamedQueries({@NamedQuery(name = "CsfAlerterLog.findByMesgId", query = "SELECT c FROM CsfAlerterLog c WHERE c.mesgId = :mesgId"), @NamedQuery(name = "CsfAlerterLog.findByAlerterCode", query = "SELECT c FROM CsfAlerterLog c WHERE c.alerterCode = :alerterCode"), @NamedQuery(name = "CsfAlerterLog.findByAlertDetails", query = "SELECT c FROM CsfAlerterLog c WHERE c.alertDetails = :alertDetails"), @NamedQuery(name = "CsfAlerterLog.findByDisplayMesg", query = "SELECT c FROM CsfAlerterLog c WHERE c.displayMesg = :displayMesg")})
public class CsfAlerterLog implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "MESG_ID", nullable = false)
    private String mesgId;
    @Column(name = "ALERTER_CODE", nullable = false)
    private int alerterCode;
    @Column(name = "ALERT_DETAILS")
    private String alertDetails;
    @Column(name = "DISPLAY_MESG")
    private String displayMesg;

    /* start relationship */
    @OneToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false)
    private CsfCmeLog csfCmeLog;
    
    @OneToOne(mappedBy="csfAlerterLog", cascade = { CascadeType.ALL })
    private CsfAlertResolution csfAlertResolution;
    
    /* end relationship */
    
    public CsfAlerterLog() {
    }

    public CsfAlerterLog(String mesgId) {
        this.mesgId = mesgId;
    }

    public CsfAlerterLog(String mesgId, int alerterCode) {
        this.mesgId = mesgId;
        this.alerterCode = alerterCode;
    }
    
    public CsfAlerterLog(CsfCmeLog csfCmeLog, int alerterCode) {
        this.csfCmeLog = csfCmeLog;
        this.mesgId = csfCmeLog.getMesgId();
        this.alerterCode = alerterCode;
    }

    public String getMesgId() {
        return mesgId;
    }

    public void setMesgId(String mesgId) {
        this.mesgId = mesgId;
    }

    public int getAlerterCode() {
        return alerterCode;
    }

    public void setAlerterCode(int alerterCode) {
        this.alerterCode = alerterCode;
    }

    public String getAlertDetails() {
        return alertDetails;
    }

    public void setAlertDetails(String alertDetails) {
        this.alertDetails = alertDetails;
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
        if (!(object instanceof CsfAlerterLog)) {
            return false;
        }
        CsfAlerterLog other = (CsfAlerterLog) object;
        if ((this.mesgId == null && other.mesgId != null) || (this.mesgId != null && !this.mesgId.equals(other.mesgId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfAlerterLog[mesgId=" + mesgId + "]";
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

    /**
     * @return the csfAlertResolution
     */
    public CsfAlertResolution getCsfAlertResolution() {
        return csfAlertResolution;
    }

    /**
     * @param csfAlertResolution the csfAlertResolution to set
     */
    public void setCsfAlertResolution(CsfAlertResolution csfAlertResolution) {
        this.csfAlertResolution = csfAlertResolution;
    }

}
