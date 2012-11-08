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
@Table(name = "CSF_ERROR_RESOLUTION")
@NamedQueries({@NamedQuery(name = "CsfErrorResolution.findByMesgId", query = "SELECT c FROM CsfErrorResolution c WHERE c.mesgId = :mesgId"), @NamedQuery(name = "CsfErrorResolution.findByResolutionStatus", query = "SELECT c FROM CsfErrorResolution c WHERE c.resolutionStatus = :resolutionStatus"), @NamedQuery(name = "CsfErrorResolution.findByResolutionBy", query = "SELECT c FROM CsfErrorResolution c WHERE c.resolutionBy = :resolutionBy"), @NamedQuery(name = "CsfErrorResolution.findByResolutionDetails", query = "SELECT c FROM CsfErrorResolution c WHERE c.resolutionDetails = :resolutionDetails"), @NamedQuery(name = "CsfErrorResolution.findByResolutionDt", query = "SELECT c FROM CsfErrorResolution c WHERE c.resolutionDt = :resolutionDt")})
public class CsfErrorResolution implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "MESG_ID", nullable = false)
    private String mesgId;
    @Column(name = "RESOLUTION_STATUS")
    private String resolutionStatus;
    @Column(name = "RESOLUTION_BY")
    private String resolutionBy;
    @Column(name = "RESOLUTION_DETAILS")
    private String resolutionDetails;
    @Column(name = "RESOLUTION_DT")
    @Temporal(TemporalType.TIMESTAMP)
    private Date resolutionDt;

    public CsfErrorResolution() {
    }

    public CsfErrorResolution(String mesgId) {
        this.mesgId = mesgId;
    }

    public String getMesgId() {
        return mesgId;
    }

    public void setMesgId(String mesgId) {
        this.mesgId = mesgId;
    }

    public String getResolutionStatus() {
        return resolutionStatus;
    }

    public void setResolutionStatus(String resolutionStatus) {
        this.resolutionStatus = resolutionStatus;
    }

    public String getResolutionBy() {
        return resolutionBy;
    }

    public void setResolutionBy(String resolutionBy) {
        this.resolutionBy = resolutionBy;
    }

    public String getResolutionDetails() {
        return resolutionDetails;
    }

    public void setResolutionDetails(String resolutionDetails) {
        this.resolutionDetails = resolutionDetails;
    }

    public Date getResolutionDt() {
        return resolutionDt;
    }

    public void setResolutionDt(Date resolutionDt) {
        this.resolutionDt = resolutionDt;
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
        if (!(object instanceof CsfErrorResolution)) {
            return false;
        }
        CsfErrorResolution other = (CsfErrorResolution) object;
        if ((this.mesgId == null && other.mesgId != null) || (this.mesgId != null && !this.mesgId.equals(other.mesgId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfErrorResolution[mesgId=" + mesgId + "]";
    }

}
