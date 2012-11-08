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
@Table(name = "CSF_ALERTER_GROUPS")
@NamedQueries({@NamedQuery(name = "CsfAlerterGroups.findByAlerterGroup", query = "SELECT c FROM CsfAlerterGroups c WHERE c.alerterGroup = :alerterGroup"), @NamedQuery(name = "CsfAlerterGroups.findByAlerterFrom", query = "SELECT c FROM CsfAlerterGroups c WHERE c.alerterFrom = :alerterFrom"), @NamedQuery(name = "CsfAlerterGroups.findByAlerterToPrimary", query = "SELECT c FROM CsfAlerterGroups c WHERE c.alerterToPrimary = :alerterToPrimary"), @NamedQuery(name = "CsfAlerterGroups.findByAlerterToSecondary", query = "SELECT c FROM CsfAlerterGroups c WHERE c.alerterToSecondary = :alerterToSecondary"), @NamedQuery(name = "CsfAlerterGroups.findByActiveFlag", query = "SELECT c FROM CsfAlerterGroups c WHERE c.activeFlag = :activeFlag"), @NamedQuery(name = "CsfAlerterGroups.findByCreateId", query = "SELECT c FROM CsfAlerterGroups c WHERE c.createId = :createId"), @NamedQuery(name = "CsfAlerterGroups.findByCreateDateTime", query = "SELECT c FROM CsfAlerterGroups c WHERE c.createDateTime = :createDateTime"), @NamedQuery(name = "CsfAlerterGroups.findByLastModId", query = "SELECT c FROM CsfAlerterGroups c WHERE c.lastModId = :lastModId"), @NamedQuery(name = "CsfAlerterGroups.findByLastModDateTime", query = "SELECT c FROM CsfAlerterGroups c WHERE c.lastModDateTime = :lastModDateTime")})
public class CsfAlerterGroups implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "ALERTER_GROUP", nullable = false)
    private String alerterGroup;
    @Column(name = "ALERTER_FROM")
    private String alerterFrom;
    @Column(name = "ALERTER_TO_PRIMARY")
    private String alerterToPrimary;
    @Column(name = "ALERTER_TO_SECONDARY")
    private String alerterToSecondary;
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

    public CsfAlerterGroups() {
    }

    public CsfAlerterGroups(String alerterGroup) {
        this.alerterGroup = alerterGroup;
    }

    public String getAlerterGroup() {
        return alerterGroup;
    }

    public void setAlerterGroup(String alerterGroup) {
        this.alerterGroup = alerterGroup;
    }

    public String getAlerterFrom() {
        return alerterFrom;
    }

    public void setAlerterFrom(String alerterFrom) {
        this.alerterFrom = alerterFrom;
    }

    public String getAlerterToPrimary() {
        return alerterToPrimary;
    }

    public void setAlerterToPrimary(String alerterToPrimary) {
        this.alerterToPrimary = alerterToPrimary;
    }

    public String getAlerterToSecondary() {
        return alerterToSecondary;
    }

    public void setAlerterToSecondary(String alerterToSecondary) {
        this.alerterToSecondary = alerterToSecondary;
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
        hash += (alerterGroup != null ? alerterGroup.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfAlerterGroups)) {
            return false;
        }
        CsfAlerterGroups other = (CsfAlerterGroups) object;
        if ((this.alerterGroup == null && other.alerterGroup != null) || (this.alerterGroup != null && !this.alerterGroup.equals(other.alerterGroup))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfAlerterGroups[alerterGroup=" + alerterGroup + "]";
    }

}
