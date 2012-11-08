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
@Table(name = "CSF_RECONCILIATION_APPS")
@NamedQueries({@NamedQuery(name = "CsfReconciliationApps.findByAppName", query = "SELECT c FROM CsfReconciliationApps c WHERE c.appName = :appName"), @NamedQuery(name = "CsfReconciliationApps.findByPersistent", query = "SELECT c FROM CsfReconciliationApps c WHERE c.persistent = :persistent"), @NamedQuery(name = "CsfReconciliationApps.findByInterval1", query = "SELECT c FROM CsfReconciliationApps c WHERE c.interval1 = :interval1"), @NamedQuery(name = "CsfReconciliationApps.findByCreateId", query = "SELECT c FROM CsfReconciliationApps c WHERE c.createId = :createId"), @NamedQuery(name = "CsfReconciliationApps.findByCreateDateTime", query = "SELECT c FROM CsfReconciliationApps c WHERE c.createDateTime = :createDateTime"), @NamedQuery(name = "CsfReconciliationApps.findByLastModId", query = "SELECT c FROM CsfReconciliationApps c WHERE c.lastModId = :lastModId"), @NamedQuery(name = "CsfReconciliationApps.findByLastModDateTime", query = "SELECT c FROM CsfReconciliationApps c WHERE c.lastModDateTime = :lastModDateTime")})
public class CsfReconciliationApps implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "APP_NAME", nullable = false)
    private String appName;
    @Column(name = "PERSISTENT", nullable = false)
    private char persistent;
    @Column(name = "INTERVAL1")
    private Integer interval1;
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

    public CsfReconciliationApps() {
    }

    public CsfReconciliationApps(String appName) {
        this.appName = appName;
    }

    public CsfReconciliationApps(String appName, char persistent) {
        this.appName = appName;
        this.persistent = persistent;
    }

    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public char getPersistent() {
        return persistent;
    }

    public void setPersistent(char persistent) {
        this.persistent = persistent;
    }

    public Integer getInterval1() {
        return interval1;
    }

    public void setInterval1(Integer interval1) {
        this.interval1 = interval1;
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
        hash += (appName != null ? appName.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfReconciliationApps)) {
            return false;
        }
        CsfReconciliationApps other = (CsfReconciliationApps) object;
        if ((this.appName == null && other.appName != null) || (this.appName != null && !this.appName.equals(other.appName))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfReconciliationApps[appName=" + appName + "]";
    }

}
