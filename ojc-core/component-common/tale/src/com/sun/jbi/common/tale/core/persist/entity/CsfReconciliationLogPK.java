/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Embeddable;

/**
 *
 * @author echou
 */
@Embeddable
public class CsfReconciliationLogPK implements Serializable {
    @Column(name = "APP_NAME", nullable = false)
    private String appName;
    @Column(name = "APP_MESG_ID", nullable = false)
    private String appMesgId;
    @Column(name = "UNIT_NAME", nullable = false)
    private String unitName;

    public CsfReconciliationLogPK() {
    }

    public CsfReconciliationLogPK(String appName, String appMesgId, String unitName) {
        this.appName = appName;
        this.appMesgId = appMesgId;
        this.unitName = unitName;
    }

    public String getAppName() {
        return appName;
    }

    public void setAppName(String appName) {
        this.appName = appName;
    }

    public String getAppMesgId() {
        return appMesgId;
    }

    public void setAppMesgId(String appMesgId) {
        this.appMesgId = appMesgId;
    }

    public String getUnitName() {
        return unitName;
    }

    public void setUnitName(String unitName) {
        this.unitName = unitName;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (appName != null ? appName.hashCode() : 0);
        hash += (appMesgId != null ? appMesgId.hashCode() : 0);
        hash += (unitName != null ? unitName.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfReconciliationLogPK)) {
            return false;
        }
        CsfReconciliationLogPK other = (CsfReconciliationLogPK) object;
        if ((this.appName == null && other.appName != null) || (this.appName != null && !this.appName.equals(other.appName))) {
            return false;
        }
        if ((this.appMesgId == null && other.appMesgId != null) || (this.appMesgId != null && !this.appMesgId.equals(other.appMesgId))) {
            return false;
        }
        if ((this.unitName == null && other.unitName != null) || (this.unitName != null && !this.unitName.equals(other.unitName))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfReconciliationLogPK[appName=" + appName + ", appMesgId=" + appMesgId + ", unitName=" + unitName + "]";
    }

}
