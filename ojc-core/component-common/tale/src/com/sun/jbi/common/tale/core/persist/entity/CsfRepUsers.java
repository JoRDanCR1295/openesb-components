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
@Table(name = "CSF_REP_USERS")
@NamedQueries({@NamedQuery(name = "CsfRepUsers.findByUserLogicalId", query = "SELECT c FROM CsfRepUsers c WHERE c.userLogicalId = :userLogicalId"), @NamedQuery(name = "CsfRepUsers.findByUserDescription", query = "SELECT c FROM CsfRepUsers c WHERE c.userDescription = :userDescription"), @NamedQuery(name = "CsfRepUsers.findByUserName", query = "SELECT c FROM CsfRepUsers c WHERE c.userName = :userName"), @NamedQuery(name = "CsfRepUsers.findByUserPassword", query = "SELECT c FROM CsfRepUsers c WHERE c.userPassword = :userPassword"), @NamedQuery(name = "CsfRepUsers.findByActiveFlag", query = "SELECT c FROM CsfRepUsers c WHERE c.activeFlag = :activeFlag"), @NamedQuery(name = "CsfRepUsers.findByCreateId", query = "SELECT c FROM CsfRepUsers c WHERE c.createId = :createId"), @NamedQuery(name = "CsfRepUsers.findByCreateDateTime", query = "SELECT c FROM CsfRepUsers c WHERE c.createDateTime = :createDateTime"), @NamedQuery(name = "CsfRepUsers.findByLastModId", query = "SELECT c FROM CsfRepUsers c WHERE c.lastModId = :lastModId"), @NamedQuery(name = "CsfRepUsers.findByLastModDateTime", query = "SELECT c FROM CsfRepUsers c WHERE c.lastModDateTime = :lastModDateTime")})
public class CsfRepUsers implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "USER_LOGICAL_ID", nullable = false)
    private String userLogicalId;
    @Column(name = "USER_DESCRIPTION")
    private String userDescription;
    @Column(name = "USER_NAME")
    private String userName;
    @Column(name = "USER_PASSWORD")
    private String userPassword;
    @Column(name = "ACTIVE_FLAG", nullable = false)
    private char activeFlag;
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

    public CsfRepUsers() {
    }

    public CsfRepUsers(String userLogicalId) {
        this.userLogicalId = userLogicalId;
    }

    public CsfRepUsers(String userLogicalId, char activeFlag) {
        this.userLogicalId = userLogicalId;
        this.activeFlag = activeFlag;
    }

    public String getUserLogicalId() {
        return userLogicalId;
    }

    public void setUserLogicalId(String userLogicalId) {
        this.userLogicalId = userLogicalId;
    }

    public String getUserDescription() {
        return userDescription;
    }

    public void setUserDescription(String userDescription) {
        this.userDescription = userDescription;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getUserPassword() {
        return userPassword;
    }

    public void setUserPassword(String userPassword) {
        this.userPassword = userPassword;
    }

    public char getActiveFlag() {
        return activeFlag;
    }

    public void setActiveFlag(char activeFlag) {
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
        hash += (userLogicalId != null ? userLogicalId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfRepUsers)) {
            return false;
        }
        CsfRepUsers other = (CsfRepUsers) object;
        if ((this.userLogicalId == null && other.userLogicalId != null) || (this.userLogicalId != null && !this.userLogicalId.equals(other.userLogicalId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfRepUsers[userLogicalId=" + userLogicalId + "]";
    }

}
