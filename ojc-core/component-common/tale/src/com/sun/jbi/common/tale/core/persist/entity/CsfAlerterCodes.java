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
@Table(name = "CSF_ALERTER_CODES")
@NamedQueries({@NamedQuery(name = "CsfAlerterCodes.findByAlerterCode", query = "SELECT c FROM CsfAlerterCodes c WHERE c.alerterCode = :alerterCode"), @NamedQuery(name = "CsfAlerterCodes.findByAlerterLabel", query = "SELECT c FROM CsfAlerterCodes c WHERE c.alerterLabel = :alerterLabel"), @NamedQuery(name = "CsfAlerterCodes.findByAlerterCategory", query = "SELECT c FROM CsfAlerterCodes c WHERE c.alerterCategory = :alerterCategory"), @NamedQuery(name = "CsfAlerterCodes.findByAlerterLevel", query = "SELECT c FROM CsfAlerterCodes c WHERE c.alerterLevel = :alerterLevel"), @NamedQuery(name = "CsfAlerterCodes.findByAlerterDescription", query = "SELECT c FROM CsfAlerterCodes c WHERE c.alerterDescription = :alerterDescription"), @NamedQuery(name = "CsfAlerterCodes.findByAlerterGroup", query = "SELECT c FROM CsfAlerterCodes c WHERE c.alerterGroup = :alerterGroup"), @NamedQuery(name = "CsfAlerterCodes.findByAlerterChannelCode", query = "SELECT c FROM CsfAlerterCodes c WHERE c.alerterChannelCode = :alerterChannelCode"), @NamedQuery(name = "CsfAlerterCodes.findByActiveFlag", query = "SELECT c FROM CsfAlerterCodes c WHERE c.activeFlag = :activeFlag"), @NamedQuery(name = "CsfAlerterCodes.findByCreateId", query = "SELECT c FROM CsfAlerterCodes c WHERE c.createId = :createId"), @NamedQuery(name = "CsfAlerterCodes.findByCreateDateTime", query = "SELECT c FROM CsfAlerterCodes c WHERE c.createDateTime = :createDateTime"), @NamedQuery(name = "CsfAlerterCodes.findByLastModId", query = "SELECT c FROM CsfAlerterCodes c WHERE c.lastModId = :lastModId"), @NamedQuery(name = "CsfAlerterCodes.findByLastModDateTime", query = "SELECT c FROM CsfAlerterCodes c WHERE c.lastModDateTime = :lastModDateTime")})
public class CsfAlerterCodes implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "ALERTER_CODE", nullable = false)
    private Integer alerterCode;
    @Column(name = "ALERTER_LABEL")
    private String alerterLabel;
    @Column(name = "ALERTER_CATEGORY")
    private String alerterCategory;
    @Column(name = "ALERTER_LEVEL")
    private String alerterLevel;
    @Column(name = "ALERTER_DESCRIPTION")
    private String alerterDescription;
    @Column(name = "ALERTER_GROUP")
    private String alerterGroup;
    @Column(name = "ALERTER_CHANNEL_CODE")
    private Integer alerterChannelCode;
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

    public CsfAlerterCodes() {
    }

    public CsfAlerterCodes(Integer alerterCode) {
        this.alerterCode = alerterCode;
    }

    public Integer getAlerterCode() {
        return alerterCode;
    }

    public void setAlerterCode(Integer alerterCode) {
        this.alerterCode = alerterCode;
    }

    public String getAlerterLabel() {
        return alerterLabel;
    }

    public void setAlerterLabel(String alerterLabel) {
        this.alerterLabel = alerterLabel;
    }

    public String getAlerterCategory() {
        return alerterCategory;
    }

    public void setAlerterCategory(String alerterCategory) {
        this.alerterCategory = alerterCategory;
    }

    public String getAlerterLevel() {
        return alerterLevel;
    }

    public void setAlerterLevel(String alerterLevel) {
        this.alerterLevel = alerterLevel;
    }

    public String getAlerterDescription() {
        return alerterDescription;
    }

    public void setAlerterDescription(String alerterDescription) {
        this.alerterDescription = alerterDescription;
    }

    public String getAlerterGroup() {
        return alerterGroup;
    }

    public void setAlerterGroup(String alerterGroup) {
        this.alerterGroup = alerterGroup;
    }

    public Integer getAlerterChannelCode() {
        return alerterChannelCode;
    }

    public void setAlerterChannelCode(Integer alerterChannelCode) {
        this.alerterChannelCode = alerterChannelCode;
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
        hash += (alerterCode != null ? alerterCode.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfAlerterCodes)) {
            return false;
        }
        CsfAlerterCodes other = (CsfAlerterCodes) object;
        if ((this.alerterCode == null && other.alerterCode != null) || (this.alerterCode != null && !this.alerterCode.equals(other.alerterCode))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfAlerterCodes[alerterCode=" + alerterCode + "]";
    }

}
