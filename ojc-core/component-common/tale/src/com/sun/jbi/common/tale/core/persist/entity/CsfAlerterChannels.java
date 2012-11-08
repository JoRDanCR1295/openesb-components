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
@Table(name = "CSF_ALERTER_CHANNELS")
@NamedQueries({@NamedQuery(name = "CsfAlerterChannels.findByAlerterChannelCode", query = "SELECT c FROM CsfAlerterChannels c WHERE c.alerterChannelCode = :alerterChannelCode"), @NamedQuery(name = "CsfAlerterChannels.findByChannelType", query = "SELECT c FROM CsfAlerterChannels c WHERE c.channelType = :channelType"), @NamedQuery(name = "CsfAlerterChannels.findByHostName", query = "SELECT c FROM CsfAlerterChannels c WHERE c.hostName = :hostName"), @NamedQuery(name = "CsfAlerterChannels.findByCommunity", query = "SELECT c FROM CsfAlerterChannels c WHERE c.community = :community"), @NamedQuery(name = "CsfAlerterChannels.findByTrapPort", query = "SELECT c FROM CsfAlerterChannels c WHERE c.trapPort = :trapPort"), @NamedQuery(name = "CsfAlerterChannels.findByListenerPort", query = "SELECT c FROM CsfAlerterChannels c WHERE c.listenerPort = :listenerPort"), @NamedQuery(name = "CsfAlerterChannels.findByActiveFlag", query = "SELECT c FROM CsfAlerterChannels c WHERE c.activeFlag = :activeFlag"), @NamedQuery(name = "CsfAlerterChannels.findByCreateId", query = "SELECT c FROM CsfAlerterChannels c WHERE c.createId = :createId"), @NamedQuery(name = "CsfAlerterChannels.findByCreateDateTime", query = "SELECT c FROM CsfAlerterChannels c WHERE c.createDateTime = :createDateTime"), @NamedQuery(name = "CsfAlerterChannels.findByLastModId", query = "SELECT c FROM CsfAlerterChannels c WHERE c.lastModId = :lastModId"), @NamedQuery(name = "CsfAlerterChannels.findByLastModDateTime", query = "SELECT c FROM CsfAlerterChannels c WHERE c.lastModDateTime = :lastModDateTime")})
public class CsfAlerterChannels implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "ALERTER_CHANNEL_CODE", nullable = false)
    private Integer alerterChannelCode;
    @Column(name = "CHANNEL_TYPE")
    private String channelType;
    @Column(name = "HOST_NAME")
    private String hostName;
    @Column(name = "COMMUNITY")
    private String community;
    @Column(name = "TRAP_PORT")
    private String trapPort;
    @Column(name = "LISTENER_PORT")
    private String listenerPort;
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

    public CsfAlerterChannels() {
    }

    public CsfAlerterChannels(Integer alerterChannelCode) {
        this.alerterChannelCode = alerterChannelCode;
    }

    public Integer getAlerterChannelCode() {
        return alerterChannelCode;
    }

    public void setAlerterChannelCode(Integer alerterChannelCode) {
        this.alerterChannelCode = alerterChannelCode;
    }

    public String getChannelType() {
        return channelType;
    }

    public void setChannelType(String channelType) {
        this.channelType = channelType;
    }

    public String getHostName() {
        return hostName;
    }

    public void setHostName(String hostName) {
        this.hostName = hostName;
    }

    public String getCommunity() {
        return community;
    }

    public void setCommunity(String community) {
        this.community = community;
    }

    public String getTrapPort() {
        return trapPort;
    }

    public void setTrapPort(String trapPort) {
        this.trapPort = trapPort;
    }

    public String getListenerPort() {
        return listenerPort;
    }

    public void setListenerPort(String listenerPort) {
        this.listenerPort = listenerPort;
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
        hash += (alerterChannelCode != null ? alerterChannelCode.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfAlerterChannels)) {
            return false;
        }
        CsfAlerterChannels other = (CsfAlerterChannels) object;
        if ((this.alerterChannelCode == null && other.alerterChannelCode != null) || (this.alerterChannelCode != null && !this.alerterChannelCode.equals(other.alerterChannelCode))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfAlerterChannels[alerterChannelCode=" + alerterChannelCode + "]";
    }

}
