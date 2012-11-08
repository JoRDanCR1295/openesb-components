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
@Table(name = "CSF_JMS_CHANNEL")
@NamedQueries({@NamedQuery(name = "CsfJmsChannel.findByChannelCode", query = "SELECT c FROM CsfJmsChannel c WHERE c.channelCode = :channelCode"), @NamedQuery(name = "CsfJmsChannel.findByHostIp", query = "SELECT c FROM CsfJmsChannel c WHERE c.hostIp = :hostIp"), @NamedQuery(name = "CsfJmsChannel.findByPort", query = "SELECT c FROM CsfJmsChannel c WHERE c.port = :port"), @NamedQuery(name = "CsfJmsChannel.findByName", query = "SELECT c FROM CsfJmsChannel c WHERE c.name = :name"), @NamedQuery(name = "CsfJmsChannel.findByChannelType", query = "SELECT c FROM CsfJmsChannel c WHERE c.channelType = :channelType"), @NamedQuery(name = "CsfJmsChannel.findByActiveFlag", query = "SELECT c FROM CsfJmsChannel c WHERE c.activeFlag = :activeFlag"), @NamedQuery(name = "CsfJmsChannel.findByDescription", query = "SELECT c FROM CsfJmsChannel c WHERE c.description = :description"), @NamedQuery(name = "CsfJmsChannel.findByCreateId", query = "SELECT c FROM CsfJmsChannel c WHERE c.createId = :createId"), @NamedQuery(name = "CsfJmsChannel.findByCreateDateTime", query = "SELECT c FROM CsfJmsChannel c WHERE c.createDateTime = :createDateTime"), @NamedQuery(name = "CsfJmsChannel.findByLastModId", query = "SELECT c FROM CsfJmsChannel c WHERE c.lastModId = :lastModId"), @NamedQuery(name = "CsfJmsChannel.findByLastModDateTime", query = "SELECT c FROM CsfJmsChannel c WHERE c.lastModDateTime = :lastModDateTime")})
public class CsfJmsChannel implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "CHANNEL_CODE", nullable = false)
    private Integer channelCode;
    @Column(name = "HOST_IP", nullable = false)
    private String hostIp;
    @Column(name = "PORT", nullable = false)
    private short port;
    @Column(name = "NAME", nullable = false)
    private String name;
    @Column(name = "CHANNEL_TYPE", nullable = false)
    private char channelType;
    @Column(name = "ACTIVE_FLAG", nullable = false)
    private char activeFlag;
    @Column(name = "DESCRIPTION")
    private String description;
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

    public CsfJmsChannel() {
    }

    public CsfJmsChannel(Integer channelCode) {
        this.channelCode = channelCode;
    }

    public CsfJmsChannel(Integer channelCode, String hostIp, short port, String name, char channelType, char activeFlag) {
        this.channelCode = channelCode;
        this.hostIp = hostIp;
        this.port = port;
        this.name = name;
        this.channelType = channelType;
        this.activeFlag = activeFlag;
    }

    public Integer getChannelCode() {
        return channelCode;
    }

    public void setChannelCode(Integer channelCode) {
        this.channelCode = channelCode;
    }

    public String getHostIp() {
        return hostIp;
    }

    public void setHostIp(String hostIp) {
        this.hostIp = hostIp;
    }

    public short getPort() {
        return port;
    }

    public void setPort(short port) {
        this.port = port;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public char getChannelType() {
        return channelType;
    }

    public void setChannelType(char channelType) {
        this.channelType = channelType;
    }

    public char getActiveFlag() {
        return activeFlag;
    }

    public void setActiveFlag(char activeFlag) {
        this.activeFlag = activeFlag;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
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
        hash += (channelCode != null ? channelCode.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfJmsChannel)) {
            return false;
        }
        CsfJmsChannel other = (CsfJmsChannel) object;
        if ((this.channelCode == null && other.channelCode != null) || (this.channelCode != null && !this.channelCode.equals(other.channelCode))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfJmsChannel[channelCode=" + channelCode + "]";
    }

}
