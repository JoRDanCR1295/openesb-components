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
@Table(name = "ALE_CHANNELS")
@NamedQueries({@NamedQuery(name = "AleChannels.findByAleChannelCode", query = "SELECT a FROM AleChannels a WHERE a.aleChannelCode = :aleChannelCode"), @NamedQuery(name = "AleChannels.findByChannelType", query = "SELECT a FROM AleChannels a WHERE a.channelType = :channelType"), @NamedQuery(name = "AleChannels.findByFileName", query = "SELECT a FROM AleChannels a WHERE a.fileName = :fileName"), @NamedQuery(name = "AleChannels.findByHostName", query = "SELECT a FROM AleChannels a WHERE a.hostName = :hostName"), @NamedQuery(name = "AleChannels.findByCommunity", query = "SELECT a FROM AleChannels a WHERE a.community = :community"), @NamedQuery(name = "AleChannels.findByTrapPort", query = "SELECT a FROM AleChannels a WHERE a.trapPort = :trapPort"), @NamedQuery(name = "AleChannels.findByListenerPort", query = "SELECT a FROM AleChannels a WHERE a.listenerPort = :listenerPort"), @NamedQuery(name = "AleChannels.findByEmailFrom", query = "SELECT a FROM AleChannels a WHERE a.emailFrom = :emailFrom"), @NamedQuery(name = "AleChannels.findByEmailToPrimary", query = "SELECT a FROM AleChannels a WHERE a.emailToPrimary = :emailToPrimary"), @NamedQuery(name = "AleChannels.findByEmailToSecondary", query = "SELECT a FROM AleChannels a WHERE a.emailToSecondary = :emailToSecondary"), @NamedQuery(name = "AleChannels.findByJmsPort", query = "SELECT a FROM AleChannels a WHERE a.jmsPort = :jmsPort"), @NamedQuery(name = "AleChannels.findByJmsType", query = "SELECT a FROM AleChannels a WHERE a.jmsType = :jmsType"), @NamedQuery(name = "AleChannels.findByJmsName", query = "SELECT a FROM AleChannels a WHERE a.jmsName = :jmsName"), @NamedQuery(name = "AleChannels.findByUserName", query = "SELECT a FROM AleChannels a WHERE a.userName = :userName"), @NamedQuery(name = "AleChannels.findByPassWord", query = "SELECT a FROM AleChannels a WHERE a.passWord = :passWord"), @NamedQuery(name = "AleChannels.findByEndpointName", query = "SELECT a FROM AleChannels a WHERE a.endpointName = :endpointName"), @NamedQuery(name = "AleChannels.findByServiceName", query = "SELECT a FROM AleChannels a WHERE a.serviceName = :serviceName"), @NamedQuery(name = "AleChannels.findByOpeartionName", query = "SELECT a FROM AleChannels a WHERE a.opeartionName = :opeartionName"), @NamedQuery(name = "AleChannels.findByField1", query = "SELECT a FROM AleChannels a WHERE a.field1 = :field1"), @NamedQuery(name = "AleChannels.findByField2", query = "SELECT a FROM AleChannels a WHERE a.field2 = :field2"), @NamedQuery(name = "AleChannels.findByField3", query = "SELECT a FROM AleChannels a WHERE a.field3 = :field3"), @NamedQuery(name = "AleChannels.findByField4", query = "SELECT a FROM AleChannels a WHERE a.field4 = :field4"), @NamedQuery(name = "AleChannels.findByActiveFlag", query = "SELECT a FROM AleChannels a WHERE a.activeFlag = :activeFlag"), @NamedQuery(name = "AleChannels.findByCreateId", query = "SELECT a FROM AleChannels a WHERE a.createId = :createId"), @NamedQuery(name = "AleChannels.findByCreateDateTime", query = "SELECT a FROM AleChannels a WHERE a.createDateTime = :createDateTime"), @NamedQuery(name = "AleChannels.findByLastModId", query = "SELECT a FROM AleChannels a WHERE a.lastModId = :lastModId"), @NamedQuery(name = "AleChannels.findByLastModDateTime", query = "SELECT a FROM AleChannels a WHERE a.lastModDateTime = :lastModDateTime")})
public class AleChannels implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "ALE_CHANNEL_CODE", nullable = false)
    private Integer aleChannelCode;
    @Column(name = "CHANNEL_TYPE")
    private String channelType;
    @Column(name = "FILE_NAME")
    private String fileName;
    @Column(name = "HOST_NAME")
    private String hostName;
    @Column(name = "COMMUNITY")
    private String community;
    @Column(name = "TRAP_PORT")
    private String trapPort;
    @Column(name = "LISTENER_PORT")
    private String listenerPort;
    @Column(name = "EMAIL_FROM")
    private String emailFrom;
    @Column(name = "EMAIL_TO_PRIMARY")
    private String emailToPrimary;
    @Column(name = "EMAIL_TO_SECONDARY")
    private String emailToSecondary;
    @Column(name = "JMS_PORT")
    private String jmsPort;
    @Column(name = "JMS_TYPE")
    private String jmsType;
    @Column(name = "JMS_NAME")
    private String jmsName;
    @Column(name = "USER_NAME")
    private String userName;
    @Column(name = "PASS_WORD")
    private String passWord;
    @Column(name = "ENDPOINT_NAME")
    private String endpointName;
    @Column(name = "SERVICE_NAME")
    private String serviceName;
    @Column(name = "OPEARTION_NAME")
    private String opeartionName;
    @Column(name = "FIELD_1")
    private String field1;
    @Column(name = "FIELD_2")
    private String field2;
    @Column(name = "FIELD_3")
    private String field3;
    @Column(name = "FIELD_4")
    private String field4;
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

    public AleChannels() {
    }

    public AleChannels(Integer aleChannelCode) {
        this.aleChannelCode = aleChannelCode;
    }

    public Integer getAleChannelCode() {
        return aleChannelCode;
    }

    public void setAleChannelCode(Integer aleChannelCode) {
        this.aleChannelCode = aleChannelCode;
    }

    public String getChannelType() {
        return channelType;
    }

    public void setChannelType(String channelType) {
        this.channelType = channelType;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
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

    public String getEmailFrom() {
        return emailFrom;
    }

    public void setEmailFrom(String emailFrom) {
        this.emailFrom = emailFrom;
    }

    public String getEmailToPrimary() {
        return emailToPrimary;
    }

    public void setEmailToPrimary(String emailToPrimary) {
        this.emailToPrimary = emailToPrimary;
    }

    public String getEmailToSecondary() {
        return emailToSecondary;
    }

    public void setEmailToSecondary(String emailToSecondary) {
        this.emailToSecondary = emailToSecondary;
    }

    public String getJmsPort() {
        return jmsPort;
    }

    public void setJmsPort(String jmsPort) {
        this.jmsPort = jmsPort;
    }

    public String getJmsType() {
        return jmsType;
    }

    public void setJmsType(String jmsType) {
        this.jmsType = jmsType;
    }

    public String getJmsName() {
        return jmsName;
    }

    public void setJmsName(String jmsName) {
        this.jmsName = jmsName;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getPassWord() {
        return passWord;
    }

    public void setPassWord(String passWord) {
        this.passWord = passWord;
    }

    public String getEndpointName() {
        return endpointName;
    }

    public void setEndpointName(String endpointName) {
        this.endpointName = endpointName;
    }

    public String getServiceName() {
        return serviceName;
    }

    public void setServiceName(String serviceName) {
        this.serviceName = serviceName;
    }

    public String getOpeartionName() {
        return opeartionName;
    }

    public void setOpeartionName(String opeartionName) {
        this.opeartionName = opeartionName;
    }

    public String getField1() {
        return field1;
    }

    public void setField1(String field1) {
        this.field1 = field1;
    }

    public String getField2() {
        return field2;
    }

    public void setField2(String field2) {
        this.field2 = field2;
    }

    public String getField3() {
        return field3;
    }

    public void setField3(String field3) {
        this.field3 = field3;
    }

    public String getField4() {
        return field4;
    }

    public void setField4(String field4) {
        this.field4 = field4;
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
        hash += (aleChannelCode != null ? aleChannelCode.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof AleChannels)) {
            return false;
        }
        AleChannels other = (AleChannels) object;
        if ((this.aleChannelCode == null && other.aleChannelCode != null) || (this.aleChannelCode != null && !this.aleChannelCode.equals(other.aleChannelCode))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.AleChannels[aleChannelCode=" + aleChannelCode + "]";
    }

}
