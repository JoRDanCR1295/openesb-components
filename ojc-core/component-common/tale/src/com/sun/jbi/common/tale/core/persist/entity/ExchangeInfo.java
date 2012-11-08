/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToOne;
import javax.persistence.Table;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "EXCHANGE_INFO")
@NamedQueries({@NamedQuery(name = "ExchangeInfo.findByMesgId", query = "SELECT e FROM ExchangeInfo e WHERE e.mesgId = :mesgId"), @NamedQuery(name = "ExchangeInfo.findByExchangeId", query = "SELECT e FROM ExchangeInfo e WHERE e.exchangeId = :exchangeId"), @NamedQuery(name = "ExchangeInfo.findByEndpointName", query = "SELECT e FROM ExchangeInfo e WHERE e.endpointName = :endpointName"), @NamedQuery(name = "ExchangeInfo.findByServiceName", query = "SELECT e FROM ExchangeInfo e WHERE e.serviceName = :serviceName"), @NamedQuery(name = "ExchangeInfo.findByOpeartionName", query = "SELECT e FROM ExchangeInfo e WHERE e.opeartionName = :opeartionName")})
public class ExchangeInfo implements Serializable {
    private static final long serialVersionUID = 1L;
    
    @Id
    @Column(name = "MESG_ID", nullable = false)
    private String mesgId;
    @Column(name = "EXCHANGE_ID", nullable = false)
    private String exchangeId;
    @Column(name = "ENDPOINT_NAME")
    private String endpointName;
    @Column(name = "SERVICE_NAME")
    private String serviceName;
    @Column(name = "OPEARTION_NAME")
    private String opeartionName;

    /* relationship */
    @OneToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false)
    private CsfCmeLog csfCmeLog;
    
    public ExchangeInfo() {
    }
    
    public ExchangeInfo(String mesgId) {
        this.mesgId = mesgId;
    }

    public ExchangeInfo(String mesgId, String exchangeId) {
        this.mesgId = mesgId;
        this.exchangeId = exchangeId;
    }
    
    public ExchangeInfo(CsfCmeLog csfCmeLog, String exchangeId) {
        this.csfCmeLog = csfCmeLog;
        this.mesgId = csfCmeLog.getMesgId();
        this.exchangeId = exchangeId;
    }

    /**
     * @return the exchangeId
     */
    public String getExchangeId() {
        return exchangeId;
    }

    /**
     * @param exchangeId the exchangeId to set
     */
    public void setExchangeId(String exchangeId) {
        this.exchangeId = exchangeId;
    }

    /**
     * @return the mesgId
     */
    public String getMesgId() {
        return mesgId;
    }

    /**
     * @param mesgId the mesgId to set
     */
    public void setMesgId(String mesgId) {
        this.mesgId = mesgId;
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

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (mesgId != null ? mesgId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof ExchangeInfo)) {
            return false;
        }
        ExchangeInfo other = (ExchangeInfo) object;
        if ((this.mesgId == null && other.mesgId != null) || (this.mesgId != null && !this.mesgId.equals(other.mesgId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.ExchangeInfo[mesgId=" + mesgId + "]";
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

}
