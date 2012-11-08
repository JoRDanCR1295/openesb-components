/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "CSF_PAYLOAD_LOG")
@NamedQueries({@NamedQuery(name = "CsfPayloadLog.findByMesgId", query = "SELECT c FROM CsfPayloadLog c WHERE c.mesgId = :mesgId"), @NamedQuery(name = "CsfPayloadLog.findByEncodeMode", query = "SELECT c FROM CsfPayloadLog c WHERE c.encodeMode = :encodeMode")})
public class CsfPayloadLog implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @Column(name = "MESG_ID", nullable = false)
    private String mesgId;
    @Column(name = "ENCODE_MODE")
    private String encodeMode;

    /* start relationship */
    @OneToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false)
    private CsfCmeLog csfCmeLog;
    
    @OneToMany(mappedBy="csfPayloadLog", cascade = { CascadeType.ALL })
    private Collection<PayloadAttach> payloadAttachs = new HashSet<PayloadAttach> ();
    
    @OneToMany(mappedBy="csfPayloadLog", cascade = { CascadeType.ALL })
    private Collection<CsfPayloadStore> csfPayloadStores = new HashSet<CsfPayloadStore> ();
    
    /* end relationship */
    
    public CsfPayloadLog() {
    }

    public CsfPayloadLog(String mesgId) {
        this.mesgId = mesgId;
    }
    
    public CsfPayloadLog(CsfCmeLog csfCmeLog) {
        this.csfCmeLog = csfCmeLog;
        this.mesgId = csfCmeLog.getMesgId();
    }

    public String getMesgId() {
        return mesgId;
    }

    public void setMesgId(String mesgId) {
        this.mesgId = mesgId;
    }

    public String getEncodeMode() {
        return encodeMode;
    }

    public void setEncodeMode(String encodeMode) {
        this.encodeMode = encodeMode;
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
        if (!(object instanceof CsfPayloadLog)) {
            return false;
        }
        CsfPayloadLog other = (CsfPayloadLog) object;
        if ((this.mesgId == null && other.mesgId != null) || (this.mesgId != null && !this.mesgId.equals(other.mesgId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfPayloadLog[mesgId=" + mesgId + "]";
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

    /**
     * @return the payloadAttachs
     */
    public Collection<PayloadAttach> getPayloadAttachs() {
        return payloadAttachs;
    }

    /**
     * @param payloadAttachs the payloadAttachs to set
     */
    public void setPayloadAttachs(Collection<PayloadAttach> payloadAttachs) {
        this.payloadAttachs = payloadAttachs;
    }

    /**
     * @return the csfPayloadStores
     */
    public Collection<CsfPayloadStore> getCsfPayloadStores() {
        return csfPayloadStores;
    }

    /**
     * @param csfPayloadStores the csfPayloadStores to set
     */
    public void setCsfPayloadStores(Collection<CsfPayloadStore> csfPayloadStores) {
        this.csfPayloadStores = csfPayloadStores;
    }

}
