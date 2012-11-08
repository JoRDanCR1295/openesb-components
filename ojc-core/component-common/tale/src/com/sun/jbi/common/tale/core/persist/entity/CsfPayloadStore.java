/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "CSF_PAYLOAD_STORE")
@NamedQueries({@NamedQuery(name = "CsfPayloadStore.findByMesgId", query = "SELECT c FROM CsfPayloadStore c WHERE c.csfPayloadStorePK.mesgId = :mesgId"), @NamedQuery(name = "CsfPayloadStore.findByVersion", query = "SELECT c FROM CsfPayloadStore c WHERE c.csfPayloadStorePK.version = :version"), @NamedQuery(name = "CsfPayloadStore.findByPayloadType", query = "SELECT c FROM CsfPayloadStore c WHERE c.csfPayloadStorePK.payloadType = :payloadType"), @NamedQuery(name = "CsfPayloadStore.findByCreateId", query = "SELECT c FROM CsfPayloadStore c WHERE c.createId = :createId"), @NamedQuery(name = "CsfPayloadStore.findByCreateDateTime", query = "SELECT c FROM CsfPayloadStore c WHERE c.createDateTime = :createDateTime")})
public class CsfPayloadStore implements Serializable {
    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected CsfPayloadStorePK csfPayloadStorePK;
    @Lob
    @Column(name = "PAYLOAD_MESG")
    private String payloadMesg;
    @Lob
    @Column(name = "EXTERNALIZED_MESG")
    private String externalizedMesg;
    @Column(name = "CREATE_ID")
    private String createId;
    @Column(name = "CREATE_DATE_TIME")
    @Temporal(TemporalType.TIMESTAMP)
    private Date createDateTime;

    /* start relationship */
    @ManyToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false)
    private CsfPayloadLog csfPayloadLog;
    
    @OneToMany(mappedBy="csfPayloadStore", cascade = { CascadeType.ALL })
    private Collection<MsgExternalize> msgExternalizes = new HashSet<MsgExternalize> ();
    
    /* end relationship */
    
    public CsfPayloadStore() {
    }

    public CsfPayloadStore(CsfPayloadStorePK csfPayloadStorePK) {
        this.csfPayloadStorePK = csfPayloadStorePK;
    }

    public CsfPayloadStore(String mesgId, short version, String payloadType) {
        this.csfPayloadStorePK = new CsfPayloadStorePK(mesgId, version, payloadType);
    }

    public CsfPayloadStore(CsfPayloadLog csfPayloadLog, short version, String payloadType) {
        this.csfPayloadLog = csfPayloadLog;
        this.csfPayloadStorePK = new CsfPayloadStorePK(csfPayloadLog.getMesgId(), version, payloadType);
    }
    
    public CsfPayloadStorePK getCsfPayloadStorePK() {
        return csfPayloadStorePK;
    }

    public void setCsfPayloadStorePK(CsfPayloadStorePK csfPayloadStorePK) {
        this.csfPayloadStorePK = csfPayloadStorePK;
    }

    public String getPayloadMesg() {
        return payloadMesg;
    }

    public void setPayloadMesg(String payloadMesg) {
        this.payloadMesg = payloadMesg;
    }

    public String getExternalizedMesg() {
        return externalizedMesg;
    }

    public void setExternalizedMesg(String externalizedMesg) {
        this.externalizedMesg = externalizedMesg;
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

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (csfPayloadStorePK != null ? csfPayloadStorePK.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof CsfPayloadStore)) {
            return false;
        }
        CsfPayloadStore other = (CsfPayloadStore) object;
        if ((this.csfPayloadStorePK == null && other.csfPayloadStorePK != null) || (this.csfPayloadStorePK != null && !this.csfPayloadStorePK.equals(other.csfPayloadStorePK))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.CsfPayloadStore[csfPayloadStorePK=" + csfPayloadStorePK + "]";
    }

    /**
     * @return the csfPayloadLog
     */
    public CsfPayloadLog getCsfPayloadLog() {
        return csfPayloadLog;
    }

    /**
     * @param csfPayloadLog the csfPayloadLog to set
     */
    public void setCsfPayloadLog(CsfPayloadLog csfPayloadLog) {
        this.csfPayloadLog = csfPayloadLog;
    }

    /**
     * @return the msgExternalizes
     */
    public Collection<MsgExternalize> getMsgExternalizes() {
        return msgExternalizes;
    }

    /**
     * @param msgExternalizes the msgExternalizes to set
     */
    public void setMsgExternalizes(Collection<MsgExternalize> msgExternalizes) {
        this.msgExternalizes = msgExternalizes;
    }

}
