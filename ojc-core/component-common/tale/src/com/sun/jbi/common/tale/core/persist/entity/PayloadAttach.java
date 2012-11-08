/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "PAYLOAD_ATTACH")
@NamedQueries({@NamedQuery(name = "PayloadAttach.findByMesgId", query = "SELECT p FROM PayloadAttach p WHERE p.payloadAttachPK.mesgId = :mesgId"), @NamedQuery(name = "PayloadAttach.findBySequenceId", query = "SELECT p FROM PayloadAttach p WHERE p.payloadAttachPK.sequenceId = :sequenceId")})
public class PayloadAttach implements Serializable {
    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected PayloadAttachPK payloadAttachPK;
    @Lob
    @Column(name = "ATTACHMENT")
    private byte[] attachment;

    /* relationship */
    @ManyToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false)
    private CsfPayloadLog csfPayloadLog;
    
    public PayloadAttach() {
    }

    public PayloadAttach(PayloadAttachPK payloadAttachPK) {
        this.payloadAttachPK = payloadAttachPK;
    }

    public PayloadAttach(String mesgId, String sequenceId) {
        this.payloadAttachPK = new PayloadAttachPK(mesgId, sequenceId);
    }
    
    public PayloadAttach(CsfPayloadLog csfPayloadLog, String sequenceId) {
        this.csfPayloadLog = csfPayloadLog;
        this.payloadAttachPK = new PayloadAttachPK(csfPayloadLog.getMesgId(), sequenceId);
    }

    public PayloadAttachPK getPayloadAttachPK() {
        return payloadAttachPK;
    }

    public void setPayloadAttachPK(PayloadAttachPK payloadAttachPK) {
        this.payloadAttachPK = payloadAttachPK;
    }

    public byte[] getAttachment() {
        return attachment;
    }

    public void setAttachment(byte[] attachment) {
        this.attachment = attachment;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (payloadAttachPK != null ? payloadAttachPK.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof PayloadAttach)) {
            return false;
        }
        PayloadAttach other = (PayloadAttach) object;
        if ((this.payloadAttachPK == null && other.payloadAttachPK != null) || (this.payloadAttachPK != null && !this.payloadAttachPK.equals(other.payloadAttachPK))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.PayloadAttach[payloadAttachPK=" + payloadAttachPK + "]";
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

}
