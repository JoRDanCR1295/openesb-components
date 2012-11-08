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
import javax.persistence.JoinColumns;
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
@Table(name = "MSG_EXTERNALIZE")
@NamedQueries({@NamedQuery(name = "MsgExternalize.findByMesgId", query = "SELECT m FROM MsgExternalize m WHERE m.msgExternalizePK.mesgId = :mesgId"), @NamedQuery(name = "MsgExternalize.findByVersion", query = "SELECT m FROM MsgExternalize m WHERE m.msgExternalizePK.version = :version"), @NamedQuery(name = "MsgExternalize.findByPayloadType", query = "SELECT m FROM MsgExternalize m WHERE m.msgExternalizePK.payloadType = :payloadType"), @NamedQuery(name = "MsgExternalize.findByPartId", query = "SELECT m FROM MsgExternalize m WHERE m.msgExternalizePK.partId = :partId")})
public class MsgExternalize implements Serializable {
    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected MsgExternalizePK msgExternalizePK;
    @Lob
    @Column(name = "PART_CONTENT")
    private String partContent;

    /* relationship */
    @ManyToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumns({
        @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false),
        @JoinColumn(name="VERSION", referencedColumnName="VERSION", insertable=false, updatable=false),
        @JoinColumn(name="PAYLOAD_TYPE", referencedColumnName="PAYLOAD_TYPE", insertable=false, updatable=false)
    })
    private CsfPayloadStore csfPayloadStore;
    
    public MsgExternalize() {
    }

    public MsgExternalize(MsgExternalizePK msgExternalizePK) {
        this.msgExternalizePK = msgExternalizePK;
    }

    public MsgExternalize(String mesgId, short version, String payloadType, String partId) {
        this.msgExternalizePK = new MsgExternalizePK(mesgId, version, payloadType, partId);
    }
    
    public MsgExternalize(CsfPayloadStore csfPayloadStore, String partId) {
        this.csfPayloadStore = csfPayloadStore;
        this.msgExternalizePK = new MsgExternalizePK(csfPayloadStore.getCsfPayloadStorePK().getMesgId(),
                csfPayloadStore.getCsfPayloadStorePK().getVersion(),
                csfPayloadStore.getCsfPayloadStorePK().getPayloadType(),
                partId);
    }

    public MsgExternalizePK getMsgExternalizePK() {
        return msgExternalizePK;
    }

    public void setMsgExternalizePK(MsgExternalizePK msgExternalizePK) {
        this.msgExternalizePK = msgExternalizePK;
    }

    public String getPartContent() {
        return partContent;
    }

    public void setPartContent(String partContent) {
        this.partContent = partContent;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (msgExternalizePK != null ? msgExternalizePK.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof MsgExternalize)) {
            return false;
        }
        MsgExternalize other = (MsgExternalize) object;
        if ((this.msgExternalizePK == null && other.msgExternalizePK != null) || (this.msgExternalizePK != null && !this.msgExternalizePK.equals(other.msgExternalizePK))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.MsgExternalize[msgExternalizePK=" + msgExternalizePK + "]";
    }

    /**
     * @return the csfPayloadStore
     */
    public CsfPayloadStore getCsfPayloadStore() {
        return csfPayloadStore;
    }

    /**
     * @param csfPayloadStore the csfPayloadStore to set
     */
    public void setCsfPayloadStore(CsfPayloadStore csfPayloadStore) {
        this.csfPayloadStore = csfPayloadStore;
    }

}
