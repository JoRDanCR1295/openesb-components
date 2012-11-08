/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;
import javax.persistence.Column;
import javax.persistence.Embeddable;

/**
 *
 * @author echou
 */
@Embeddable
public class MsgExternalizePK implements Serializable {
    @Column(name = "MESG_ID", nullable = false)
    private String mesgId;
    @Column(name = "VERSION", nullable = false)
    private short version;
    @Column(name = "PAYLOAD_TYPE", nullable = false)
    private String payloadType;
    @Column(name = "PART_ID", nullable = false)
    private String partId;

    public MsgExternalizePK() {
    }

    public MsgExternalizePK(String mesgId, short version, String payloadType, String partId) {
        this.mesgId = mesgId;
        this.version = version;
        this.payloadType = payloadType;
        this.partId = partId;
    }

    public String getMesgId() {
        return mesgId;
    }

    public void setMesgId(String mesgId) {
        this.mesgId = mesgId;
    }

    public short getVersion() {
        return version;
    }

    public void setVersion(short version) {
        this.version = version;
    }

    public String getPayloadType() {
        return payloadType;
    }

    public void setPayloadType(String payloadType) {
        this.payloadType = payloadType;
    }

    public String getPartId() {
        return partId;
    }

    public void setPartId(String partId) {
        this.partId = partId;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (mesgId != null ? mesgId.hashCode() : 0);
        hash += (int) version;
        hash += (payloadType != null ? payloadType.hashCode() : 0);
        hash += (partId != null ? partId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof MsgExternalizePK)) {
            return false;
        }
        MsgExternalizePK other = (MsgExternalizePK) object;
        if ((this.mesgId == null && other.mesgId != null) || (this.mesgId != null && !this.mesgId.equals(other.mesgId))) {
            return false;
        }
        if (this.version != other.version) {
            return false;
        }
        if ((this.payloadType == null && other.payloadType != null) || (this.payloadType != null && !this.payloadType.equals(other.payloadType))) {
            return false;
        }
        if ((this.partId == null && other.partId != null) || (this.partId != null && !this.partId.equals(other.partId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.MsgExternalizePK[mesgId=" + mesgId + ", version=" + version + ", payloadType=" + payloadType + ", partId=" + partId + "]";
    }

}
