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
public class MsgTrackingPK implements Serializable {
    @Column(name = "MESG_ID", nullable = false)
    private String mesgId;
    @Column(name = "TRACK_ID", nullable = false)
    private String trackId;

    public MsgTrackingPK() {
    }

    public MsgTrackingPK(String mesgId, String trackId) {
        this.mesgId = mesgId;
        this.trackId = trackId;
    }

    public String getMesgId() {
        return mesgId;
    }

    public void setMesgId(String mesgId) {
        this.mesgId = mesgId;
    }

    public String getTrackId() {
        return trackId;
    }

    public void setTrackId(String trackId) {
        this.trackId = trackId;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (mesgId != null ? mesgId.hashCode() : 0);
        hash += (trackId != null ? trackId.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof MsgTrackingPK)) {
            return false;
        }
        MsgTrackingPK other = (MsgTrackingPK) object;
        if ((this.mesgId == null && other.mesgId != null) || (this.mesgId != null && !this.mesgId.equals(other.mesgId))) {
            return false;
        }
        if ((this.trackId == null && other.trackId != null) || (this.trackId != null && !this.trackId.equals(other.trackId))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.MsgTrackingPK[mesgId=" + mesgId + ", trackId=" + trackId + "]";
    }

}
