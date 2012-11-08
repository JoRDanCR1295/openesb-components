/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;

import javax.persistence.CascadeType;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "MSG_TRACKING")
@NamedQueries({@NamedQuery(name = "MsgTracking.findByMesgId", query = "SELECT m FROM MsgTracking m WHERE m.msgTrackingPK.mesgId = :mesgId"), @NamedQuery(name = "MsgTracking.findByTrackId", query = "SELECT m FROM MsgTracking m WHERE m.msgTrackingPK.trackId = :trackId")})
public class MsgTracking implements Serializable {
    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected MsgTrackingPK msgTrackingPK;

    /* relationship */
    @ManyToOne(cascade = { CascadeType.PERSIST } )
    @JoinColumn(name="MESG_ID", referencedColumnName="MESG_ID", insertable=false, updatable=false)
    private CsfCmeLog csfCmeLog;
    
    public MsgTracking() {
    }

    public MsgTracking(MsgTrackingPK msgTrackingPK) {
        this.msgTrackingPK = msgTrackingPK;
    }

    public MsgTracking(String mesgId, String trackId) {
        this.msgTrackingPK = new MsgTrackingPK(mesgId, trackId);
    }
    
    public MsgTracking(CsfCmeLog csfCmeLog, String trackId) {
        this.csfCmeLog = csfCmeLog;
        this.msgTrackingPK = new MsgTrackingPK(csfCmeLog.getMesgId(), trackId);
    }

    public MsgTrackingPK getMsgTrackingPK() {
        return msgTrackingPK;
    }

    public void setMsgTrackingPK(MsgTrackingPK msgTrackingPK) {
        this.msgTrackingPK = msgTrackingPK;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (msgTrackingPK != null ? msgTrackingPK.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof MsgTracking)) {
            return false;
        }
        MsgTracking other = (MsgTracking) object;
        if ((this.msgTrackingPK == null && other.msgTrackingPK != null) || (this.msgTrackingPK != null && !this.msgTrackingPK.equals(other.msgTrackingPK))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.MsgTracking[msgTrackingPK=" + msgTrackingPK + "]";
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
