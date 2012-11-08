/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.common.tale.core.persist.entity;

import java.io.Serializable;
import javax.persistence.EmbeddedId;
import javax.persistence.Entity;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

/**
 *
 * @author echou
 */
@Entity
@Table(name = "ALERTER_CODES_CHANNEL")
@NamedQueries({@NamedQuery(name = "AlerterCodesChannel.findByAlerterCode", query = "SELECT a FROM AlerterCodesChannel a WHERE a.alerterCodesChannelPK.alerterCode = :alerterCode"), @NamedQuery(name = "AlerterCodesChannel.findByAleChannelCode", query = "SELECT a FROM AlerterCodesChannel a WHERE a.alerterCodesChannelPK.aleChannelCode = :aleChannelCode")})
public class AlerterCodesChannel implements Serializable {
    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected AlerterCodesChannelPK alerterCodesChannelPK;

    public AlerterCodesChannel() {
    }

    public AlerterCodesChannel(AlerterCodesChannelPK alerterCodesChannelPK) {
        this.alerterCodesChannelPK = alerterCodesChannelPK;
    }

    public AlerterCodesChannel(int alerterCode, int aleChannelCode) {
        this.alerterCodesChannelPK = new AlerterCodesChannelPK(alerterCode, aleChannelCode);
    }

    public AlerterCodesChannelPK getAlerterCodesChannelPK() {
        return alerterCodesChannelPK;
    }

    public void setAlerterCodesChannelPK(AlerterCodesChannelPK alerterCodesChannelPK) {
        this.alerterCodesChannelPK = alerterCodesChannelPK;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (alerterCodesChannelPK != null ? alerterCodesChannelPK.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof AlerterCodesChannel)) {
            return false;
        }
        AlerterCodesChannel other = (AlerterCodesChannel) object;
        if ((this.alerterCodesChannelPK == null && other.alerterCodesChannelPK != null) || (this.alerterCodesChannelPK != null && !this.alerterCodesChannelPK.equals(other.alerterCodesChannelPK))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.AlerterCodesChannel[alerterCodesChannelPK=" + alerterCodesChannelPK + "]";
    }

}
