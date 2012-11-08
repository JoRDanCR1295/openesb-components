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
public class AlerterCodesChannelPK implements Serializable {
    @Column(name = "ALERTER_CODE", nullable = false)
    private int alerterCode;
    @Column(name = "ALE_CHANNEL_CODE", nullable = false)
    private int aleChannelCode;

    public AlerterCodesChannelPK() {
    }

    public AlerterCodesChannelPK(int alerterCode, int aleChannelCode) {
        this.alerterCode = alerterCode;
        this.aleChannelCode = aleChannelCode;
    }

    public int getAlerterCode() {
        return alerterCode;
    }

    public void setAlerterCode(int alerterCode) {
        this.alerterCode = alerterCode;
    }

    public int getAleChannelCode() {
        return aleChannelCode;
    }

    public void setAleChannelCode(int aleChannelCode) {
        this.aleChannelCode = aleChannelCode;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (int) alerterCode;
        hash += (int) aleChannelCode;
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof AlerterCodesChannelPK)) {
            return false;
        }
        AlerterCodesChannelPK other = (AlerterCodesChannelPK) object;
        if (this.alerterCode != other.alerterCode) {
            return false;
        }
        if (this.aleChannelCode != other.aleChannelCode) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.AlerterCodesChannelPK[alerterCode=" + alerterCode + ", aleChannelCode=" + aleChannelCode + "]";
    }

}
