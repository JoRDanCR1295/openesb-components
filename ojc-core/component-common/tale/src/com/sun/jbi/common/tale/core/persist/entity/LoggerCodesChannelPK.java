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
public class LoggerCodesChannelPK implements Serializable {
    @Column(name = "LOGGER_CODE", nullable = false)
    private int loggerCode;
    @Column(name = "ALE_CHANNEL_CODE", nullable = false)
    private int aleChannelCode;

    public LoggerCodesChannelPK() {
    }

    public LoggerCodesChannelPK(int loggerCode, int aleChannelCode) {
        this.loggerCode = loggerCode;
        this.aleChannelCode = aleChannelCode;
    }

    public int getLoggerCode() {
        return loggerCode;
    }

    public void setLoggerCode(int loggerCode) {
        this.loggerCode = loggerCode;
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
        hash += (int) loggerCode;
        hash += (int) aleChannelCode;
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof LoggerCodesChannelPK)) {
            return false;
        }
        LoggerCodesChannelPK other = (LoggerCodesChannelPK) object;
        if (this.loggerCode != other.loggerCode) {
            return false;
        }
        if (this.aleChannelCode != other.aleChannelCode) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.LoggerCodesChannelPK[loggerCode=" + loggerCode + ", aleChannelCode=" + aleChannelCode + "]";
    }

}
