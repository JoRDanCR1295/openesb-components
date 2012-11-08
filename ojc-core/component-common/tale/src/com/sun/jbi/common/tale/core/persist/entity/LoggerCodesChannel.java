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
@Table(name = "LOGGER_CODES_CHANNEL")
@NamedQueries({@NamedQuery(name = "LoggerCodesChannel.findByLoggerCode", query = "SELECT l FROM LoggerCodesChannel l WHERE l.loggerCodesChannelPK.loggerCode = :loggerCode"), @NamedQuery(name = "LoggerCodesChannel.findByAleChannelCode", query = "SELECT l FROM LoggerCodesChannel l WHERE l.loggerCodesChannelPK.aleChannelCode = :aleChannelCode")})
public class LoggerCodesChannel implements Serializable {
    private static final long serialVersionUID = 1L;
    @EmbeddedId
    protected LoggerCodesChannelPK loggerCodesChannelPK;

    public LoggerCodesChannel() {
    }

    public LoggerCodesChannel(LoggerCodesChannelPK loggerCodesChannelPK) {
        this.loggerCodesChannelPK = loggerCodesChannelPK;
    }

    public LoggerCodesChannel(int loggerCode, int aleChannelCode) {
        this.loggerCodesChannelPK = new LoggerCodesChannelPK(loggerCode, aleChannelCode);
    }

    public LoggerCodesChannelPK getLoggerCodesChannelPK() {
        return loggerCodesChannelPK;
    }

    public void setLoggerCodesChannelPK(LoggerCodesChannelPK loggerCodesChannelPK) {
        this.loggerCodesChannelPK = loggerCodesChannelPK;
    }

    @Override
    public int hashCode() {
        int hash = 0;
        hash += (loggerCodesChannelPK != null ? loggerCodesChannelPK.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof LoggerCodesChannel)) {
            return false;
        }
        LoggerCodesChannel other = (LoggerCodesChannel) object;
        if ((this.loggerCodesChannelPK == null && other.loggerCodesChannelPK != null) || (this.loggerCodesChannelPK != null && !this.loggerCodesChannelPK.equals(other.loggerCodesChannelPK))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "com.sun.jbi.engine.ale.core.persist.entity.LoggerCodesChannel[loggerCodesChannelPK=" + loggerCodesChannelPK + "]";
    }

}
