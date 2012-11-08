/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)LoggableXAResource.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.recovery;

import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;
import javax.transaction.xa.XAException;

import java.io.File;

import java.util.logging.Logger;
import java.util.logging.Level;

import com.sun.jbi.jmsbc.LogSupport;
import com.sun.jbi.internationalization.Messages;
import org.omg.CORBA.COMM_FAILURE;

/**
 *
 * LoggableXAResource
 */
public class LoggableXAResource implements XAResource {
    private XAResource delegate;
    
    private static final Messages mMessages =
        Messages.getMessages(LoggableXAResource.class);
    private static final Logger mLogger =
        Messages.getLogger(LoggableXAResource.class);
        
    private boolean crashOnCommit = false;
    private boolean logCalls = false;
    
    /** Creates a new instance of LoggableXAResource */
    public LoggableXAResource(XAResource delegate) {
        this.delegate = delegate;
    }

    public void commit(Xid xid, boolean onePhase) throws XAException {
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_COMMIT_CALLED",
                        new Object [] {xidToString(xid), Boolean.valueOf(onePhase)});                
            logCalls = false;
        }
        
        if (crashOnCommit) {
            mLogger.log(Level.SEVERE,
                "LoggableXAResource_CAUSE_HARD_CRASH=****** TESTING MODE ONLY ****** !!! CAUSING HARD CRASH !!!");
            // Cause a system crash

            System.exit(0);
        } else {
            delegate.commit(xid, onePhase);
        }
    }
    
    public void end(Xid xid, int flags) throws XAException {
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_END_CALLED",
                        new Object [] {xidToString(xid), new Integer(flags)});
        }
        delegate.end(xid, flags);        
    }
    
    public void forget (Xid xid) throws XAException {
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_FORGET_CALLED",
                        new Object [] {xidToString(xid)});
        }
        delegate.forget(xid);        
    }
    
    public int getTransactionTimeout() throws XAException {
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_GET_TRANSACTION_TIMEOUT_CALLED");
        }
        int timeout = delegate.getTransactionTimeout();
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_GET_TRANSACTION_TIMEOUT_RESULT",
                        new Object [] {new Integer(timeout)});
        }
        return timeout;
    }
    
    public boolean isSameRM(XAResource xares) throws XAException {
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_IS_SAME_RM_CALLED",
                        new Object [] {xares.toString()});
        }
        boolean isSame = delegate.isSameRM(xares);
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_IS_SAME_RM_RESULT",
                        new Object [] {Boolean.valueOf(isSame)});
        }
        
        return isSame;
    }
    
    public int prepare(Xid xid) throws XAException {
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_PREPARE_CALLED",
                        new Object [] {xidToString(xid)});
        }
        int res = delegate.prepare(xid);
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_PREPARE_RESULT",
                        new Object [] {new Integer(res)});
        }        
        return res;
    }
    
    public Xid[] recover(int flag) throws XAException {
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_RECOVER_CALLED",
                        new Object [] {new Integer(flag)});
        }
        Xid [] xids = delegate.recover(flag);
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            StringBuffer xidsStr = new StringBuffer();
            if (xids.length > 0) {
                for (int i=0; i < xids.length; i++) {
                    xidsStr.append(xidToString(xids[i]));
                    if (i < xids.length-1){
                        xidsStr.append(", ");
                    }
                }
            } else {
                xidsStr.append("none");                        
            }
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_RECOVER_RESULT",
                        new Object [] {new Integer(xids.length), xidsStr.toString()});
        }        
        return xids;
    }
    
    public void rollback(Xid xid) throws XAException {
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_ROLLBACK_CALLED",
                        new Object [] {xidToString(xid)});
        }
        delegate.rollback(xid);
    }
    
    public boolean setTransactionTimeout(int seconds) throws XAException {
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_SET_TRANSACTION_TIMEOUT_CALLED",
                        new Object [] {new Integer(seconds)});
        }
        boolean res = delegate.setTransactionTimeout(seconds);
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_SET_TRANSACTION_TIMEOUT_RESULT",
                        new Object [] {Boolean.valueOf(res)});
        }        
        return res;
    }
    
    public void start(Xid xid, int flags) throws XAException {
        if (logCalls && mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "LoggableXAResource_START_CALLED",
                        new Object [] {xidToString(xid), new Integer(flags)});
        }
        delegate.start(xid, flags);
    }
    
    public void crashOnCommit (boolean val) {
        this.crashOnCommit = val;
    }

    public void logCalls (boolean val) {
        this.logCalls = val;
    }
    
    private static final char hexChar(int c) {
        final String hex = "0123456789ABCDEF";
        return hex.charAt(c & 0xF);
    }
    
    private String xidToString(Xid xid) {
        StringBuffer result = new StringBuffer();
        result.append("xid:");

        result.append(xid.getFormatId());
        // format id
        result.append(":");

        byte [] globalTransactionId = xid.getGlobalTransactionId();
        for (int i = 0; i < globalTransactionId.length; i++) {
            byte b = globalTransactionId[i];
            result.append(hexChar(b >> 4));
            result.append(hexChar(b));
        }
        result.append(":");
        
        byte [] branchQualifier = xid.getBranchQualifier();
        for (int i = 0; i < branchQualifier.length; i++) {
            byte b = branchQualifier[i];
            result.append(hexChar(b >> 4));
            result.append(hexChar(b));
        }
        return result.toString();
    }
    
}
