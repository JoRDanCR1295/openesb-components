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
 * @(#)InstallerExt.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.mbeans;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.jmsbc.LogSupport;

/**
 * Installer Extension MBean, allow configuration to be changed before installation
 *
 */
public class InstallerExt implements InstallerExtMBean {

    String mThreads;
    String mForceConcurrencyMode;
    int mForceMaxConcurrentConsumers;
    String mDefaultRedeliveryHandling;
    
    private static final Messages mMessages =
        Messages.getMessages(InstallerExt.class);
    private static final Logger mLogger =
        Messages.getLogger(InstallerExt.class);

        
    /** 
     * Creates a new instance of InstallerExt 
     */
    public InstallerExt() {
    }

    /** 
     * Gets the number of threads
     * @return String Number of threads
     */
    public String getThreads() { 
        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG, 
                        "InstallerExt_GETTHREADS_CALLED",
                        new Object[]{mThreads});
        }        
        return mThreads;
    }
    
    /** 
     * Sets the number of threads
     * @param val String Number of threads
     */
    public void setThreads(String val) {
        String oldVal = mThreads;
        mThreads = val;
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG, 
                        "JMSBC-C0301.AttributeChanged",
                        new Object[]{"Threads", oldVal, val});
        }        
    }

    public void setForceConcurrencyMode(String val){
        String oldVal = mForceConcurrencyMode;
        mForceConcurrencyMode = val;
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG,
                        "JMSBC-C0301.AttributeChanged",
                        new Object[]{"ForceConcurrencyMode", oldVal, val});
        
        }
    }

    public String getForceConcurrencyMode() {
        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "InstallerExt_GETFORCECONCURRENCYMODE_CALLED",
                        new Object[]{mForceConcurrencyMode});
        }
        return mForceConcurrencyMode;
    }

    public int getForceMaxConcurrentConsumers(){
        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "InstallerExt_GETFORCEMAXCONCURRENTCONSUMERS",
                        new Object[]{mForceMaxConcurrentConsumers});
        }
        return mForceMaxConcurrentConsumers;
        
    }
    public void setForceMaxConcurrentConsumers(int val){
        int oldVal = mForceMaxConcurrentConsumers;
        mForceMaxConcurrentConsumers = val;
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG,
                        "JMSBC-C0301.AttributeChanged",
                        new Object[]{"ForceMaxConcurrentConsumers", oldVal, val});

        }
    }

    public String getDefaultRedeliveryHandling(){
        if (mLogger.isLoggable(LogSupport.LEVEL_DEBUG)) {
            mLogger.log(LogSupport.LEVEL_DEBUG,
                        "InstallerExt_GETDEFAULTREDELIVERYHANDLING",
                        new Object[]{mDefaultRedeliveryHandling});
        }
        return mDefaultRedeliveryHandling;
    }
    public void setDefaultRedeliveryHandling(String val){
        String oldVal = mDefaultRedeliveryHandling;
        mDefaultRedeliveryHandling = val;
        if (mLogger.isLoggable(Level.CONFIG)) {
            mLogger.log(Level.CONFIG,
                        "JMSBC-C0301.AttributeChanged",
                        new Object[]{"DefaultRedeliveryHandling", oldVal, val});

        }
    }


    
}
