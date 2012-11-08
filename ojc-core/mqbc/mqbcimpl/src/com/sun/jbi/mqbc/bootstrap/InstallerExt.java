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

package com.sun.jbi.mqbc.bootstrap;

import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.management.InvalidAttributeValueException;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.Property;
import com.sun.jbi.mqbc.I18n;
import com.sun.jbi.mqbc.mbeans.RuntimeConfiguration;

/**
 * Installer Extension MBean, allow configuration to be changed before
 * installation.
 * 
 * @author Noel.Ang@sun.com
 */
public class InstallerExt implements InstallerExtMBean {
    private final int CONSTRAINTS_MIN_OUTBOUND_THREADS = 1;
    private final int CONSTRAINTS_MAX_OUTBOUND_THREADS = 65535;
    private final Logger mLogger = Logger.getLogger(getClass().getName());
    private volatile int mThreads = 10;
    private volatile boolean mUseSeparateTransactionBranches = true;

    public InstallerExt() {
    }

    public Integer getOutboundThreads() {
        return mThreads;
    }

    public void setOutboundThreads(Integer val)
            throws InvalidAttributeValueException {
        if (val == null) {
            throw new InvalidAttributeValueException("Null value");
        } else {
            if (val < CONSTRAINTS_MIN_OUTBOUND_THREADS
                    || val > CONSTRAINTS_MAX_OUTBOUND_THREADS) {
                throw new InvalidAttributeValueException(I18n.msg(
                        "0500: ''Outbound Threads'' must be in the range of {0} and {1}."
                                + " The value ''{2}'' specified is not valid.",
                        CONSTRAINTS_MIN_OUTBOUND_THREADS,
                        CONSTRAINTS_MAX_OUTBOUND_THREADS, val));
            }
        }
        synchronized (this) {
            int oldValue = mThreads;
            mThreads = val;
            mLogger.log(Level.INFO, I18n.msg(
                    "0501: ''Outbound Threads'' changed from {0} to {1}.",
                    oldValue, mThreads));
        }
    }

    public Boolean getSeparateTrxBranches() {
        return mUseSeparateTransactionBranches;
    }

    public void setSeparateTrxBranches(Boolean val)
            throws
            InvalidAttributeValueException {
        if (val == null) {
            throw new InvalidAttributeValueException("Null value");
        }
        mUseSeparateTransactionBranches = val;
    }

    public void read(ComponentConfig config) {
        Property property;
        String value;
        
        property = config.getProperty(RuntimeConfiguration.OUTBOUND_THREADS_PROPERTY);
        value = property.getValue();
        if (value != null) {
            try {
                mThreads = Integer.parseInt(value);
            } catch (NumberFormatException e) {
                ; // fall-thru; use preset value
            }
        }
        
        property = config.getProperty(RuntimeConfiguration.SEPARATE_TRX_BRANCHES_PROPERTY);
        value = property.getValue();
        if (value != null) {
            mUseSeparateTransactionBranches =
                    "1".equals(value) || Boolean.parseBoolean(value);
        }
    }

    public Properties toProperties() {
        Properties properties = new Properties();

        properties.setProperty(RuntimeConfiguration.OUTBOUND_THREADS_PROPERTY,
                Integer.toString(mThreads)
        );
        
        properties.setProperty(RuntimeConfiguration.SEPARATE_TRX_BRANCHES_PROPERTY,
                Boolean.toString(mUseSeparateTransactionBranches)
        );

        return properties;
    }
}
