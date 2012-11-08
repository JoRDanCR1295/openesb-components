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
package com.sun.jbi.filebc.bootstrap;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This allows configuration to be changed before installation
 *
 * @author aegloff
 */
public class InstallerExt implements InstallerExtMBean {

    private static final Messages messages =
            Messages.getMessages(InstallerExt.class);
    private Logger mLogger;
//    String mThreads;
    Integer mThreads;
    Integer mIBWorkerThreads;
    private ComponentConfig mConfigProps;

    public InstallerExt() {
        mLogger = Messages.getLogger(InstallerExt.class);
    }

    public Integer getThreads() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "getThreads(): " + mThreads + " threads");
        }
        return mThreads;
    }

    public void setThreads(Integer val) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "setThreads(): " + val + " threads");
        }
        mThreads = val;
    }

    public Integer getIBWorkerThreads() {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "getIBWorkerThreads(): " + mIBWorkerThreads + " threads");
        }
        return mIBWorkerThreads;
    }

    public void setIBWorkerThreads(Integer val) {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "setIBWorkerThreads(): " + val + " threads");
        }
        mIBWorkerThreads = val;
    }
    public void setInitialConfigurations(ComponentConfig props) {
        this.mConfigProps = props;
        mThreads = Integer.valueOf(props.getProperty(CONFIG_THREADS).getValue());
        mIBWorkerThreads = Integer.valueOf(props.getProperty(CONFIG_IB_WORKER_THREADS).getValue());
    }

    public ComponentConfig getInstallationConfigurationProperties() {
        mConfigProps.getProperty(CONFIG_THREADS).setValue(mThreads.toString());
        mConfigProps.getProperty(CONFIG_IB_WORKER_THREADS).setValue(mIBWorkerThreads.toString());
        return mConfigProps;
    }
}
