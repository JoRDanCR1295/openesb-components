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

package com.sun.jbi.jdbcbc.bootstrap;

import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;


/**
 * Installer Extension MBean, allow configuration to be changed before installation
 */
public class InstallerExt implements InstallerExtMBean {
    private static final Messages mMessages = Messages.getMessages(InstallerExt.class);
    String mThreads;
    private Logger mLogger;

    /** Creates a new instance of InstallerExt */
    public InstallerExt() {
        mLogger = Messages.getLogger(InstallerExt.class);
    }

    /**
     *
     * @return 
     */
    //@Override
    public String getThreads() {
        mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00224.Get_threads"), mThreads);
        return mThreads;
    }

    /**
     *
     * @param val 
     */
    //@Override
    public void setThreads(final String val) {
        mLogger.log(Level.INFO,mMessages.getString("SQLSE_R00225.Set_threads"), val);
        mThreads = val;
    }
}