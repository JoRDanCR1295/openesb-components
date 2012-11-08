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

package com.sun.jbi.execbc.bootstrap;

import com.sun.jbi.internationalization.Messages;
import java.util.logging.Logger;

/**
 * This allows configuration to be changed before installation
 *
 * @author aegloff
 * @author Jun Xu
 */
public class InstallerExt implements InstallerExtMBean {
    private static final Messages messages =
            Messages.getMessages(InstallerExt.class);
    private Logger mLogger;
    
    String mMaxSessions;
    
    public InstallerExt() {
        mLogger = Messages.getLogger(InstallerExt.class);
    }
    
    public String getMaximumSessions() {
        mLogger.info("Get_max_sessions" + mMaxSessions);
        return mMaxSessions;
    }
    
    public void setMaximumSessions(String val) {
        mLogger.info("Set_max_sessions" + val);
        mMaxSessions = val;
    }
}
