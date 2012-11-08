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
 * @(#)MaxIdleTask.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.connection;

import com.sun.jbi.hl7bc.connection.Connection;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Wake up at the interval of MaxIdleTimeout
 * and check if the connection is used within previous MaxIdleTimeout millis
 * if yes, 
 * if no, evict the connection from the pool
 * @author Raghunadh Teegavarapu
 */
public class MaxIdleTask extends TimerTask {
    private static Logger mLogger = Logger.getLogger(MaxIdleTask.class.getName());

    private Connection mConn;
    
    public MaxIdleTask(Connection conn) {
        mConn = conn;
    }
    
    public void run() {
        try {
            HL7BCConnectionManager.checkConnection(mConn);
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "Exception removing connection from the connection pool, e=" + ex.getMessage(), ex);
        }
    }
}
