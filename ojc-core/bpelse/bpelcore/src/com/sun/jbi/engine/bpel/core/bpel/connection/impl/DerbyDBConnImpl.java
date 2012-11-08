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
 * @(#)DerbyDBConnImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.connection.impl;

import java.sql.Connection;
import java.sql.SQLException;

import com.sun.jbi.engine.bpel.core.bpel.connection.DBConnection;

/**
 * @author Sun Microsystems
 */
public class DerbyDBConnImpl extends DBConnection {

    /**
     * constructor
     * @param conn jdbc connection
     * @throws SQLException SQLException
     */
    public DerbyDBConnImpl(Connection conn) throws SQLException {
        super(conn);
    }

//    /**
//     * @see com.sun.jbi.engine.bpel.core.bpel.connection.DBConnection#release()
//     */
//    @Override
//    public void release() throws SQLException {
//        if (mConn.isClosed()) {
//            /*
//             * This check is not required as per the javadoc of java.sql.Connection. 
//             * Without the check, if close() is called more than once, it throws the 
//             * following exception.
//             */
////            java.lang.IllegalStateException: shareCount cannot be negative
////            at com.sun.enterprise.resource.ResourceHandle.decrementCount(ResourceHandle.java:205)
////            at com.sun.enterprise.resource.ConnectorAllocator$ConnectionListenerImpl.connectionClosed(ConnectorAllocator.java:58)
////            at com.sun.gjc.spi.ManagedConnection.connectionClosed(ManagedConnection.java:589)
////            at com.sun.gjc.spi.ConnectionHolder.close(ConnectionHolder.java:163)
////            at com.sun.jbi.engine.bpel.core.bpel.connection.impl.DerbyDBConnImpl.release(DerbyDBConnImpl.java:86)
//            return;
//        }
//        endTx();
//        mConn.close();
//    }
}
