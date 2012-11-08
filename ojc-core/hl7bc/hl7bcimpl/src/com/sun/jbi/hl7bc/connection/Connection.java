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
 * @(#)Connection.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.connection;

import org.apache.mina.common.ConnectFuture;

import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import org.apache.mina.common.IoSession;
import org.apache.mina.common.IoHandler;

/**
 * @author Raghunadh Teegavarapu
 */
public interface Connection {
    // connection configuration params name
    public static final String CONN_MAX_IDEL_TIMEOUT = "CONN_MAX_IDEL_TIMEOUT";

    public void startTimer() throws Exception;

    public void stopTimer();

    public ConnectionInfo getKey();

    public ConnectFuture getClientObject();

    public void setMaxIdleTimeout(long timeout);

    public long getMaxIdleTimeout();

    public void setLastUsed(long curTime);

    public long getLastUsed();

    public void discard();

    public boolean isConnected();

    public IoSession getIOSessionObject();

    public void createConnection(ConnectionInfo cfg) throws Exception;

    public IoHandler getIoHandler();

    public void setIoHandler(IoHandler ioHandler);

}
