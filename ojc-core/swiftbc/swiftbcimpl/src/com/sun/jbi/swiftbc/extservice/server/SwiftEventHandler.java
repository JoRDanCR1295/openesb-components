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
 * @(#)SwiftEventHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.swiftbc.extservice.server;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.swiftbc.ApplicationException;

import org.apache.mina.protocol.ProtocolHandlerAdapter;
import org.apache.mina.protocol.ProtocolSession;

import java.io.ByteArrayOutputStream;

import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * @author Sriram, S. Nageswara Rao
 * @version
 */
public class SwiftEventHandler extends ProtocolHandlerAdapter {
    private static final Messages mMessages = Messages.getMessages(SwiftEventHandler.class);
    private static final Logger mLog = Messages.getLogger(SwiftEventHandler.class);
    private SwiftListener mSwiftListener;

    public SwiftEventHandler() {
        // mSwiftListeners = new HashSet();
    }

    public void addSwiftListener(SwiftListener listener) {
        // mSwiftListeners.add(listener);
        mSwiftListener = listener;
    }

    /**
     * Returns the Swift Listener
     * @return SwiftListener
     */
    public SwiftListener getSwiftListener() {
        return mSwiftListener;
    }

    public void sessionOpened(ProtocolSession session)
        throws Exception {
        session.setAttribute("currentInput", new ByteArrayOutputStream());
        session.setAttribute("readBytes", new Long(0));
    }

    public void messageReceived(ProtocolSession session, Object message) {
        String SwiftMsg = (String) message;

        try {
            doWork(SwiftMsg, session);
        } catch (Exception ex) {
            mLog.log(Level.SEVERE,
                mMessages.getString("SwiftEventHandler_AN_EXCEPTION"), ex);
        }
    }

    public void messageSent(ProtocolSession session, Object message) {
        if (mLog.isLoggable(Level.INFO)) {
            mLog.log(Level.INFO, "SwiftEventHandler_SEND_ACK_SwiftES",
                new Object[] { message.toString() });
        }
    }

    public void doWork(String content, ProtocolSession session)
        throws Exception {
        DefaultSwiftCallback callback = new DefaultSwiftCallback(session);
        mSwiftListener.onMessage(content, callback);
    }

    class DefaultSwiftCallback implements SwiftCallback {
        private ProtocolSession mSession;

        DefaultSwiftCallback(ProtocolSession session) {
            mSession = session;
        }

        public void onReply(String SwiftMsgAck)
            throws ApplicationException, Exception {
            mSession.write(SwiftMsgAck);
        }
    }
}
