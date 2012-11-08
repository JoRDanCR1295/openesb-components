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
 * @(#)HL7EventHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.server;

import java.io.ByteArrayOutputStream;
import java.util.StringTokenizer;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Iterator;
import java.util.Set;
import java.util.regex.*;

import javax.xml.transform.dom.DOMSource;

import org.apache.mina.common.IdleStatus;
import org.apache.mina.common.IoHandlerAdapter;
import org.apache.mina.common.IoSession;
import org.apache.mina.common.CloseFuture;
import org.apache.mina.util.SessionUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.hl7bc.ApplicationException;
import com.sun.jbi.hl7bc.extservice.persist.MLLPV1PersistenceHandler;
import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.I18n;

import static com.sun.jbi.hl7bc.util.XmlUtil.*;


/**
 * @author Raghunadh, S. Nageswara Rao
 * @version
 */
public class HL7EventHandler extends IoHandlerAdapter {

    private static final Logger mLog = Logger.getLogger(HL7EventHandler.class.getName());

    private static final String READBYTES = "readBytes";

    private static final String CURRENTINPUT = "currentInput";

    private static final String TIMEOUTOCCURED = "timeOutOccured";

    private static final String TRUE = "true";

    public static final String NAK_SENT_COUNTER = "nakSentCounter";

    public static final String CANNED_NAK_SENT_COUNTER = "cannedNakSentCounter";

    public static final String HL7_GENERIC_MESSAGETYPE = "all_HL7_MessageTypes";

    private HL7Listener mHL7Listener;

    private Map<String, HL7Listener> mHL7Listeners;

    private HL7Server mHL7Server;
    private MLLPV1PersistenceHandler persistenceHandler = null;

    public HL7EventHandler() {
        mHL7Listeners = new HashMap<String, HL7Listener>();
    }
    
    public HL7EventHandler(MLLPV1PersistenceHandler persistenceHandler){
        mHL7Listeners = new HashMap<String, HL7Listener>();       
        this.persistenceHandler = persistenceHandler;
    }

    public void addHL7Listener(HL7Listener listener) {
        // mHL7Listeners.add(listener);
        mHL7Listener = listener;
    }

    public void addHL7Listeners(Map<String, HL7Listener> listeners) {
        mHL7Listeners = listeners;
    }

    public void addConnectionObserver(HL7Server hl7Server) {
        mHL7Server = hl7Server;
    }

    /**
     * Returns the HL7 Listener
     * 
     * @return HL7Listener
     */
    public HL7Listener getHL7Listener() {
        return mHL7Listener;
    }

    /**
     * Returns the HL7 Listeners
     * 
     * @return Map
     */
    public Map<String, HL7Listener> getHL7Listeners() {
        return mHL7Listeners;
    }

    public void sessionOpened(IoSession session) throws Exception {
        initializeNewIoSession(session);
    }

    public void sessionCreated(IoSession session) throws Exception {
        SessionUtil.initialize(session);
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("A new client connection is created: " + session.getRemoteAddress().toString());
        }
        if (mHL7Server != null) {
            mHL7Server.notifyNewConnetion(session);
        }
    }

    public void sessionIdle(IoSession session, IdleStatus status) throws Exception {
        if (status.toString().equals(IdleStatus.READER_IDLE.toString())) {
            session.setAttribute(TIMEOUTOCCURED, TRUE);
        }
    }

    public void sessionClosed(IoSession session) throws Exception {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine("client connection is closed: " + session.getRemoteAddress().toString());
        }
        if (mHL7Server != null) {
            mHL7Server.notifyLostConnection(session);
        }
    }

    public void messageReceived(IoSession session, Object message) {
        String hl7Msg = (String) message;
        try {
            if(persistenceHandler == null){
				doWork(hl7Msg, session);
            }else{
                doWork(message, session);
            }
        } catch (Exception ex) {
            mLog.log(Level.SEVERE, I18n.msg("E0273: An Exception occured while receiving the message from the socket"),
                    ex);
        }
    }

    public void messageSent(IoSession session, Object message) {
        if (mLog.isLoggable(Level.FINE)) {
            mLog.log(Level.FINE, I18n.msg("I0160: Sending HL7 message acknowledgement to HL7 External System :{0}",
                    message.toString()));
        }
        if (mHL7Server != null) {
            mHL7Server.notifyLastACKMessageSentTimeStamp(session.getRemoteAddress().toString());
        }       
    }

    public void doWork(String content, IoSession session) throws Exception {
        DefaultHL7Callback callback = new DefaultHL7Callback(session);
        if (mHL7Server != null) {
            mHL7Server.notifyLastMessageReceivedTimeStamp(session.getRemoteAddress().toString());
        }       
        mHL7Listener = findHL7Listener(content);
        mHL7Listener = mHL7Listener == null ? (HL7Listener) mHL7Listeners.get(HL7_GENERIC_MESSAGETYPE) : mHL7Listener;
        if (mHL7Listener != null) {
            mHL7Listener.onMessage(content, callback);
        } else {
            String msg = I18n.msg(
                    "E0274: No HL7Listener created for the received message type. Available hl7listeners are : {0}, provide valid message type i.e MSH.9 field value on the operation.",
                    mHL7Listeners.keySet());
            mLog.log(Level.SEVERE, msg);
            throw new Exception(msg);
        }
    }

   public synchronized void doWork(Object message, IoSession session) throws Exception {
        String content = (String) message;        
        DefaultHL7Callback callback = new DefaultHL7Callback(session);
        if (mHL7Server != null) {
            mHL7Server.notifyLastMessageReceivedTimeStamp(session.getRemoteAddress().toString());
        } 
        // check duplicate message
        if (content == null)
            return;
        boolean found = this.persistenceHandler.checkDuplicateAndsendResponseFromLog(content);
        // Duplicate messsage found. Now send the commit ack followed by HL7 response
        if (found && this.persistenceHandler.getResponseMessage() != null) {
            boolean ack = found;
            mLog.fine(I18n.msg("MLLPV1 Exchange Test: Sending MLLPV1 : {0}", (ack ? "Ack" : "Nak")));
            sendPersistedACK(callback, this.persistenceHandler.getResponseMessage()); // Sending
                                                                                        // the HL7
            // Response message from
            // HL7MESSAGELOG. It can
            // be HL7 ACK .
            return;
        }       
        mHL7Listener = findHL7Listener(content);
        mHL7Listener = mHL7Listener == null ? (HL7Listener) mHL7Listeners.get(HL7_GENERIC_MESSAGETYPE) : mHL7Listener;
        if (mHL7Listener != null) {
            mHL7Listener.onMessage(content, callback);
        } else {
            String msg = I18n.msg(
                    "E0274: No HL7Listener created for the received message type. Available hl7listeners are : {0}, provide valid message type i.e MSH.9 field value on the operation.",
                    mHL7Listeners.keySet());
            mLog.log(Level.SEVERE, msg);
            throw new Exception(msg);
        }
    }
   
    protected HL7Listener findHL7Listener(String content) throws Exception {
        Set entrySet = getHL7Listeners().entrySet();
        HL7Listener hl7Listener = null;
        String msh9fldValue = getMSH9FldValue(content);
        for (Iterator it = entrySet.iterator(); it.hasNext();) {
            Entry entry = (Entry) it.next();
            String messageType = (String) entry.getKey();
            if(msh9fldValue != null){
                if (msh9fldValue.contains(messageType)) {
                    hl7Listener = (HL7Listener) entry.getValue();
                    break;
                }
            }
        }
        return hl7Listener;
    }
    
    private String getMSH9FldValue(String message)throws Exception{
        String messageType = null;
        if(message.startsWith(HL7Constants.MSH)){
            String fldSeparator = Character.toString(message.charAt(3));
            String delim = Pattern.quote(fldSeparator);
            String[] fields = message.split(delim);
            if (fields.length > 8) {
                messageType = fields[8];
            }
        }else{// This is required to handle hl7 xml messages
            Document document = createDocumentFromXML(true, new String(message));
            Element element = document.getDocumentElement();
            DOMSource src = new DOMSource((Node) element);
            Node MSHSeg = src.getNode().getFirstChild();
            NodeList childNodes = MSHSeg.getChildNodes();
            for(int j = 0; j < childNodes.getLength(); j++){
                 Node childNode = childNodes.item(j);
                 String childNodeName = childNode.getLocalName();
                 if (childNodeName != null && childNodeName.contains(HL7Constants.MSH9)) {
                     messageType = childNode.getFirstChild().getFirstChild().getNodeValue();
                     break;
                 }                        
            }              
        }
        return messageType;
    }
    
    private void sendPersistedACK(DefaultHL7Callback callback, String response) {          
        callback.sendPersistedACK(response); //       
	}

    /**
     * Prepares a new IoSession for use, adding the attributes that will be used
     * by the HL7 BC protocol implementations.
     */
	public static void initializeNewIoSession(IoSession session) {
		session.setAttribute(CURRENTINPUT, new ByteArrayOutputStream());
        session.setAttribute(READBYTES, new Long(0));
        session.setAttribute(NAK_SENT_COUNTER, new Long(0));
        session.setAttribute(CANNED_NAK_SENT_COUNTER, new Long(0));
	}
    
    class DefaultHL7Callback implements HL7Callback {

        private IoSession mSession;

        DefaultHL7Callback(IoSession session) {
            mSession = session;
        }

        public void onReply(String hl7MsgAck, boolean isAck) throws ApplicationException, Exception {
            mSession.write(hl7MsgAck);
        }
		
        public void sendPersistedACK(String ackMsg){        	
        	mSession.write(ackMsg);
			/*if(mSession.isConnected()) {
               CloseFuture future =  mSession.close();
            }*/
        }

        public void increaseNakSentCount() {
            long counter = (Long) mSession.getAttribute(NAK_SENT_COUNTER);
            mSession.setAttribute(NAK_SENT_COUNTER, ++counter);
        }

        public long getNakSentCount() {
            return (Long) mSession.getAttribute(NAK_SENT_COUNTER);
        }

        public void increaseCannedNakSentCount() {
            long counter = (Long) mSession.getAttribute(CANNED_NAK_SENT_COUNTER);
            mSession.setAttribute(CANNED_NAK_SENT_COUNTER, ++counter);
        }

        public long getCannedNakSentCount() {
            return (Long) mSession.getAttribute(CANNED_NAK_SENT_COUNTER);
        }

        public void closeConnection() {
            if (mSession.isConnected()) {
                CloseFuture future = mSession.close();
            }
        }

        public String getClientInfo() {
            return mSession.getRemoteAddress().toString();
        }
    }

    
}
