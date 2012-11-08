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
 * @(#)TCPIpSwiftConnector.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extservice.client;

import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.Condition;

import org.apache.mina.protocol.ProtocolHandlerAdapter;

import com.sun.jbi.swiftbc.extensions.SAGObjectFactory;
import com.sun.jbi.swiftbc.extensions.SAGObjectFactoryFactory;
import com.sun.jbi.swiftbc.extensions.SwiftLetter;
import com.sun.jbi.swiftbc.extensions.SwiftEnvelope;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import com.sun.jbi.swiftbc.extensions.jni.HandleClient;
import com.sun.jbi.swiftbc.extensions.jni.HandleFactory;
import com.sun.jbi.swiftbc.extensions.jni.SAGJNIMessage;
import com.sun.jbi.swiftbc.extservice.ProtocolInfo;
import com.sun.jbi.internationalization.Messages;


/**
 * This class creates and manages connections to the Swift External System.
 * @author S. Nageswara Rao
 */
public class TCPIpSwiftConnector extends ProtocolHandlerAdapter implements SwiftConnector {
    
    private static final Messages mMessages = Messages.getMessages(TCPIpSwiftConnector.class);
    
    private static final Logger log = Messages.getLogger(TCPIpSwiftConnector.class);
    
    
    private String SwiftACK = null;
    private HandleFactory hf;
    private HandleClient handleClient ;
    final Lock lock = new ReentrantLock();
    final Condition ackNotRecvd  = lock.newCondition();
    private SAGObjectFactoryFactory soff = new SAGObjectFactoryFactory();
    private SAGObjectFactory sof = soff.getObjectFactory();
    
    public TCPIpSwiftConnector() { 	
    }
    
    public void connect(ProtocolInfo protocolInfo) throws Exception {
        //String hostName = protocolInfo.get(SwiftAddress.ATTR_SWIFT_SVR_LOCATION);
        //int port = Integer.parseInt(protocolInfo.get(SwiftAddress.ATTR_SWIFT_SVR_PORT));
    	String configFile = System.getProperty("sagConfigFile");
    	if( configFile == null ){
    		Exception e = new Exception("SAG configuration file not specified:");
    		log.log(Level.SEVERE, mMessages.getString("SwiftConnector_AN_EXCEPTION"), e);
    		throw e;
    	}
    	hf = HandleFactory.newFactory(configFile);
    	handleClient = hf.newHClient(10000);
        
    }
    
    public void sendSwiftMessage(String SwiftMsg) throws Exception {
    	SwiftMessage message = sof.getNewMessage();
    	SwiftLetter letter = sof.getNewLetter();
    	message.setLetter(letter);
    	handleClient.putRequest((SAGJNIMessage)message);
    }
    
    public String recvSwiftMessage() throws Exception {
        lock.lock();
        try {
            while (SwiftACK == null)
                ackNotRecvd.await();
        } catch (Exception ex) {
            log.log(Level.SEVERE, mMessages.getString("SwiftConnector_AN_EXCEPTION"), ex);
            String errMsg = mMessages.getString("SwiftConnector_AN_EXCEPTION");
            throw new Exception(errMsg, ex);
        } finally {
            lock.unlock();
        }
        return SwiftACK;
    }

    public String sendReceiveSwiftMessage(String SwiftMsg) throws Exception {
    	SwiftMessage message = sof.getNewMessage();
    	SwiftLetter letter = message.getLetter();
        letter.set(SwiftMsg.length(), SwiftMsg);
        SwiftEnvelope envelope = message.getEnvelope();
        envelope.setApplicationId("harry_c");
        envelope.setContextId("Advanced");
        envelope.setMsgFormat("Sag:RelaxedSNL");
                envelope.setSender("Administrator");
        envelope.setSenderAuth("Swift2007sol");
    	message.setLetter(letter);
        message.setEnvelope(envelope);
    	SwiftMessage response  = sof.getNewMessage();
    	handleClient.call((SAGJNIMessage)message, (SAGJNIMessage)response);
    	return response.getLetter().getBuffer();
    	
    }

    public void disconnect() throws Exception {
   }
 
}
