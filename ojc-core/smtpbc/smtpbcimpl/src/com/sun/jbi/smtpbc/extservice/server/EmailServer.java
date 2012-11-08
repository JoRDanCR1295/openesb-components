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
 * @(#)EmailServer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.smtpbc.extservice.server;

import java.net.InetSocketAddress;
import java.nio.charset.Charset;
import java.util.HashMap;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.mina.common.ByteBuffer;
import org.apache.mina.common.IoAcceptor;
import org.apache.mina.common.SimpleByteBufferAllocator;

import org.apache.mina.filter.codec.ProtocolCodecFilter;
import org.apache.mina.filter.codec.textline.TextLineCodecFactory;

import org.apache.mina.transport.socket.nio.SocketAcceptor;
import org.apache.mina.transport.socket.nio.SocketAcceptorConfig;

import com.sun.jbi.internationalization.Messages;


/**
 *
 *
 * @author       Alexander Fung
 * @version      $Revision: 1.4 $
 *
 */
public class EmailServer {

    private static final Logger log = Messages.getLogger(EmailServer.class);
    
    private static final Messages mMessages =  Messages.getMessages(EmailServer.class);
	
    public static final int mDefaultPort = 25;

    private IoAcceptor acceptor;
    private SocketAcceptorConfig cfg;
    
    private HashMap<Integer,HashMap> portAddressMap;

    public EmailServer() {
        
        
        ByteBuffer.setUseDirectBuffers(false);
        ByteBuffer.setAllocator(new SimpleByteBufferAllocator());

        acceptor = new SocketAcceptor();

        cfg = new SocketAcceptorConfig();
        cfg.getSessionConfig().setReuseAddress( true );
        cfg.getFilterChain().addLast( "codec", new ProtocolCodecFilter( new TextLineCodecFactory( Charset.forName( "US-ASCII" ))));
        
        portAddressMap = new HashMap<Integer,HashMap>();
        
    }

    public void startEmailService(final EmailListener listener,
                                  final String name,
                                  final int port)
        throws Exception {

        HashMap<InetSocketAddress,EmailEventHandler> addressHandlerMap = portAddressMap.get(Integer.valueOf(port));
        EmailEventHandler handler = null;
        InetSocketAddress socketAddress = null;
        if(addressHandlerMap == null){
        	
        	Integer objPort = Integer.valueOf(port);
        	socketAddress = new InetSocketAddress(port);
        	
        	handler = new EmailEventHandler();
            handler.addEmailListener(listener);
            
            addressHandlerMap = new HashMap<InetSocketAddress,EmailEventHandler>();
            addressHandlerMap.put(socketAddress,handler);
            portAddressMap.put(objPort,addressHandlerMap);
        	
        }else{
        	socketAddress = addressHandlerMap.keySet().iterator().next();
        	handler = addressHandlerMap.get(socketAddress);
        }
        
    	
        
        acceptor.bind( socketAddress, handler, cfg);
        log.log(Level.FINE,mMessages.getString("EmailServer.serverListening",Integer.valueOf(port)));
        
    }

    public void stopEmailService(final String name, final int port) {
        acceptor.unbind(new InetSocketAddress(port));
        
        portAddressMap.remove(Integer.valueOf(port));
        
        log.log(Level.FINE,mMessages.getString("EmailServer.stoppedListening",Integer.valueOf(port)));
    }

    public void stopAllServices() throws Exception {
        acceptor.unbindAll();
        portAddressMap = new HashMap<Integer,HashMap>();
        log.log(Level.FINE,mMessages.getString("EmailServer.stoppedAllListening"));
    }

    public static void main(final String[] args) throws Exception {
        final EmailServer server = new EmailServer();
        final AddressBook book = new AddressBook();
        book.addEmailAddress("afung@afung-gx270xp.stc.com");
        server.startEmailService(new DefaultEmailListener(book),
                                 "testServer", EmailServer.mDefaultPort);
    }
}
