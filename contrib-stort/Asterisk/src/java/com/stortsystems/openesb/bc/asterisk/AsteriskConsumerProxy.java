/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AsteriskConsumerProxy.java,v 1.3 2008/04/15 22:49:41 l_siqueira Exp $
 */

package com.stortsystems.openesb.bc.asterisk;

import java.io.*;
import java.net.*;

import com.stortsystems.helios.manager.*;

import com.sun.jbi.sample.component.common.RuntimeHelper;
import com.sun.jbi.sample.component.common.deployment.ConsumerEndpoint;
import java.io.StringReader;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.jbi.messaging.MessagingException;
import javax.jbi.JBIException;
import java.util.*;


public class AsteriskConsumerProxy extends Thread implements ManagerEventListener {

    private AsteriskConsumerEndpoint cendpoint;
    private ManagerSessionEvent session;
    private static final long SEND_SYNC_TIMEOUT = 60000;
    private AsteriskNormalizer mNormalizer;
    private static int i = 0;
    
    public AsteriskConsumerProxy(AsteriskConsumerEndpoint ep, String[] props) 
            throws UnknownHostException, IOException, ManagerSessionException {
        
        cendpoint = ep;
        session = new ManagerSessionEvent(props[0], Integer.parseInt(props[1]));
        session.addListener(this);
        session.login(props[2], props[3]);
        
        if ( props[4].trim().length() > 0 ) {
            
            String events[] = props[4].split(" ");
            ManagerEventFilter filter = new ManagerEventFilter();
            for ( int i = 0; i < events.length; ++i ) {
                filter.addEventType(events[i]);
                System.out.println("Asterisk-BC DEBUG: added event: " + events[i]);
            }
            session.setEventFilter( filter );
        }
        
        this.mNormalizer = new AsteriskNormalizer(cendpoint.getWSDL(), cendpoint.getWSDLBinding());
        
    }
    
    public void disconnect() throws IOException, ManagerSessionException {
        
        session.disconnect();
        
    }
    
    @Override
    public void run() {
        try {
            session.waitEvents();
        } catch ( IOException e ) {}
        
    }
    
    @Override
    public void interrupt() {
        super.interrupt();
   
    }
    
   private void doInOnlyMessageExchange(QName operation, Source inSource)
           throws Exception {
       // 2. normalized the message.
       // 3. locate service endpoint
       // 4. create message exchange according to the Opeations MEP
             // get the component context and the delivery channel for preparing to send message
       ComponentContext compContext = this.cendpoint.getComponentContext();
       DeliveryChannel channel = this.cendpoint.getDeliveryChannel();
       // create INOUT Message Exchange
       InOnly inOnlyME = this.cendpoint.createInOnlyMessageExchange(operation);
       // set the content of the IN normalized message ( Normalize the message )
       NormalizedMessage inMsg = inOnlyME.getInMessage();
       Operation wsdlOperation = this.cendpoint.getWSDLOperation(inOnlyME.getOperation());
       
       this.mNormalizer.normalizeInput(wsdlOperation, inMsg, RuntimeHelper.sourceToDOMSource(inSource));
       // send the message exchange and wait for response
       boolean isSent = channel.sendSync(inOnlyME, SEND_SYNC_TIMEOUT);
       if ( !isSent ) {
           throw new Exception("JMXBinding:Timeout occured in sending the message to provider");
       }
       // check if you got a done message or error ( done or error are only allowed in in-only)
       // process the Message Exchange to check for done or error message and
       // complete InOut message exchange with provider
       //TODO: put this below code in processInOnlyMessageExchangeOnConsumer()
       ExchangeStatus status = inOnlyME.getStatus();
       this.cendpoint.getLogger().fine("Consumer:InOnly:Processing Message Exchange with status " + status);
       if (ExchangeStatus.DONE.equals(status) ) {
           this.cendpoint.getLogger().fine("Consumer: Completed the INONLY MessageExchange");
           return;
       } else if (ExchangeStatus.ERROR.equals(status) ) {
           // error can occur any time. so just return the error back to client.
           Exception serverSideEx = inOnlyME.getError();
           StringBuffer exMsgBuff = RuntimeHelper.getExceptionStackTrace(serverSideEx);
           throw new Exception("Consumer:INONLY Message Exchange status ERROR.\n" + exMsgBuff);
       } else {
           // any other status is error.
           throw new Exception("Consumer:INONLY Message Exchange error. status: " + status);
       }          } 
    
    public void onEvent( ManagerEvent event ) {
        
        StringBuffer msg = new StringBuffer();
        msg.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        msg.append("<Asterisk-event xmlns=\"http://www.stortsystems.com/asterisk-bc/events\">\n");
        msg.append("<Event>\n");
        //System.out.println("DEBUG - " + event.getXML());
        msg.append(event.getXML());
        /*              <Event>Newchannel</Event>
                        <Privilege>call,all</Privilege>
                        <Channel>OSS/dsp</Channel>
                        <State>Ringing</State>
                        <CallerID><unknown></CallerID>
                        <CallerIDName><unknown></CallerIDName>
                        <Uniqueid>1196291190.99</Uniqueid> */                            
        msg.append("</Event>\n");
        msg.append("</Asterisk-event>\n");
        
        Source inMsgSource = RuntimeHelper.createDOMSource(new StringReader(msg.toString()));
        
        String operation = this.cendpoint.getOperationNameStr();
        QName operationQName = this.cendpoint.getOperationQName(operation);
        
        try {
            doInOnlyMessageExchange(operationQName, inMsgSource);
        } catch ( Exception e ) {
            System.out.println( e );
        }
        
    }
    
}
