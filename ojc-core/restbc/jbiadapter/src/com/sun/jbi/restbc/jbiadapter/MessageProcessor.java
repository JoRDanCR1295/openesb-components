package com.sun.jbi.restbc.jbiadapter;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;

import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.common.qos.messaging.MessagingChannel;
import com.sun.jersey.api.client.ClientRequest;
import com.sun.jersey.api.client.ClientResponse;

import java.util.logging.Logger;


/**
 * MessageProcessor.java
 * 
 * @author Edward Chou
 */

public class MessageProcessor implements Runnable {
    
    /*
     * 71-80
     */
    private static final Logger logger = Logger.getLogger(MessageProcessor.class.getName());

    private Receiver receiver;
    private RestComponent component;
    private MessageExchange msgExchange;
    private RestSUManager suManager;
    private ComponentContext context;

    public MessageProcessor(Receiver receiver,
            RestComponent component,
            MessageExchange msgExchange,
            ComponentContext context,
            RestSUManager suManager) throws Exception {
        this.receiver = receiver;
        this.component = component;
        this.msgExchange = msgExchange;
        this.suManager = suManager;
        this.context = context;
    }
    
    /**
     * worker thread that handles each MessageExchange instance
     */
    public void run() {
        String exchangeId = msgExchange.getExchangeId();
        
        OutstandingMsgExchangeContext outstandingMsgExchangeContext = receiver.removeOutstandingMsgExchangeContext(exchangeId);
        
        if (outstandingMsgExchangeContext == null) {  // Outbound MEP

            if (msgExchange instanceof InOut) {
                // InOut exchange
                try {
                    processOutboundInOut((InOut) msgExchange);
                } catch (Exception ex) {
                    String msg = I18n.loc("RESTBC-7071: error processing MessageExchange: id={0}, {1}", exchangeId, ex);//NOI18N
                    logger.severe(msg);
                    return;
                }
            } else if (msgExchange instanceof InOnly) {
                // InOnly exchange
                try {
                    processOutboundInOnly((InOnly) msgExchange);
                } catch (Exception ex) {
                    String msg = I18n.loc("RESTBC-7071: error processing MessageExchange: id={0}, {1}", exchangeId, ex);//NOI18N
                    logger.severe(msg);
                    return;
                }
            } else {
                // unsupported exchange
                String msg = I18n.loc("RESTBC-7072: unsupported MEP: id={0}", exchangeId);//NOI18N
                logger.severe(msg);
                return;
            }
            
        } else {  // Inbound MEP
            String msg = I18n.loc("RESTBC-7073: Inbound MEP should not get here: id={0}", exchangeId);//NOI18N
            logger.severe(msg);
            return;
        }
        
        
    }
    
    private void processOutboundInOut(InOut inOut) throws Exception {
        
        if (inOut.getStatus() == ExchangeStatus.ACTIVE) {
            try {
                OutboundConfiguration outboundConfig = suManager.findActivatedEndpointOutbound(
                        inOut.getEndpoint().getServiceName(),
                        inOut.getEndpoint().getEndpointName(),
                        inOut.getOperation());
                
                if (outboundConfig == null) {
                    String msg = I18n.loc("RESTBC-7074: cannot find corresponding endpoint for this MessageExchange: id={0}", inOut.getExchangeId());//NOI18N
                    logger.severe(msg);
                    throw new Exception(msg);
                }
                
                NormalizedMessage requestMsg = inOut.getInMessage();
                
                JerseyClientWrapper clientWrapper = JerseyClientWrapper.getInstance();
                
                ClientRequest clientRequest = clientWrapper.buildClientRequest(component, requestMsg, 
                        outboundConfig);
                
                ClientResponse clientResponse = clientWrapper.makeRequest(clientRequest, requestMsg, 
                        outboundConfig);
                
                NormalizedMessage replyMsg = inOut.createMessage();
                clientWrapper.buildNormalizedReplyMessage(replyMsg, clientRequest, clientResponse, outboundConfig);
                
                inOut.setOutMessage(replyMsg);
                
            } catch (Exception e) {
                String msg = I18n.loc("RESTBC-7075: error when processing MessageExchange: id={0}, {1}", inOut.getExchangeId(), e);//NOI18N
                logger.severe(msg);
                inOut.setError(e);
            }
            
            MessagingChannel channel = new BaseMessagingChannel(context);
            inOut.setProperty(ServiceQuality.MESSAGE_ID, inOut.getExchangeId());
            channel.send(inOut);
            
        } else if (inOut.getStatus() == ExchangeStatus.DONE) {
            // currently don't do anything, but need to cleanup this ME from the redelivery cache
            // later for redelivery handling.
            
        } else {
            // status == ExchangeStatus.ERROR
            // do some redelivery handling here
            
            String msg = I18n.loc("RESTBC-7076: got ERROR status for MessageExchange: id={0}", inOut.getExchangeId());//NOI18N
            logger.severe(msg);
            throw new Exception(msg);
        }
    }
    
    private void processOutboundInOnly(InOnly inOnly) throws Exception {
        
        if (inOnly.getStatus() == ExchangeStatus.ACTIVE) {
            try {
                OutboundConfiguration outboundConfig = suManager.findActivatedEndpointOutbound(
                        inOnly.getEndpoint().getServiceName(),
                        inOnly.getEndpoint().getEndpointName(),
                        inOnly.getOperation());
                
                if (outboundConfig == null) {
                    String msg = I18n.loc("RESTBC-7074: cannot find corresponding endpoint for this MessageExchange: id={0}", inOnly.getExchangeId());//NOI18N
                    logger.severe(msg);
                    throw new Exception(msg);
                }
                
                NormalizedMessage requestMsg = inOnly.getInMessage();
                
                JerseyClientWrapper clientWrapper = JerseyClientWrapper.getInstance();
                
                ClientRequest clientRequest = clientWrapper.buildClientRequest(component, requestMsg, outboundConfig);
                
                ClientResponse clientResponse = clientWrapper.makeRequest(clientRequest, requestMsg, 
                        outboundConfig);
                
                if (clientResponse.getStatus() >= 400) {
                    Exception e = new Exception("client response " + clientResponse.getResponseStatus());
                    inOnly.setError(e);
                } else {
                    inOnly.setStatus(ExchangeStatus.DONE);
                }
                
            } catch (Exception e) {
                String msg = I18n.loc("RESTBC-7075: error when processing MessageExchange: id={0}, {1}", inOnly.getExchangeId(), e);//NOI18N
                logger.severe(msg);
                inOnly.setError(e);
            }
            
            MessagingChannel channel = new BaseMessagingChannel(context);
            inOnly.setProperty(ServiceQuality.MESSAGE_ID, inOnly.getExchangeId());
            channel.send(inOnly);
            
        } else if (inOnly.getStatus() == ExchangeStatus.DONE) {
            // currently don't do anything, but need to cleanup this ME from the redelivery cache
            // later for redelivery handling.
            
        } else {
            // status == ExchangeStatus.ERROR
            // do some redelivery handling here
            
            String msg = I18n.loc("RESTBC-7076: got ERROR status for MessageExchange: id={0}", inOnly.getExchangeId());//NOI18N
            logger.severe(msg);
            throw new Exception(msg);
        }
    }
 
}

