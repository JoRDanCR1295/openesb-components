/*
 * ProviderSEEndpoint.java
 */

package com.sun.jbi.sample.engine;

import com.sun.jbi.sample.component.common.MessageExchangeHandler;
import com.sun.jbi.sample.component.common.RuntimeHelper;
import com.sun.jbi.sample.component.common.MessageExchangeListener;
import com.sun.jbi.sample.component.common.deployment.ProviderEndpoint;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor;
import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.wsdl.Definition;

/**
 * This class extends the ProviderEndpoint to implement component specific endpoint configuration.
 * It implements the MessageExchangeListener to receive message exchange notifications from the 
 * MessageExchangeSupport fired by MessageExchangeReceiver and process it using a message exchange
 * handler.
 *
 * This endpoint is configured for a service provided by this component which can receive a xml 
 * document and apply xslt transformation and send the results back in a InOut message exchange
 * to complete a service invocation on this endpoint.
 *
 * @author chikkala
 */
public class ProviderSEEndpoint extends ProviderEndpoint implements MessageExchangeListener {
    
    private XSLTFileLocator mXsltFileLocator;
    private XSLTService mXsltService;
    
    /** Creates a new instance of XSLTProviderEndpoint */
    public ProviderSEEndpoint(SUDescriptor.Provides provides, Definition wsdlDef, ProviderSEServiceUnit su) {
        super(provides, wsdlDef, su);
    }
    /**
     * XSLT transformation service used in transformation of the xml documents received on this 
     * endpoint.
     */
    public XSLTService getXSLTService() {
        return this.mXsltService;
    }
    /**
     * returns the XSLTFileLocator object that is created to find the xslt file 
     * to be used for a particular service operation invocation by looking at the mapping
     * file specified in the service unit that deploys this endpoint.
     */
    public XSLTFileLocator getXSLTFileLocator() {
        return this.mXsltFileLocator;
    }
    
    @Override
    public void doInit() throws JBIException {
        RuntimeHelper.getLogger().info("ProviderSEEndpoint:doInit called");
        ProviderSEServiceUnit su = (ProviderSEServiceUnit) this.getServiceUnit();
        this.mXsltFileLocator = su.getXsltFileLocator();
        this.mXsltService = XSLTService.XSLTServiceImpl.getInstance();
    }
    /**
     * creates the message exchange handler that will be used to process the message exchange
     * received for this endpoint.
     */
    @Override
    public MessageExchangeHandler createMessageExchangeHandler() {
        return new ProviderSEMessageExchangeHandler(this);
    }
    /**
     * returns the implementation of the message exchange listener which the 
     * MessageExchangeSupport invokes when a message exchange is received from delivery
     * channel by MessageExchangeReceiver.
     */
    @Override
    protected MessageExchangeListener createMessageExchangeListener() {
        return this;
    }
    /**
     * implementation of the message exchange received method of the listener.
     */
    public void messageExchangeReceived(ExchangeStatus status, MessageExchange me) {
        try {
            RuntimeHelper.getLogger().fine("MXListener.MessageExchangeReceived: with Status: " + status);
            processMessageExchangeWithHandler(status, me);
            // processMessageExchange(me);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
}
