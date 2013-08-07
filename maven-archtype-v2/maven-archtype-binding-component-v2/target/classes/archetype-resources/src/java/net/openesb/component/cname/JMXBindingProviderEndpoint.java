#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * JMXBindingProviderEndpoint.java
 *
 */
package net.openesb.component.${artifactId};

import net.openesb.component.${artifactId}.wsdlext.PortExt;
import net.openesb.component.${artifactId}.wsdlext.WSDLExtHelper;
import net.openesb.component.${artifactId}.common.MessageExchangeHandler;
import net.openesb.component.${artifactId}.common.RuntimeHelper;
import net.openesb.component.${artifactId}.common.MessageExchangeListener;
import net.openesb.component.${artifactId}.common.deployment.ProviderEndpoint;
import net.openesb.component.${artifactId}.common.deployment.SUDescriptor;
import net.openesb.component.${artifactId}.common.deployment.ServiceUnit;
import java.util.logging.Level;
import javax.jbi.JBIException;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessageExchange;
import javax.wsdl.Definition;
import javax.xml.namespace.QName;

/**
 * This class extends the ProviderEndpoint to implement component specific
 * endpoint configuration. It implements the MessageExchangeListener to receive
 * message exchange notifications from the MessageExchangeSupport fired by
 * MessageExchangeReceiver and process it using a message exchange handler to
 * show how an asynchronous processing of message exchange can be done.
 *
 * This endpoint uses the JMXBindingProviderProxy as a message exchange handler
 * that can invoke the external provider to send a message and receive the
 * response in a InOut message exchange.
 *
 * @author chikkala
 */
public class JMXBindingProviderEndpoint extends ProviderEndpoint implements MessageExchangeListener {
    
    JMXBindingProviderProxy mProviderProxy;

    /**
     * Creates a new instance of XSLTProviderEndpoint
     */
    public JMXBindingProviderEndpoint(SUDescriptor.Provides provides, Definition wsdlDef, ServiceUnit su) {
        super(provides, wsdlDef, su);
    }
    
    @Override
    public void doInit() throws JBIException {
        RuntimeHelper.getLogger().info("JMXBindingProviderEndpoint:doInit called");
        this.mProviderProxy = new JMXBindingProviderProxy(this);
    }
    
    @Override
    public MessageExchangeHandler createMessageExchangeHandler() {
        return this.mProviderProxy;
    }
    
    @Override
    protected MessageExchangeListener createMessageExchangeListener() {
        return this;
    }
    
    public void messageExchangeReceived(ExchangeStatus status, MessageExchange me) {
        try {
            RuntimeHelper.getLogger().fine("MXListener.MessageExchangeReceived: with Status: " + status);
            processMessageExchangeWithHandler(status, me);
            // processMessageExchange(me);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }
    
    public PortExt getJMXProviderInfo() {
        PortExt portExt = null;
        try {
            QName serviceName = this.getService().getServiceName();
            String endpointName = this.getService().getEndpointName();
            Definition def = this.getWSDL();
            portExt = WSDLExtHelper.getPortExt(def, serviceName, endpointName);
        } catch (Exception ex) {
            getLogger().log(Level.FINE, ex.getMessage(), ex);
        }
        return portExt;
    }
}
