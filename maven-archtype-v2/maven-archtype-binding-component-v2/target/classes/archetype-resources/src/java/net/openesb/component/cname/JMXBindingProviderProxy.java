#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * JMXBindingProviderProxy.java
 *
 */
package net.openesb.component.${artifactId};

import net.openesb.component.${artifactId}.wsdlext.PortExt;
import net.openesb.component.${artifactId}.common.AbstractMessageExchangeHandler;
import net.openesb.component.${artifactId}.common.RuntimeHelper;
import java.io.IOException;
import java.io.StringReader;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.management.MBeanServerConnection;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import javax.wsdl.Operation;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

/**
 * This is a provider side implementation of the message exchange handler on a
 * binding component which acts as a service provider proxy for the external
 * provider. During the message exchange processing of the InOut message
 * exchange, this handler acting as a proxy to the external provider invokes the
 * service operation on the external provider and does required normalization
 * and denormalization in processing out and in messages received from or send
 * to the external service provider.
 *
 * @author chikkala
 */
public class JMXBindingProviderProxy extends AbstractMessageExchangeHandler {
    
    private JMXBindingProviderEndpoint mEndpoint;
    private String mServiceURL;
    private String mUsername;
    private String mPassword;
    private String mMBeanName;
    private JMXBindingNormalizer mNormalizer;

    /**
     * Creates a new instance of ProviderSEMXHandler
     */
    public JMXBindingProviderProxy(JMXBindingProviderEndpoint endpoint) {
        
        this.mEndpoint = endpoint;
        
        PortExt jmxInfo = this.mEndpoint.getJMXProviderInfo();
        this.mServiceURL = jmxInfo.getServiceURL();
        this.mUsername = jmxInfo.getUsername();
        this.mPassword = jmxInfo.getPassword();
        this.mMBeanName = jmxInfo.getMBean();
        
        this.mNormalizer = new JMXBindingNormalizer(endpoint.getWSDL(), endpoint.getWSDLBinding());
    }
    
    protected Logger getLogger() {
        return this.mEndpoint.getLogger();
    }
    
    protected DeliveryChannel getDeliveryChannel() {
        return this.mEndpoint.getDeliveryChannel();
    }
    
    protected void validateMessageExchange() throws MessagingException {
        MessageExchange msgExchange = this.getMessageExchange();
        
        if (this.getMessageExchange() == null) {
            throw new MessagingException("MessageExchange Object is null in MessageExchageHandler");
        }
        
        if (MessageExchange.Role.CONSUMER.equals(msgExchange.getRole())) {
            throw new MessagingException("Provider Message Exchange Handler can not have MessageExchange with CONSUMER Role");
        }
        
        if (!(msgExchange instanceof InOut)) {
            throw new MessagingException("InOut Message Exchange Handler MessageExchange object should be instanceof javax.jbi.messaging.InOut ");
        }
    }
    
    protected void processError(Exception ex) {
        MessageExchange msgExchange = this.getMessageExchange();
        Exception errEx = msgExchange.getError(); // get the error and print
        RuntimeHelper.getLogger().info(
                "InOut Message Exchange Provider received Error: " + errEx.getMessage());
        msgExchange.getError().printStackTrace();
    }
    
    protected void processDone() {
        MessageExchange msgExchange = this.getMessageExchange();
        RuntimeHelper.getLogger().info("InOut Message Exchange Provider received DONE :"
                + " END of service invocation");
    }
    
    protected void processFault(Fault fault) {
        MessageExchange msgExchange = this.getMessageExchange();
        RuntimeHelper.logError("InOut Message Exchange Provider Handler can not receive Fault on Provider side");
    }
    
    protected void processMessage() {
        try {
            processInMessageOnProvider((InOut) this.getMessageExchange());
        } catch (JBIException ex) {
            ex.printStackTrace();
        }
    }
    
    protected void processInMessageOnProvider(InOut inOutExchange) throws JBIException {
        RuntimeHelper.getLogger().fine("Processing In Message on Provider side " + inOutExchange);
        // receive IN message.
        NormalizedMessage inMsg = inOutExchange.getInMessage();
        NormalizedMessage outMsg = null;
        Fault fault = null;
        DOMSource inContent = null;
        DOMSource outContent = null;
        Source faultContent = null;
        String faultAsText = null;
        
        QName opName = inOutExchange.getOperation();
        QName svcName = inOutExchange.getEndpoint().getServiceName();
        Operation wsdlOperation = this.mEndpoint.getWSDLOperation(opName);

        // process in message
        // invoke the service operation
        try {
            inContent = this.mNormalizer.denormalizeInput(wsdlOperation, inMsg);
            StringBuffer outputDoc = invokeOperation(opName.getLocalPart(),
                    RuntimeHelper.readFromDOMSource(inContent));
            outContent = RuntimeHelper.createDOMSource(new StringReader(outputDoc.toString()));
        } catch (Exception ex) {
            // exception invoking the operation. so, set exception text as fault content.
            ex.printStackTrace();
            faultAsText = RuntimeHelper.getExceptionAsText(ex);
            String faultText = RuntimeHelper.getExceptionAsXmlText(ex);
            faultContent = RuntimeHelper.createDOMSource(new StringReader(faultText));
        }
        // set out or fault message
        if (outContent != null) {
            // set the out message content.
            outMsg = inOutExchange.createMessage();
            inOutExchange.setOutMessage(outMsg);
            this.mNormalizer.normalizeOutput(wsdlOperation, outMsg, outContent);
            // outMsg.setContent(outContent);
        } else if (faultContent != null) {
            fault = inOutExchange.createFault();
            inOutExchange.setFault(fault);
            fault.setContent(faultContent);  // may need to normalize the content.
        }
        // send out or fault message.
        this.send();
    }
    
    protected StringBuffer invokeOperation(String operation, StringBuffer inputDoc) throws MessagingException {
        try {
            StringBuffer outputDoc = invokeSendMessage(this.mServiceURL, this.mUsername, this.mPassword,
                    this.mMBeanName, operation, inputDoc);
            return outputDoc;
        } catch (Exception ex) {
            ex.printStackTrace();
            throw new MessagingException(ex);
        }
    }

    /**
     * invokes a jmx mbean to send message.
     */
    protected StringBuffer invokeSendMessage(String serviceURL, String username, String password,
            String mbeanName, String operation, StringBuffer inputDoc)
            throws MessagingException {
        
        JMXConnector jmxConnector = null;
        MBeanServerConnection jmxConn = null;
        JMXServiceURL jmxURL = null;
        ObjectName epAddressName = null;
        
        Map<String, Object> env = new HashMap<String, Object>();
        if (username != null) {
            String[] credentials = new String[]{username, password};
            env.put("jmx.remote.credentials", credentials);
            env.put(JMXConnectorFactory.DEFAULT_CLASS_LOADER, this.getClass().getClassLoader());
        }
        
        StringBuffer outDoc = null;
        
        Object result = null;
        String mbeanOperation = "sendMessage";
        
        Object[] params = new Object[2];
        params[0] = operation;
        params[1] = inputDoc;
        
        String[] signature = new String[2];
        signature[0] = "java.lang.String";
        signature[1] = "java.lang.StringBuffer";
        
        try {
            jmxURL = new JMXServiceURL(serviceURL);
            epAddressName = new ObjectName(mbeanName);
            
            jmxConnector = JMXConnectorFactory.connect(jmxURL, env);
            jmxConn = jmxConnector.getMBeanServerConnection();
            
            result = jmxConn.invoke(epAddressName, mbeanOperation, params, signature);
            if (result != null) {
                outDoc = (StringBuffer) result;
            }
        } catch (Exception ex) {
            throw new MessagingException(ex);
        } finally {
            if (jmxConnector != null) {
                try {
                    jmxConnector.close();
                } catch (IOException ex) {
                    RuntimeHelper.logDebug(ex);
                    // ingore
                }
            }
        }
        return outDoc;
    }
}
