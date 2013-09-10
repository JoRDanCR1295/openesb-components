/*
 * ProviderSEMessageExchangeHandler.java
 *
 */
package net.openesb.component.ServiceEngine-archetype;

import net.openesb.component.ServiceEngine-archetype.common.AbstractMessageExchangeHandler;
import net.openesb.component.ServiceEngine-archetype.common.RuntimeHelper;
import net.openesb.component.ServiceEngine-archetype.common.wsdl.WSDL11JBIWrapper;
import java.io.StringReader;
import java.util.logging.Logger;
import javax.jbi.JBIException;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;

/**
 * This class extends the AbstractMessageExchangeHandler to implement the
 * component specific message exchange processing on the provider side in a
 * service engine.
 *
 * @author chikkala
 */
public class ProviderSEMessageExchangeHandler extends AbstractMessageExchangeHandler {
    
    private ProviderSEEndpoint mEndpoint;

    /**
     * Creates a new instance of ProviderSEMXHandler
     */
    public ProviderSEMessageExchangeHandler(ProviderSEEndpoint endpoint) {
        this.mEndpoint = endpoint;
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
        Source inContent = null;
        Source outContent = null;
        Source faultContent = null;
        String faultAsText = null;
        // process in message
        inContent = inMsg.getContent();
        // invoke the service operation
        try {
            QName opName = inOutExchange.getOperation();
            QName svcName = inOutExchange.getEndpoint().getServiceName();
            RuntimeHelper.getLogger().fine("xslt service info " + svcName + opName);
            
            XSLTFileLocator xsltFileLocator = this.mEndpoint.getXSLTFileLocator();
            XSLTService xsltService = this.mEndpoint.getXSLTService();
            
            String xsltPath = xsltFileLocator.findXsltFile(svcName, opName);
            RuntimeHelper.getLogger().fine("XsltPath " + xsltPath);
            if (xsltPath == null) {
                RuntimeHelper.getLogger().fine(" No XSLT File found in MAP "
                        + xsltFileLocator.printMap());
            }
            outContent = xsltService.transform(inContent, xsltPath);
            
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
            normalizeOutMessage(inOutExchange, outMsg, outContent);
            // outMsg.setContent(outContent);
        } else if (faultContent != null) {
            fault = inOutExchange.createFault();
            inOutExchange.setFault(fault);
            fault.setContent(faultContent);  // may need to normalize the content.
        }
        // send out or fault message.
        // this.getDeliveryChannel().sendSync(inOutExchange, SEND_SYNC_TIMEOUT);
        this.send();
    }
    
    private void normalizeOutMessage(MessageExchange me, NormalizedMessage normMsg, Source msgSrc) throws MessagingException {
        try {
            // DOMSource wrappedSource = wrapOutputMessage(me, msgSrc);
            WSDL11JBIWrapper outWrapper = WSDL11JBIWrapper.createOutputWrapper(
                    this.mEndpoint.getWSDLOperation(me.getOperation()));

            // check if the output from the transformer is in a wrapped form
            WSDL11JBIWrapper wrapper = WSDL11JBIWrapper.sourceToWrapper(
                    RuntimeHelper.sourceToDOMSource(msgSrc));
            if (wrapper != null) {
                outWrapper.appendParts(wrapper.getParts());
            } else {
                outWrapper.appendPart(RuntimeHelper.sourceToDOMSource(msgSrc));
            }
            
            normMsg.setContent(outWrapper.toDOMSource());
            
        } catch (MessagingException ex) {
            throw ex;
        } catch (ParserConfigurationException ex) {
            throw new MessagingException(ex);
        }
    }
}
