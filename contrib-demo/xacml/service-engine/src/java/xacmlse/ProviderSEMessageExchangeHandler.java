/*
 * ProviderSEMessageExchangeHandler.java
 *
 */

package xacmlse;

import com.sun.jbi.sample.component.common.AbstractMessageExchangeHandler;
import com.sun.jbi.sample.component.common.RuntimeHelper;
import com.sun.jbi.sample.component.common.wsdl.WSDL11JBIWrapper;
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
 * This class extends the AbstractMessageExchangeHandler to implement the component specific
 * message exchange processing on the provider side in a service engine.
 *
 * @author chikkala
 */
public class ProviderSEMessageExchangeHandler extends AbstractMessageExchangeHandler {
    
    private ProviderSEEndpoint mEndpoint;
    
    /** Creates a new instance of ProviderSEMXHandler */
    public ProviderSEMessageExchangeHandler(ProviderSEEndpoint endpoint) {
        this.mEndpoint = endpoint;
    }
    @Override
    protected Logger getLogger() {
        return this.mEndpoint.getLogger();
    }
    
    @Override
    protected DeliveryChannel getDeliveryChannel() {
        return this.mEndpoint.getDeliveryChannel();
    }
    
    @Override
    protected void validateMessageExchange() throws MessagingException {
        MessageExchange msgExchange = this.getMessageExchange();
        
        if ( this.getMessageExchange() == null ) {
            throw new MessagingException("MessageExchange Object is null in MessageExchageHandler");
        }
        
        if ( MessageExchange.Role.CONSUMER.equals(msgExchange.getRole()) ) {
            throw new MessagingException("Provider Message Exchange Handler can not have MessageExchange with CONSUMER Role");
        }
        
        if (!(msgExchange instanceof InOut) ) {
            throw new MessagingException("InOut Message Exchange Handler MessageExchange object should be instanceof javax.jbi.messaging.InOut ");
        }
    }
    
    @Override
    protected void processError(Exception ex) {
        MessageExchange msgExchange = this.getMessageExchange();
        Exception errEx = msgExchange.getError(); // get the error and print
        RuntimeHelper.getLogger().info(
                "InOut Message Exchange Provider received Error: " + errEx.getMessage());
        msgExchange.getError().printStackTrace();
    }
    
    @Override
    protected void processDone() {
        MessageExchange msgExchange = this.getMessageExchange();
        RuntimeHelper.getLogger().info("InOut Message Exchange Provider received DONE :" +
                " END of service invocation");
    }
    
    @Override
    protected void processFault(Fault fault) {
        MessageExchange msgExchange = this.getMessageExchange();
        RuntimeHelper.logError("InOut Message Exchange Provider Handler can not receive Fault on Provider side");
    }
    
    @Override
    protected void processMessage() {
        try {
            processInMessageOnProvider((InOut)this.getMessageExchange());
        } catch (JBIException ex) {
            ex.printStackTrace();
        }
    }
    
    protected void processInMessageOnProvider(InOut inOutExchange) throws JBIException {
        RuntimeHelper.getLogger().fine("Processing In Message on Provider side " + inOutExchange );
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
            RuntimeHelper.getLogger().fine("XACML service info " + svcName + opName);
            XACMLService pep = this.mEndpoint.getPolicyService();
            XACMLFileLocator fileLocator = this.mEndpoint.getXSLTFileLocator();
            String filePath = fileLocator.findXacmlFile(svcName, opName);
            RuntimeHelper.getLogger().fine("XacmlPath " + filePath);
            if ( filePath == null ) {
                RuntimeHelper.getLogger().fine(" No XACML File found in MAP " +
                        fileLocator.printMap());
            }
            else if (pep.config(filePath)) 
                // making sure the file is there and proper. sblais 14 nov 2007
                outContent = pep.evaluatePolicy(inOutExchange);
            else {
                faultAsText = "Can't find the XACML Policy File.";
                faultContent = RuntimeHelper.createDOMSource(new StringReader("<Fault>"+faultAsText+"</Fault>"));
            }
            
        } catch (Exception ex) {
            // exception invoking the operation. so, set exception text as fault content.
            ex.printStackTrace();
            faultAsText = RuntimeHelper.getExceptionAsText(ex);
            String faultText = RuntimeHelper.getExceptionAsXmlText(ex);
            faultContent = RuntimeHelper.createDOMSource(new StringReader(faultText));
        }
        // set out or fault message
        if ( outContent != null ) {
            // set the out message content.
            outMsg = inOutExchange.createMessage();
            inOutExchange.setOutMessage(outMsg);
            normalizeOutMessage(inOutExchange, outMsg, outContent);
            // outMsg.setContent(outContent);
        } else if ( faultContent != null ) {
            fault = inOutExchange.createFault();
            inOutExchange.setFault(fault);
            fault.setContent(faultContent);  // may need to normalize the content.
        }
        // send out or fault message.
        // this.getDeliveryChannel().sendSync(inOutExchange, SEND_SYNC_TIMEOUT);
        this.send();
    }
    
    private void normalizeOutMessage(MessageExchange me, NormalizedMessage normMsg, Source msgSrc ) throws MessagingException {
        try {
            // DOMSource wrappedSource = wrapOutputMessage(me, msgSrc);
            WSDL11JBIWrapper outWrapper = WSDL11JBIWrapper.createOutputWrapper(
                    this.mEndpoint.getWSDLOperation(me.getOperation()));
            
            // check if the output from the transformer is in a wrapped form
            WSDL11JBIWrapper wrapper = WSDL11JBIWrapper.sourceToWrapper(
                    RuntimeHelper.sourceToDOMSource(msgSrc));
            if ( wrapper != null ) {
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
