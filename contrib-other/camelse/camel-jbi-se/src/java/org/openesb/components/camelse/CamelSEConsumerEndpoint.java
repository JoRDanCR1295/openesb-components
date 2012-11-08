/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.openesb.components.camelse;

import java.util.ArrayList;
import java.util.List;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.Fault;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.openesb.components.camelse.JBIWrapperUtil;
import org.openesb.components.camelse.common.RuntimeHelper;
import org.openesb.components.camelse.common.deployment.ConsumerEndpoint;
import org.openesb.components.camelse.common.deployment.SUDescriptor;
import org.openesb.components.camelse.common.deployment.ServiceUnit;
import org.openesb.components.camelse.common.wsdl.WSDL11JBIWrapper;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author chikkala
 */
public class CamelSEConsumerEndpoint  extends ConsumerEndpoint {
    private static final String XMLNS_NS = "http://www.w3.org/2000/xmlns/";
    private static final long SEND_SYNC_TIMEOUT = 60000; // 1 minute
    
    /** Creates a new instance of CamelSEConsumerEndpoint */
    public CamelSEConsumerEndpoint(SUDescriptor.Consumes consumes, Definition wsdlDef, ServiceUnit su) {
        super(consumes, wsdlDef, su);
    }
    
    @Override
    protected void doInit() throws JBIException {
        RuntimeHelper.getLogger().fine("CamelSEConsumerEndpoint(Consumer): init called");
        this.initCamelEndpoint();
    }
    
    @Override
    protected void doActivate() throws JBIException {
        RuntimeHelper.getLogger().fine("CamelSEConsumerEndpoint(Consumer): activate called");
        this.activateCamelEndpoint();
    }
    
    @Override
    protected void doDeactivate() throws JBIException {
        RuntimeHelper.getLogger().fine("CamelSEConsumerEndpoint(Consumer): deactivate called");
        this.deactivateCamelEndpoint();
    }
    
    @Override
    protected void doClean() throws JBIException {
        RuntimeHelper.getLogger().fine("CamelSEConsumerEndpoint(Consumer): clean called");
        this.cleanCamelEndpoint();
    }

    private void activateCamelEndpoint() {
        //NOOP
    }

    private void cleanCamelEndpoint() {
        //NOOP
    }

    private void deactivateCamelEndpoint() {
        //NOOP
    }

    private void initCamelEndpoint() {
        //NOOP
    }
    
//    /**
//     * initiate exchange with a sepcified operation.
//     * @param operation operation local name or qname as string. Assumes the service endpoint's 
//     * interface ns for operations ns
//     * @param inSource
//     * @return
//     * @throws javax.jbi.JBIException
//     */
//    public Source initiateMessageExchange(String operation, Source inSource) throws JBIException {
//        QName opQName = null;
//        if (operation != null && operation.trim().length() > 0) {
//            QName qn = QName.valueOf(operation);
//            if (qn.getNamespaceURI().trim().length() > 0) {
//                opQName = qn;
//            } else {
//                opQName = this.getOperationQName(operation);
//            }
//        }
//        return initiateMessageExchange(opQName, inSource); // with default operation.
//    }    
    
    /**
     * 
     * @param operation openration name. can be null for default operation.
     * @param inSource
     * @return outSource if any. else null.
     * @throws javax.jbi.JBIException on  exception
     */
    public Source initiateMessageExchange(QName operation, Source inSource) throws JBIException {
        // 1. Decide what type of message exchange needed for this operation.
        Source outSource = null;
        try {
            Operation wsdlOp = this.getWSDLOperation(operation);
            if ( wsdlOp == null ) {
                String msg = "Cannot find the wsdl operation " + operation + " on the jbi consumer endpoint " + this.getID();
                this.getLogger().fine(msg);
                throw new JBIException(msg);
            }
            QName wsdlOpQName = this.getOperationQName(wsdlOp.getName());
            if ( OperationType.REQUEST_RESPONSE.equals(wsdlOp.getStyle()) ) {
                outSource = doInOutMessageExchange(wsdlOpQName, inSource);
            } else if ( OperationType.ONE_WAY.equals(wsdlOp.getStyle()) ) {
                doInOnlyMessageExchange(wsdlOpQName, inSource);
            } else {
                throw new JBIException("Unsupported MEP " + wsdlOp.getStyle() +
                    "for operation " + operation);
            }
            return outSource;
        } catch (Exception ex) {
            ex.printStackTrace();
            throw new JBIException(ex);
        }
    }
    
    /**
     * invokes the service provider with in-only message exchange sent to delivery channel
     * by the consumer.
     * //TODO: pass message headers, attachments and other normalized message settings.
     * @param operation operation name on a service
     * @param inSource input xml document for the InOut operation
     * @throws java.lang.Exception if any error occurs in invoking the operation on the service.
     * @return output xml document as a Source object received from InOut operation of the
     * service invoked.
     */
    private void doInOnlyMessageExchange(QName operation, Source inSource)
            throws Exception {
        // 2. normalized the message.
        // 3. locate service endpoint
        // 4. create message exchange according to the Opeations MEP
        
        // get the component context and the delivery channel for preparing to send message
        ComponentContext compContext = this.getComponentContext();
        DeliveryChannel channel = this.getDeliveryChannel();
        // create INOUT Message Exchange
        InOnly inOnlyME = this.createInOnlyMessageExchange(operation);
        // set the content of the IN normalized message ( Normalize the message )
        NormalizedMessage inMsg = inOnlyME.getInMessage();
        Operation wsdlOperation = this.getWSDLOperation(inOnlyME.getOperation());
        JBIWrapperUtil.addJBIWrapperToInput(wsdlOperation, inMsg, RuntimeHelper.sourceToDOMSource(inSource));
////        System.out.println("#### CamelSE ConsumerEndpont.doInOnlyMx : In Message Content for InOnly ");
////        System.out.println(RuntimeHelper.readFromSource(inMsg.getContent()));
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
        this.getLogger().fine("Consumer:InOnly:Processing Message Exchange with status " + status);
        if (ExchangeStatus.DONE.equals(status) ) {
            this.getLogger().fine("Consumer: Completed the INONLY MessageExchange");
            return;
        } else if (ExchangeStatus.ERROR.equals(status) ) {
            // error can occur any time. so just return the error back to client.
            Exception serverSideEx = inOnlyME.getError();
            StringBuffer exMsgBuff = RuntimeHelper.getExceptionStackTrace(serverSideEx);
            throw new Exception("Consumer:INONLY Message Exchange status ERROR.\n" + exMsgBuff);
        } else {
            // any other status is error.
            throw new Exception("Consumer:INONLY Message Exchange error. status: " + status);
        }        
    }

    
    /**
     * invokes service provider with in-out message exchange sent to delivery channel
     * by the consumer
     * @param operation operation name on a service
     * @param inSource input xml document for the InOut operation
     * @throws java.lang.Exception if any error occurs in invoking the operation on the service.
     * @return output xml document as a Source object received from InOut operation of the
     * service invoked.
     */
    private Source doInOutMessageExchange(QName operation, Source inSource)
    throws Exception {
        // 2. normalized the message.
        // 3. locate service endpoint
        // 4. create message exchange according to the Opeations MEP
        
        // get the component context and the delivery channel for preparing to send message
        ComponentContext compContext = this.getComponentContext();
        DeliveryChannel channel = this.getDeliveryChannel();
        // create INOUT Message Exchange
        InOut inOutME = this.createInOutMessageExchange(operation);
        // set the content of the IN normalized message ( Normalize the message )
        NormalizedMessage inMsg = inOutME.getInMessage();
        Operation wsdlOperation = this.getWSDLOperation(inOutME.getOperation());
        JBIWrapperUtil.addJBIWrapperToInput(wsdlOperation, inMsg, RuntimeHelper.sourceToDOMSource(inSource));
        // send the message exchange and wait for response
        boolean isSent = channel.sendSync(inOutME, SEND_SYNC_TIMEOUT);
        if ( !isSent ) {
            throw new Exception("CamelSE:Timeout occured in sending the message to provider");
        }
        // check if you got a out message or fault or error
        // process the Message Exchange to get the output message and
        // complete InOut message exchange with provider
        NormalizedMessage outMsg = processInOutMessageExchangeOnConsumer(inOutME);
        Source outSource = JBIWrapperUtil.removeJBIWrapperFromOutput(wsdlOperation, outMsg);
        return outSource;
    }
    
    /**
     * takes the InOut message exchange received from sendSync call and processes it further
     * to complete InOut message exchange with provider and returns the out message or throws
     * exception in case of error or faults.
     */
    private NormalizedMessage processInOutMessageExchangeOnConsumer(InOut inoutExchange)
    throws Exception {
        // InOut MessageExchange processing on consumer side
        // 1. ACTIVE status receives a fault or out message,
        //    send the done status to complete message exchange
        //    return the fault/out message to external consumer
        // 2. can not receive DONE status
        // 3. when received ERROR status, return the error to consumer.
        
        // process the message exchange based on its state.
        ExchangeStatus status = inoutExchange.getStatus();
        this.getLogger().fine("Consumer:Processing Message Exchange with status " + status);
        
        if (ExchangeStatus.ACTIVE.equals(status) ) {
            
            Fault fault = inoutExchange.getFault();
            NormalizedMessage outMsg = inoutExchange.getOutMessage();
            
            // send done to complete message exchange.
            DeliveryChannel channel = this.getDeliveryChannel();
            inoutExchange.setStatus(ExchangeStatus.DONE);
            channel.send(inoutExchange);
            this.getLogger().fine("Consumer: Completed the INOUT MessageExchange");
            
            // process fault or out message
            if ( fault != null ) {
                // throw an exception if there is a fault message.
                Source faultContent = fault.getContent();
                StringBuffer faultContentBuff = null;
                if ( faultContent != null ) {
                    faultContentBuff = RuntimeHelper.readFromSource(faultContent);
                }
                throw new Exception("Consumer:INOUT message exchange Fault \n" +
                    faultContentBuff);
            }
            
            // return the outMessage for processing
            if ( outMsg != null ) {
                return outMsg;
            } else {
                throw new Exception("Consumer: Null Out message in INOUT message exchange. ");
            }
        } else if (ExchangeStatus.DONE.equals(status) ) {
            // can not get DONE on Consumer side in INOUT message exchange.
            throw new Exception("Consumer: Illegal status DONE on INOUT message exchange");
        } else if (ExchangeStatus.ERROR.equals(status) ) {
            // error can occur any time. so just return the error back to client.
            Exception serverSideEx = inoutExchange.getError();
            StringBuffer exMsgBuff = RuntimeHelper.getExceptionStackTrace(serverSideEx);
            throw new Exception("Consumer:INOUT Message Exchange status ERROR.\n" + exMsgBuff);
        } else {
            throw new Exception("Consumer:INOUT Message Exchange error. status: " + status);
        }
    }    
    

    
}
