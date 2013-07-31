#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * JMXBindingConsumerProxy.java
 */
package net.openesb.component.${artifactId};

import net.openesb.component.${artifactId}.common.RuntimeHelper;
import net.openesb.component.${artifactId}.common.deployment.ConsumerEndpoint;
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

/**
 * This class acts as a proxy to the external service consumer that can receive
 * messages from the external consumer using JMX interface and then initiate the
 * InOut message exchange to invoke a service provided by the component in the
 * jbi environment.
 *
 * this code shows how a synchronous send/receive messages from the delivery
 * channel can be used in InOut message exchange to complete a service
 * invocation.
 *
 * @author chikkala
 */
public class JMXBindingConsumerProxy implements JMXEndpointMBean {

    /**
     * Synchronous send timeout
     */
    private static final long SEND_SYNC_TIMEOUT = 60000;
    private ConsumerEndpoint mEndpoint;
    private JMXBindingNormalizer mNormalizer;

    /**
     * Creates a new instance of JMXEndpointMBeanImpl
     */
    public JMXBindingConsumerProxy(ConsumerEndpoint endpoint) {
        this.mEndpoint = endpoint;
        this.mNormalizer = new JMXBindingNormalizer(endpoint.getWSDL(), endpoint.getWSDLBinding());
    }
    
    public StringBuffer sendMessage(String operation, StringBuffer inputDoc) throws Exception {
        // when receive input from external service consumer,
        // 1. normalized the message, 2. send in-out message exchange. 3. denormalize out message.
        Source inMsgSource = RuntimeHelper.createDOMSource(new StringReader(inputDoc.toString()));
        QName operationQName = this.mEndpoint.getOperationQName(operation);
        Source out = initiateMessageExchange(operationQName, inMsgSource);
        StringBuffer outBuff = null;
        if (out != null) {
            // read the denormalized out message to string buffer.
            outBuff = RuntimeHelper.readFromSource(out);
        }
        // return the out message content to the external service consumer.
        return outBuff;
    }
    
    public Source initiateMessageExchange(QName operation, Source inSource) throws JBIException {
        // 1. Decide what type of message exchange needed for this operation.
        Source outSource = null;
        try {
            Operation wsdlOp = this.mEndpoint.getWSDLOperation(operation);
            if (OperationType.REQUEST_RESPONSE.equals(wsdlOp.getStyle())) {
                outSource = doInOutMessageExchange(operation, inSource);
            } else if (OperationType.ONE_WAY.equals(wsdlOp.getStyle())) {
                doInOnlyMessageExchange(operation, inSource);
            } else {
                throw new JBIException("Unsupported MEP " + wsdlOp.getStyle()
                        + "for operation " + operation);
            }
            return outSource;
        } catch (Exception ex) {
            throw new JBIException(ex);
        }
    }

    /**
     * invokes the service provider with in-only message exchange sent to
     * delivery channel by the consumer
     *
     * @param operation operation name on a service
     * @param inSource input xml document for the InOut operation
     * @throws java.lang.Exception if any error occurs in invoking the operation
     * on the service.
     * @return output xml document as a Source object received from InOut
     * operation of the service invoked.
     */
    private void doInOnlyMessageExchange(QName operation, Source inSource)
            throws Exception {
        // 2. normalized the message.
        // 3. locate service endpoint
        // 4. create message exchange according to the Opeations MEP

        // get the component context and the delivery channel for preparing to send message
        ComponentContext compContext = this.mEndpoint.getComponentContext();
        DeliveryChannel channel = this.mEndpoint.getDeliveryChannel();
        // create INOUT Message Exchange
        InOnly inOnlyME = this.mEndpoint.createInOnlyMessageExchange(operation);
        // set the content of the IN normalized message ( Normalize the message )
        NormalizedMessage inMsg = inOnlyME.getInMessage();
        Operation wsdlOperation = this.mEndpoint.getWSDLOperation(inOnlyME.getOperation());
        this.mNormalizer.normalizeInput(wsdlOperation, inMsg, RuntimeHelper.sourceToDOMSource(inSource));
        // send the message exchange and wait for response
        boolean isSent = channel.sendSync(inOnlyME, SEND_SYNC_TIMEOUT);
        if (!isSent) {
            throw new Exception("JMXBinding:Timeout occured in sending the message to provider");
        }
        // check if you got a done message or error ( done or error are only allowed in in-only)
        // process the Message Exchange to check for done or error message and
        // complete InOut message exchange with provider
        //TODO: put this below code in processInOnlyMessageExchangeOnConsumer()
        ExchangeStatus status = inOnlyME.getStatus();
        this.mEndpoint.getLogger().fine("Consumer:InOnly:Processing Message Exchange with status " + status);
        if (ExchangeStatus.DONE.equals(status)) {
            this.mEndpoint.getLogger().fine("Consumer: Completed the INONLY MessageExchange");
            return;
        } else if (ExchangeStatus.ERROR.equals(status)) {
            // error can occur any time. so just return the error back to client.
            Exception serverSideEx = inOnlyME.getError();
            StringBuffer exMsgBuff = RuntimeHelper.getExceptionStackTrace(serverSideEx);
            throw new Exception("Consumer:INONLY Message Exchange status ERROR.${symbol_escape}n" + exMsgBuff);
        } else {
            // any other status is error.
            throw new Exception("Consumer:INONLY Message Exchange error. status: " + status);
        }        
    }

    /**
     * invokes service provider with in-out message exchange sent to delivery
     * channel by the consumer
     *
     * @param operation operation name on a service
     * @param inSource input xml document for the InOut operation
     * @throws java.lang.Exception if any error occurs in invoking the operation
     * on the service.
     * @return output xml document as a Source object received from InOut
     * operation of the service invoked.
     */
    private Source doInOutMessageExchange(QName operation, Source inSource)
            throws Exception {
        // 2. normalized the message.
        // 3. locate service endpoint
        // 4. create message exchange according to the Opeations MEP

        // get the component context and the delivery channel for preparing to send message
        ComponentContext compContext = this.mEndpoint.getComponentContext();
        DeliveryChannel channel = this.mEndpoint.getDeliveryChannel();
        // create INOUT Message Exchange
        InOut inOutME = this.mEndpoint.createInOutMessageExchange(operation);
        // set the content of the IN normalized message ( Normalize the message )
        NormalizedMessage inMsg = inOutME.getInMessage();
        Operation wsdlOperation = this.mEndpoint.getWSDLOperation(inOutME.getOperation());
        this.mNormalizer.normalizeInput(wsdlOperation, inMsg, RuntimeHelper.sourceToDOMSource(inSource));
        // send the message exchange and wait for response
        boolean isSent = channel.sendSync(inOutME, SEND_SYNC_TIMEOUT);
        if (!isSent) {
            throw new Exception("JMXBinding:Timeout occured in sending the message to provider");
        }
        // check if you got a out message or fault or error
        // process the Message Exchange to get the output message and
        // complete InOut message exchange with provider
        NormalizedMessage outMsg = processInOutMessageExchangeOnConsumer(inOutME);
        Source outSource = this.mNormalizer.denormalizeOutput(wsdlOperation, outMsg);
        return outSource;
    }

    /**
     * takes the InOut message exchange received from sendSync call and
     * processes it further to complete InOut message exchange with provider and
     * returns the out message or throws exception in case of error or faults.
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
        this.mEndpoint.getLogger().fine("Consumer:Processing Message Exchange with status " + status);
        
        if (ExchangeStatus.ACTIVE.equals(status)) {
            
            Fault fault = inoutExchange.getFault();
            NormalizedMessage outMsg = inoutExchange.getOutMessage();

            // send done to complete message exchange.
            DeliveryChannel channel = this.mEndpoint.getDeliveryChannel();
            inoutExchange.setStatus(ExchangeStatus.DONE);
            channel.send(inoutExchange);
            this.mEndpoint.getLogger().fine("Consumer: Completed the INOUT MessageExchange");

            // process fault or out message
            if (fault != null) {
                // throw an exception if there is a fault message.
                Source faultContent = fault.getContent();
                StringBuffer faultContentBuff = null;
                if (faultContent != null) {
                    faultContentBuff = RuntimeHelper.readFromSource(faultContent);
                }
                throw new Exception("Consumer:INOUT message exchange Fault ${symbol_escape}n"
                        + faultContentBuff);
            }

            // return the outMessage for processing
            if (outMsg != null) {
                return outMsg;
            } else {
                throw new Exception("Consumer: Null Out message in INOUT message exchange. ");
            }
        } else if (ExchangeStatus.DONE.equals(status)) {
            // can not get DONE on Consumer side in INOUT message exchange.
            throw new Exception("Consumer: Illegal status DONE on INOUT message exchange");
        } else if (ExchangeStatus.ERROR.equals(status)) {
            // error can occur any time. so just return the error back to client.
            Exception serverSideEx = inoutExchange.getError();
            StringBuffer exMsgBuff = RuntimeHelper.getExceptionStackTrace(serverSideEx);
            throw new Exception("Consumer:INOUT Message Exchange status ERROR.${symbol_escape}n" + exMsgBuff);
        } else {
            throw new Exception("Consumer:INOUT Message Exchange error. status: " + status);
        }
    }
}
