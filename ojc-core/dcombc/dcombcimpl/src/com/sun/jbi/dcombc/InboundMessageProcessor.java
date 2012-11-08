/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc;

import java.io.ByteArrayOutputStream;
import java.io.StringWriter;

import java.util.Map;
import java.util.HashMap;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Collections;

import javax.xml.namespace.QName;

import javax.jbi.messaging.InOut;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.component.ComponentContext;
import javax.jbi.servicedesc.ServiceEndpoint;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;

import javax.transaction.xa.XAResource;
import javax.transaction.xa.XAException;
import javax.transaction.Status;

import org.w3c.dom.Document;

import com.sun.jbi.dcombc.Endpoint;
import com.sun.jbi.dcombc.DCOMException;
import com.sun.jbi.dcombc.Endpoint.EndpointMessageType;
import com.sun.jbi.dcombc.extensions.DCOMInput;
import com.sun.jbi.dcombc.extensions.DCOMOutput;
import com.sun.jbi.dcombc.extensions.DCOMMessage;
import com.sun.jbi.dcombc.extensions.DCOMOperation;
import com.sun.jbi.dcombc.dcom.Channel;
import com.sun.jbi.dcombc.util.DCOMUtil;
import com.sun.jbi.dcombc.dcom.DCOMConnInfo;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.dcombc.packaging.EndpointConfigurationFactory;

/**
 * Processes inbound messages. Beware that replies come back the same route as outbound requests and
 * will be processed by the OutboundMessageProcessor
 *
 * @author Chandrakanth Belde
 */
public class InboundMessageProcessor implements Runnable, ReplyListener {
	/** 
	 *
	 */
    private static final long serialVersionUID = 3256727264572813369L;

    private static final Messages mMessages = Messages.getMessages(InboundMessageProcessor.class);

    private static final Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);

    /**
     * Defines the JBI identifier for an 'in' message, for use with exchange.getMessage
     */
    static final String IN_MSG = "in";

    private DocumentBuilderFactory factory;

    private DocumentBuilder builder;

    private DeliveryChannel mChannel;

    private ComponentContext mContext;

    private MessageExchangeFactory mMessageExchangeFactory;

    private Channel dcomChannel;

    private Endpoint mEndpoint;

    private static Map mInboundExchanges = Collections.synchronizedMap(new HashMap());

    private Object mMonitor;

    private long mIdleTime = 0;

    private long mLastProcessedTime = System.currentTimeMillis();

    private ServiceEndpoint mServiceEndpoint = null;

    private DCOMConnInfo dcomConnInfo;
    
    private DCOMOperation dcomOperation;

    private DCOMNormalizer dcomNormalizer;

    private DCOMDenormalizer dcomDenormalizer;

    private boolean mTransacted;

    private boolean isInput;

    private String mTransaction;

    // For inout message exchanges, this map stores the reply destinations
    // keyed by the message exchange id
    private Map mMsgContexts = Collections.synchronizedMap(new HashMap());

    public InboundMessageProcessor(ComponentContext context, Endpoint endpoint, DeliveryChannel dChannel,
            Channel channel) throws MessagingException {

        mContext = context;
        mEndpoint = endpoint;
        mChannel = dChannel;
        dcomOperation = dcomChannel.getDCOMOperation();
        dcomChannel = channel;
        dcomConnInfo = dcomChannel.getDCOMConnInfo();
        initialize();
    }

    protected InboundMessageProcessor(ComponentContext context, Endpoint endpoint, DeliveryChannel dChannel,
            Channel channel, DCOMDenormalizer mDenormalizer, DCOMNormalizer mNormalizer) throws MessagingException {

        mContext = context;
        mEndpoint = endpoint;
        mChannel = dChannel;
        dcomChannel = channel;
        dcomConnInfo = dcomChannel.getDCOMConnInfo();
        dcomOperation = dcomChannel.getDCOMOperation();        
        dcomDenormalizer = mDenormalizer;
        dcomNormalizer = mNormalizer;

        initialize();

    }

    private void initialize() throws MessagingException {

        mMonitor = new Object();

        try {
            factory = DocumentBuilderFactory.newInstance();
            builder = factory.newDocumentBuilder();
        } catch (Exception ex) {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor.INIT_FAILED", new Object[] { ex.getLocalizedMessage() });

            throw new MessagingException(
                    mMessages.getString("InboundMessageProcessor.INIT_FAILED", new Object[] { ex }));
        }
    }

    public static Map getInboundExchanges() {
        return mInboundExchanges;
    }
    
    public boolean onReply(MessageExchange mep){
    	// to do
    	return false;
	}

    public void run() {

        if (dcomConnInfo != null) {
            mLogger.log(Level.INFO, "InboundMessageProcessor.SERVICE_LOOP_ENTER", new Object[] {
            		dcomConnInfo.getMethod(), mEndpoint.getServiceName().toString(), mEndpoint.getEndpointName(),
                    dcomOperation.getBindingOperation().getName() });
        }

        // First, initialize normalizer and denormalizer
        if (dcomNormalizer == null) {
            dcomNormalizer = DCOMNormalizer.getInstance();
        }

        if (dcomDenormalizer == null) {
            dcomDenormalizer = DCOMDenormalizer.getInstance();
        }

        String mPayLoad = null;
        byte[] dcomMsg = null;

        do {

            try {
                // retrieve the dcomPayload from the Channel
                mPayLoad = dcomChannel.receive();
            } catch (Exception ex) {
                try {
                    dcomChannel.close();
                } catch (DCOMException me) {
                    mLogger.log(Level.WARNING, "InboundMessageProcessor.DCOM_ERROR", new Object[] { me });

                }
                mLogger.log(Level.WARNING, "InboundMessageProcessor.DCOM_ERROR", new Object[] { ex });
            }

           // onMessage(mPayLoad);
             
        } while (mMonitor != null);

        mLogger.log(Level.INFO, "InboundMessageProcessor.SERVICE_LOOP_EXIT", new Object[] {
                mEndpoint.getServiceName().toString(), mEndpoint.getEndpointName(),
                dcomOperation.getBindingOperation().getName() });
    }

    public long getRequestInvocationTime() {
        return mLastProcessedTime;
    }

    /**
     * Stop the InboundMessageProcessor thread.
     */
    public void stopReceiving() {
        mLogger.log(Level.INFO, "InboundMessageProcessor.STOP", new Object[] { mEndpoint.getServiceName().toString(),
                mEndpoint.getEndpointName(), dcomOperation.getBindingOperation().getName() });

        mMonitor = null;
    }
    

    /**
     * 
     * @param exchangeId
     * @param msgSrc
     */
    private void logNormalizedMessage(String exchangeId, Source msgSrc) {
        try {
            TransformerFactory tFactory = TransformerFactory.newInstance();
            Transformer trans = tFactory.newTransformer();
            trans.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            trans.setOutputProperty(OutputKeys.INDENT, "yes");
            trans.setOutputProperty(OutputKeys.METHOD, "xml");
            trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
            StringWriter out = new StringWriter();
            StreamResult result = new StreamResult(out);
            trans.transform(msgSrc, result);
            out.flush();
            out.close();
            mLogger.log(Level.FINE, "InboundMessageProcessor.SEND_TO_NMR", new Object[] { exchangeId, out.toString() });
        } catch (Throwable t) {
            ;
            ;
        }
    }
    
    /**
     * 
     * @param serviceName
     * @param endpointName
     * @return
     */
    private boolean locateServiceEndpoint(QName serviceName, String endpointName) {
        if (mServiceEndpoint == null) {
            if (EndpointConfigurationFactory.isJBIRoutingEnabled()) {
                mServiceEndpoint = mContext.getEndpoint(serviceName, endpointName);
            } else {
                ServiceEndpoint[] allServiceEndpoints = mContext.getEndpointsForService(serviceName);
                if (allServiceEndpoints != null) {
                    int epCount = 0;
                    while (mServiceEndpoint == null && epCount < allServiceEndpoints.length) {
                        if (allServiceEndpoints[epCount].getEndpointName().equals(endpointName)) {
                            // Found our endpoint
                            mServiceEndpoint = allServiceEndpoints[epCount];
                        }
                        epCount++;
                    }
                }
            }
        }

        return (mServiceEndpoint != null);
    }

    /**
     * 
     * @param mepType
     * @return
     * @throws DCOMException
     */
    private MessageExchange createMessageExchange(String mepType) throws DCOMException {
        MessageExchange msgEx = null;

        if (mMessageExchangeFactory == null) {
            mMessageExchangeFactory = mChannel.createExchangeFactory();
        }

        try {
            if (mepType.equals(EndpointMessageType.IN_ONLY)) {
                msgEx = mMessageExchangeFactory.createInOnlyExchange();
            } else if (mepType.equals(EndpointMessageType.IN_OUT)) {
                msgEx = mMessageExchangeFactory.createInOutExchange();
            } else {
                mLogger.log(Level.SEVERE, "InboundMessageProcessor.MXCH_CREATE_UNSUPPORTED_MEP",
                        new Object[] { mepType });

                throw new DCOMException(mMessages.getString("InboundMessageProcessor.MXCH_CREATE_UNSUPPORTED_MEP",
                        new Object[] { mepType }));
            }
        } catch (MessagingException ex) {
            mLogger.log(Level.SEVERE, "InboundMessageProcessor.MXCH_CREATE_FAILED", new Object[] { mepType, ex });

            throw new DCOMException(mMessages.getString("InboundMessageProcessor.MXCH_CREATE_FAILED", new Object[] {
                    mepType, ex }));
        }

        return msgEx;
    }
    
    /**
     * 
     * @param msg
     * @return
     * @throws TransformerException
     */
    private String writeMessage(NormalizedMessage msg) throws TransformerException {
		String s = null;
		try {
		if (msg.getContent() != null) {
			ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
			TransformerFactory tFactory = TransformerFactory.newInstance();
			Transformer trans = tFactory.newTransformer();
			Source source = msg.getContent();
			StreamResult result = new StreamResult(byteArrayOutputStream);
			trans.transform(source, result);
			s = byteArrayOutputStream.toString();
		}
		} catch (Exception ex) {
			;;
		}
			return s;
	}   

}
