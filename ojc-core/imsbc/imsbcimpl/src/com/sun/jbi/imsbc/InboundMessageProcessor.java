/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

package com.sun.jbi.imsbc;

import java.io.ByteArrayOutputStream;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Collections;

import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.component.ComponentContext;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;


import com.sun.jbi.common.qos.messaging.BaseMessagingChannel;
import com.sun.jbi.imsbc.Endpoint;
import com.sun.jbi.imsbc.extensions.IMSInput;
import com.sun.jbi.imsbc.extensions.IMSMessage;
import com.sun.jbi.imsbc.extensions.IMSOperation;
import com.sun.jbi.imsbc.ims.Channel;
import com.sun.jbi.imsbc.IMSException;

import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.internationalization.Messages;


/**
 * Processes inbound messages. Beware that replies come back the same route as outbound requests and
 * will be processed by the OutboundMessageProcessor
 *
 * @author Sun Microsystems
 */
public class InboundMessageProcessor implements Runnable, ReplyListener {

    private static final long serialVersionUID = 3256727264572813369L;

    private static final Messages mMessages = Messages.getMessages(InboundMessageProcessor.class);

    private static final Logger mLogger = Messages.getLogger(InboundMessageProcessor.class);

    /**
     * Defines the JBI identifier for an 'in' message, for use with exchange.getMessage
     */
    static final String IN_MSG = "in";

    private DocumentBuilderFactory factory;

    private DocumentBuilder builder;

    private BaseMessagingChannel mChannel;

    private ComponentContext mContext;

    private MessageExchangeFactory mMessageExchangeFactory;

    private Channel imsChannel;

    private Endpoint mEndpoint;

    private static Map mInboundExchanges = Collections.synchronizedMap(new HashMap());

    private Object mMonitor;

    private long mLastProcessedTime = System.currentTimeMillis();

    private IMSInput imsInput;

    private IMSMessage imsMessage;

    private IMSOperation imsOperation;

    private IMSNormalizer imsNormalizer;

    private IMSDenormalizer imsDenormalizer;

    public InboundMessageProcessor(ComponentContext context, Endpoint endpoint, BaseMessagingChannel dChannel,
            Channel channel) throws MessagingException {

        mContext = context;
        mEndpoint = endpoint;
        mChannel = dChannel;
        imsChannel = channel;
        imsOperation = imsChannel.getIMSOperation();
        imsInput = imsOperation.getIMSOperationInput();
        imsMessage = imsChannel.getIMSMessage();
        initialize();
    }

    private void initialize() throws MessagingException {

        mMonitor = new Object();

        try {
            factory = DocumentBuilderFactory.newInstance();
            builder = factory.newDocumentBuilder();
        } catch (Exception ex) {
				mLogger.log(Level.SEVERE, mMessages.getString("IMSBC-E00401.IMP_Init_Failed", ex.getLocalizedMessage() ));

            throw new MessagingException(
                    mMessages.getString("IMSBC-E00401.IMP_Init_Failed", new Object[] { ex }));
        }
    }

    public static Map getInboundExchanges() {
        return mInboundExchanges;
    }

	public boolean onReply(MessageExchange mep){
		// to be done
		return false;
	}

    public void run() {

        if (imsInput != null) {
			if (mLogger.isLoggable(Level.INFO)) 
				mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00402.IMP_Service_Loop_Enter", new Object[] {
						imsMessage.getIrmDestId(), mEndpoint.getServiceName().toString(), mEndpoint.getEndpointName(),
						imsOperation.getBindingOperation().getName() }));
        }
        try {
			// First, initialize normalizer and denormalizer
			if (imsNormalizer == null) {
				imsNormalizer = IMSNormalizer.getInstance();
			}

			if (imsDenormalizer == null) {
				imsDenormalizer = IMSDenormalizer.getInstance();
			}			
        }
        catch (Exception ex) {
			mLogger.log(Level.WARNING, ex.getLocalizedMessage());
        }



        String imsPayLoad = null;
        String imsMsg = null;

        do {
            try {
                // retrieve the imsPayload from the Channel
                imsPayLoad = imsChannel.receive();
            } catch (Exception ex) {
                try {
                    imsChannel.disconnect();
                } catch (IMSException me) {
					mLogger.log(Level.WARNING, mMessages.getString("IMSBC-W00403.IMP_Ims_Message_Error", new Object[] { me }));

                }
					mLogger.log(Level.WARNING, mMessages.getString("IMSBC-W00403.IMP_Ims_Message_Error", new Object[] { ex }));
            }

        } while (mMonitor != null);
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00404.IMP_Service_Loop_Exit", new Object[] {
					mEndpoint.getServiceName().toString(), mEndpoint.getEndpointName(),
					imsOperation.getBindingOperation().getName() }));
    }

    public long getRequestInvocationTime() {
        return mLastProcessedTime;
    }

    /**
     * Stop the InboundMessageProcessor thread.
     */
    public void stopReceiving() {
		if (mLogger.isLoggable(Level.INFO)) 
			mLogger.log(Level.INFO, mMessages.getString("IMSBC-R00405.IMP_Stop", new Object[] { mEndpoint.getServiceName().toString(),
					mEndpoint.getEndpointName(), imsOperation.getBindingOperation().getName() }));

        mMonitor = null;
    }

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
