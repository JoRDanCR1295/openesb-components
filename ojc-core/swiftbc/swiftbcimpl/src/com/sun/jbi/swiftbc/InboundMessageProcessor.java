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

/*
 * @(#)InboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.io.StringWriter;
import java.util.concurrent.Callable;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.stream.StreamResult;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import com.sun.jbi.swiftbc.extensions.SwiftInput;
import com.sun.jbi.swiftbc.extensions.SwiftOperation;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import com.sun.jbi.swiftbc.extservice.server.SwiftCallback;

import com.sun.jbi.swiftbc.extservice.SwiftInboundMessageValidationProcessor;
import com.sun.jbi.internationalization.Messages;
import com.sun.encoder.Encoder;

/**
 * Processes inbound messages
 */
public class InboundMessageProcessor extends SAGConstants implements
		Callable<String>, MessageExchangeReplyListener
{

	private static final Messages mMessages = Messages
			.getMessages(InboundMessageProcessor.class);

	private static final Logger mLog = Messages
			.getLogger(InboundMessageProcessor.class);

	private Endpoint mEndpoint;

	private ComponentContext mContext;

	private DeliveryChannel mChannel;

	private RuntimeConfiguration mRuntimeConfig;

	private SwiftCallback mSwiftCallback;

	private String mSwiftVersion;

	private String mSwiftMsg;

	private MessageExchangeFactory mMessageExchangeFactory;

	private SwiftNormalizer mSwiftNormalizer;

	private SwiftDenormalizer mSwiftDenormalizer;

	private QName mOperationName;

	private String mMsgExchangePattern;

	private SwiftOperation mSwiftOperation;

	private String mAckMode;

	private Encoder mEncoder;

	private SwiftInboundMessageValidationProcessor mSwiftInbMsgValidationprocessor;

	// used by receive channels for saving message exchanges for inbound
	// request/reply exchanges
	private Map<String, InboundReplyContext> mInboundExchanges;

	private Map<String, SwiftCallback> mSessionMap;

	public InboundMessageProcessor()
	{
	}

	protected void initialize() throws Exception
	{
		mSwiftNormalizer = SwiftNormalizer.getInstance();
		mSwiftDenormalizer = SwiftDenormalizer.getInstance();
		mSwiftOperation = (SwiftOperation) mEndpoint.getSwiftOperations().get(
				mOperationName);
		mMsgExchangePattern = (String) mEndpoint
				.getOperationMsgExchangePattern().get(mOperationName);
		mSwiftVersion = mEndpoint.getSwiftProtocolProperties().getVersionID();
	}

	protected void setComponentContext(ComponentContext compContext)
	{
		mContext = compContext;
	}

	protected void setDeliveryChannel(DeliveryChannel deliveryChannel)
	{
		mChannel = deliveryChannel;
	}

	protected void setRuntimeConfiguration(RuntimeConfiguration runtimeConfig)
	{
		mRuntimeConfig = runtimeConfig;
	}

	protected void setEndpoint(Endpoint endpoint)
	{
		mEndpoint = endpoint;
	}

	protected void setOperationName(QName operName)
	{
		mOperationName = operName;
	}

	protected void setMessage(String SwiftMsg)
	{
		mSwiftMsg = SwiftMsg;
	}

	protected void setCallback(SwiftCallback SwiftCallback)
	{
		mSwiftCallback = SwiftCallback;
	}

	protected void setSwiftEncoder(Encoder encoder)
	{
		this.mEncoder = encoder;
	}

	protected void setSessionMap(Map<String, SwiftCallback> sessionMap)
	{
		this.mSessionMap = sessionMap;
	}

	protected void setInboundMsgExchanges(
			Map<String, InboundReplyContext> inboundMsgExchanges)
	{
		mInboundExchanges = inboundMsgExchanges;
	}

	public String call() throws Exception
	{
		processInboundRequest();

		return "SUCCESS";
	}

	/**
	 * Process Inbound Requests
	 * 
	 * @throws Exception
	 */
	private void processInboundRequest() throws Exception
	{
		String msgXchangeID = null;
		String key = null;
		try
		{
			mLog.log(Level.INFO, "InboundMessageProcessor_SwiftMSG_RECV");
			long SwiftMsgReceiveTime = System.currentTimeMillis();
			MessageExchange msgXchange = createMessageExchange(mMsgExchangePattern);
			msgXchangeID = msgXchange.getExchangeId();
			SwiftInput SwiftInput = mEndpoint
					.getSwiftOperationInput(mSwiftOperation);
			SwiftMessage SwiftMessage = SwiftInput.getSwiftMessage();
			NormalizedMessage normalizedMsg = null;
			try
			{
				normalizedMsg = mSwiftNormalizer.normalize(msgXchange,
						mOperationName, mEndpoint, SwiftMessage, mSwiftMsg);
			} catch (com.sun.encoder.EncoderException encExe)
			{
				// TODO: returning the NAK for unsucessful operation of encoding
				// activity. ack code
				// - AE
				// Freeze further processing since message validation against
				// xml schema failed
				return;
			}
			Source inContentSrc = normalizedMsg.getContent();
			// TODO process nodes
			
			mSwiftInbMsgValidationprocessor = new SwiftInboundMessageValidationProcessor();

			if (msgXchange instanceof InOnly)
			{
				((InOnly) msgXchange).setInMessage(normalizedMsg);
			}
			if (mLog.isLoggable(Level.INFO))
			{
				logNormalizedMessage(msgXchangeID, inContentSrc);
			}
			InboundReplyContext inReplyContext = null;

			inReplyContext = new InboundReplyContext(SwiftMsgReceiveTime,
					normalizedMsg, this, mSwiftCallback);

			mInboundExchanges.put(msgXchangeID, inReplyContext);
			sendMessageToNMR(msgXchange);
		} catch (Exception ex)
		{
			mInboundExchanges.remove(msgXchangeID);
			if (mSessionMap.containsKey(key))
			{
				mSessionMap.remove(key);
			}
			mLog.log(Level.SEVERE,
					"InboundMessageProcessor_PROCESS_INBOUND_REQUEST_FAILED",
					new Object[]
					{ ex.getLocalizedMessage() });
			throw ex;
		}
	}

	/**
	 * Called by the service engine after done with processing the in-only
	 * message exchange
	 * 
	 * @param msgXChange
	 *            Message Exchange
	 * @param processReplySuccess
	 *            boolean value true means succesful processing of the message
	 *            exchange. False indicates unsuccesful processing
	 */
	public void onReply(MessageExchange msgXChange, boolean processReplySuccess)
			throws ApplicationException
	{
	}

	/**
	 * Logs the normalized message
	 * 
	 * @param exchangeId
	 *            message exchange Id
	 * @param msgSrc
	 *            javax.xml.transform.Source
	 */
	private void logNormalizedMessage(String exchangeId, Source msgSrc)
	{
		if (mLog.isLoggable(Level.INFO))
		{
			StringWriter out = null;
			if (msgSrc != null)
			{
				try
				{
					TransformerFactory tFactory = TransformerFactory
							.newInstance();
					Transformer trans = tFactory.newTransformer();
					trans.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
					trans.setOutputProperty(OutputKeys.INDENT, "yes");
					trans.setOutputProperty(OutputKeys.METHOD, "xml");
					trans.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION,
							"yes");
					out = new StringWriter();
					StreamResult result = new StreamResult(out);
					trans.transform(msgSrc, result);
					out.flush();
					out.close();
				} catch (Throwable t)
				{
					;
					;
				}
			}

			if (mLog.isLoggable(Level.INFO))
			{
				mLog
						.log(
								Level.INFO,
								"InboundMessageProcessor_NORMALIZED_MESSAGE_CONTENT_DUMP",
								new Object[]
								{ exchangeId,
										(out == null ? "null" : out.toString()) });
			}
		}
	}

	/**
	 * Send a MessageExchange to the NMR
	 */
	private void sendMessageToNMR(MessageExchange msgXchange) throws Exception
	{

		ServiceEndpoint se = mContext.getEndpoint(mEndpoint.getServiceName(),
				mEndpoint.getEndpointName());

		if (se == null)
		{
			// Failed to locate provider endpoint
			mLog
					.log(Level.SEVERE,
							"InboundMessageProcessor_ENDPOINT_NOTFOUND",
							new Object[]
							{ mEndpoint.getServiceName(),
									mEndpoint.getEndpointName() });
			String errMsg = mMessages
					.getString("InboundMessageProcessor_ENDPOINT_NOTFOUND",
							new Object[]
							{ mEndpoint.getServiceName(),
									mEndpoint.getEndpointName() });
			throw new MessagingException(errMsg);
		}
		msgXchange.setEndpoint(se);
		msgXchange.setOperation(mOperationName);

		if (mLog.isLoggable(Level.INFO))
		{
			mLog.log(Level.INFO,
					"InboundMessageProcessor_SENDING_MESSAGE_EXCHANGE",
					new Object[]
					{
							msgXchange.getExchangeId(),
							se.getServiceName().toString()
									+ se.getEndpointName(),
							mOperationName.toString() });

		}

		mChannel.send(msgXchange);
	}

	/**
	 * Given the message exchange pattern create Message exchange and returns
	 * 
	 * @param mepType
	 *            message exchange pattern type
	 * @return MessageExchange
	 * @throws Exception
	 */
	private MessageExchange createMessageExchange(String mepType)
			throws Exception
	{
		MessageExchange msgEx = null;
		// Create the MessageExchangeFactory
		if (mMessageExchangeFactory == null)
		{
			mMessageExchangeFactory = mChannel.createExchangeFactory();
		}
		try
		{
			// in case of enhanced acknowledgement mode create InOnly
			// MessageExchange
			if (mepType.equals(Endpoint.EndpointMessageType.IN_ONLY))
			{
				msgEx = mMessageExchangeFactory.createInOnlyExchange();
			} else if (mepType.equals(Endpoint.EndpointMessageType.IN_OUT))
			{
				msgEx = mMessageExchangeFactory.createInOutExchange();
			} else
			{
				mLog.log(Level.SEVERE,
						"InboundMessageProcessor_MXCH_CREATE_UNSUPPORTED_MEP",
						new Object[]
						{ mepType });
				throw new Exception(
						mMessages
								.getString(
										"InboundMessageProcessorListenerEndpoint_MXCH_CREATE_UNSUPPORTED_MEP",
										new Object[]
										{ mepType }));
			}
		} catch (MessagingException ex)
		{
			mLog.log(Level.SEVERE,
					"InboundMessageProcessor_MXCH_CREATE_FAILED", new Object[]
					{ mepType, ex });

			throw new Exception(
					mMessages
							.getString(
									"InboundMessageProcessorListenerEndpoint_MXCH_CREATE_FAILED",
									new Object[]
									{ mepType, ex }));
		}
		return msgEx;
	}

}
