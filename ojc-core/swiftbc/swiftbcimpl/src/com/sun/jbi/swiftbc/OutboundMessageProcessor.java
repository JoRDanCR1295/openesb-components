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
 * @(#)OutboundMessageProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc;

import static com.sun.jbi.swiftbc.extservice.client.SwiftConnectorFactory.createSwiftConnector;

import java.io.StringWriter;
import java.net.URI;
import java.net.URL;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.naming.InitialContext;
import javax.xml.namespace.QName;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.exchange.ExchangePattern;
import com.sun.jbi.swiftbc.Endpoint.EndpointState;
import com.sun.jbi.swiftbc.extensions.SwiftAddress;
import com.sun.jbi.swiftbc.extensions.SwiftInput;
import com.sun.jbi.swiftbc.extensions.SwiftOutput;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import com.sun.jbi.swiftbc.extensions.SwiftOperation;
import com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties;
import com.sun.jbi.swiftbc.extservice.ProtocolInfo;
import com.sun.jbi.swiftbc.extservice.SwiftOutboundMessageValidationProcessor;
import com.sun.jbi.swiftbc.extservice.client.SwiftConnector;
import com.sun.jbi.swiftbc.extservice.client.SwiftConnectorFactory.TransportProtocolType;

/**
 * Process replies/requests received from the SE.
 * 
 * @author S. Nageswara Rao
 */
public class OutboundMessageProcessor extends SAGConstants
{

	private static final Messages mMessages = Messages
			.getMessages(OutboundMessageProcessor.class);

	private static final Logger mLog = Messages
			.getLogger(OutboundMessageProcessor.class);

	private Collection mServiceUnits;

	private DeliveryChannel mChannel;

	private Map<String, InboundReplyContext> mInboundExchanges;

	private RuntimeConfiguration mRuntimeConfig;

	private SwiftDenormalizer mSwiftDenormalizer;

	private SwiftNormalizer mSwiftNormalizer;

	private ComponentContext mComponentContext;

	private Transformer mTrans;

	private SwiftOutboundMessageValidationProcessor mSwiftOutboundMsgValidationprocessor;

	public OutboundMessageProcessor(ComponentContext compContext,
			DeliveryChannel chnl, Collection serviceUnits,
			Map<String, InboundReplyContext> inboundMessageExchanges,
			RuntimeConfiguration runtimeConfig) throws Exception
	{
		mComponentContext = compContext;
		mChannel = chnl;
		mServiceUnits = serviceUnits;
		mInboundExchanges = inboundMessageExchanges;
		mSwiftNormalizer = SwiftNormalizer.getInstance();
		mSwiftDenormalizer = SwiftDenormalizer.getInstance();
		mRuntimeConfig = runtimeConfig;
		try
		{
			TransformerFactory factory = TransformerFactory.newInstance();
			mTrans = factory.newTransformer();
		} catch (TransformerFactoryConfigurationError ex)
		{
			throw new Exception(mMessages.getString(
					"InboundMessageProcessor_Create_transformer_exception", ex
							.getMessage()), ex);
		}
		// get the Initial Context
		InitialContext ic = mComponentContext.getNamingContext();
		// get the properties
		Properties props = mRuntimeConfig.getProperties();
	}

	/**
	 * Process the message exchange
	 */
	public void processMessage(MessageExchange msgExchange)
	{
		final String exchangeId = msgExchange.getExchangeId();
		boolean inbound = msgExchange.getRole().equals(
				MessageExchange.Role.CONSUMER);
		// capture request/reply time here
		long receivedMsgXchangeTime = System.currentTimeMillis();
		InboundReplyContext inbReplyContext = null;
		if (inbound)
		{
			inbReplyContext = mInboundExchanges.get(exchangeId);
			long invocationTime = inbReplyContext.getRequestInvocationTime();
			if (mLog.isLoggable(Level.INFO))
			{
				long difference = receivedMsgXchangeTime - invocationTime;
				mLog.log(Level.INFO,
						"OutboundMessageProcessor_MXCH_RESPONSE_RECVD",
						new Object[]
						{ exchangeId, new Long(difference) });
			}
		}
		Endpoint endpoint = findEndpoint(msgExchange);
		if (endpoint == null)
		{
			mLog.log(Level.SEVERE,
					"OutboundMessageProcessor_ENDPOINT_UNDEFINED", new Object[]
					{ msgExchange.getEndpoint().toString(),
							msgExchange.getExchangeId() });

			setErrorStatusAndSend(msgExchange);

			return;
		}

		mSwiftOutboundMsgValidationprocessor = new SwiftOutboundMessageValidationProcessor();
		int state = endpoint.getState();
		if (!(state == EndpointState.RUNNING))
		{
			String strState = (state == EndpointState.STOPPED ? "STOPPED"
					: "SHUTDOWN");

			// If the endpoint is not in the RUNNING state
			// (i.e. is stopped or shutdown), ignore the message
			mLog.log(Level.SEVERE,
					"OutboundMessageProcessor_ENDPOINT_NON_RUNNING_STATE",
					new Object[]
					{ endpoint.getEndpointName(), strState,
							msgExchange.getExchangeId() });

			setErrorStatusAndSend(msgExchange);
		} else
		{
			URI pattern = msgExchange.getPattern();
			if (mLog.isLoggable(Level.INFO))
			{
				mLog.log(Level.INFO, "OutboundMessageProcessor_MXCH_PATTERN",
						new Object[]
						{ msgExchange.getExchangeId(), pattern.toString() });
			}
                        switch (ExchangePattern.valueOf(msgExchange))
                            {
                            case IN_ONLY:
                                if (inbound)
                                    {
                                        processOneWayInbound((InOnly) msgExchange, endpoint,
                                                             inbReplyContext);
                                    } else
                                    {
                                        processOneWayOutbound((InOnly) msgExchange, endpoint);
                                    }
                                break;
                            case IN_OUT:
                                if (inbound) {
                                    // should call processInOutInbound()
                                } else {
                                    processInOutOutbound((InOut)msgExchange, endpoint);
                                }
                                break;
                            case ROBUST_IN_ONLY:
                                mLog.log(
                                         Level.WARNING,
                                         "OutboundMessageProcessor_MXCH_PATTERN_UNSUPPORTED",
                                         new Object[]
                                         { "robust in-only",
                                           msgExchange.getExchangeId() });
                                break;
                            case IN_OPTIONAL_OUT:
                                mLog.log(
                                         Level.WARNING,
                                         "OutboundMessageProcessor_MXCH_PATTERN_UNSUPPORTED",
                                         new Object[]
                                         { "robust out-only",
                                           msgExchange.getExchangeId() });
                                break;
                            default:
                                mLog.log(Level.SEVERE,
                                         "OutboundMessageProcessor_MXCH_PATTERN_INVALID",
                                         new Object[]
                                         { msgExchange.getExchangeId() });
                                return;
                            }
                }
        }

	private void processOneWayOutbound(InOnly inonly, Endpoint destination)
	{
		try
		{
			// Increment received requests on endpoint (bc is provider)
			destination.getEndpointStatus().incrementReceivedRequests();
			// Get the Normalized Message from MessageExchange
			NormalizedMessage normalizedMsg = inonly.getInMessage();
			QName operationName = inonly.getOperation();
			Source src = normalizedMsg.getContent();
			// In case of acknowledgments, message validation should not be done
			SwiftOperation SwiftOperation = locateSwiftOperation(operationName,
					destination);
			SwiftInput SwiftInput = SwiftOperation.getSwiftOperationInput();
			SwiftMessage SwiftMessage = SwiftInput.getSwiftMessage();
			if (mLog.isLoggable(Level.INFO))
			{
				mLog.log(Level.INFO,
						"OutboundMessageProcessor_BEGIN_DENORMALIZE_MESSAGE");
			}
			// Denormalize from NMS message to Swift request/acknowledgment
			// message
			String SwiftPayLoad = mSwiftDenormalizer.denormalize(normalizedMsg,
					operationName, destination, SwiftMessage);
			if (mLog.isLoggable(Level.INFO))
			{
				mLog.log(Level.INFO,
						"OutboundMessageProcessor_END_DENORMALIZE_MESSAGE");
				mLog
						.log(Level.INFO,
								"OutboundMessageProcessor_SENDING_SwiftMSG_TO_SwiftExternalSystem");
			}
			SwiftConnector swiftConnector = getConnector(destination);
			if (swiftConnector == null)
			{
				mLog.log(Level.SEVERE,
						"OutboundMessageProcessor_CONNECTOR_CREATION_FAILED");
				throw new ApplicationException(
						mMessages
								.getString("OutboundMessageProcessor_CONNECTOR_CREATION_FAILED"));
			}
			ProtocolInfo protocolInfo = getProtocolInfo(destination);
			inonly.setStatus(ExchangeStatus.DONE);
			if (mLog.isLoggable(Level.INFO))
			{
				mLog.log(Level.INFO,
						"OutboundMessageProcessor_MXCH_SET_STATUS",
						new Object[]
						{ "DONE", inonly.getExchangeId() });
			}
			destination.getEndpointStatus().incrementSentDones();
		} catch (Exception exc)
		{
			destination.getEndpointStatus().incrementSentErrors();
			mLog.log(Level.SEVERE, "OutboundMessageProcessor_MXCH_ERROR",
					new Object[]
					{ inonly.getExchangeId(), exc });
			try
			{
				inonly.setStatus(ExchangeStatus.ERROR);

				if (mLog.isLoggable(Level.INFO))
				{
					mLog.log(Level.INFO,
							"OutboundMessageProcessor_MXCH_SET_STATUS",
							new Object[]
							{ "ERROR", inonly.getExchangeId() });
				}

			} catch (MessagingException ex)
			{
				mLog.log(Level.WARNING,
						"OutboundMessageProcessor_MXCH_SET_STATUS_ERROR",
						new Object[]
						{ "in-only", inonly.getExchangeId(), ex });
			}
		} finally
		{
			try
			{
				// Send back exchange status
				mChannel.send(inonly);
			} catch (Exception mex)
			{
				mLog.log(Level.SEVERE,
						"OutboundMessageProcessor_NMR_SEND_ERROR", new Object[]
						{ inonly.getExchangeId(), mex });
			}
		}
	}

	private void processInOutOutbound(InOut inout, Endpoint destination)
	{
            try	{
        
                if (inout.getStatus() == ExchangeStatus.DONE) {
                    destination.getEndpointStatus().incrementReceivedDones();
                } else if (inout.getStatus() == ExchangeStatus.ERROR) {
                    destination.getEndpointStatus().incrementReceivedErrors();
                } else {
                // Increment received requests on endpoint (bc is provider)
                destination.getEndpointStatus().incrementReceivedRequests();
                
                // Get the Normalized Message from MessageExchange
                NormalizedMessage normalizedMsg = inout.getInMessage();
                QName operationName = inout.getOperation();
                Source src = normalizedMsg.getContent();
		
                // In case of acknowledgments, message validation should not be done
                SwiftOperation SwiftOperation =
                    locateSwiftOperation(operationName, destination);
                SwiftInput SwiftInput = SwiftOperation.getSwiftOperationInput();
                SwiftMessage SwiftMessage = SwiftInput.getSwiftMessage();
                if (mLog.isLoggable(Level.INFO)) {
                    mLog.log(Level.INFO,
                             "OutboundMessageProcessor_BEGIN_DENORMALIZE_MESSAGE");
                }

                // Denormalize from NMR message to Swift request/acknowledgment
                // message
                String SwiftPayLoad =
                    mSwiftDenormalizer.denormalize(normalizedMsg,
                                                   operationName, destination, SwiftMessage);
                SwiftPayLoad = SwiftPayLoad.trim();

                if (mLog.isLoggable(Level.INFO)) {
                    mLog.log(Level.INFO,
                             "OutboundMessageProcessor_END_DENORMALIZE_MESSAGE");
                    mLog.log(Level.INFO,
                             "OutboundMessageProcessor_SENDING_SwiftMSG_TO_SwiftExternalSystem");
                }
                
                SwiftConnector swiftConnector = getConnector(destination);
                if (swiftConnector == null)  {
                    mLog.log(Level.SEVERE,
                             "OutboundMessageProcessor_CONNECTOR_CREATION_FAILED");
                    throw new ApplicationException(mMessages
                                                   .getString("OutboundMessageProcessor_CONNECTOR_CREATION_FAILED"));
                }
			
                ProtocolInfo protocolInfo = getProtocolInfo(destination);

                // Connect and get a response synchronously for now
                swiftConnector.connect(protocolInfo);
                String swiftResponse = swiftConnector.sendReceiveSwiftMessage(SwiftPayLoad);
                swiftConnector.disconnect();
                mLog.info("Got a SWIFT response: " + swiftResponse);
                
                SwiftOutput swiftOutput = SwiftOperation.getSwiftOperationOutput();
                SwiftMessage swiftOutputMessage = swiftOutput.getSwiftMessage();
                NormalizedMessage outMsg = mSwiftNormalizer.normalize(inout,
                                                                      operationName,
                                                                      destination,
                                                                      swiftOutputMessage,
                                                                      swiftResponse);
                inout.setOutMessage(outMsg);

                if (mLog.isLoggable(Level.INFO)) {
                    mLog.log(Level.INFO,
                             "OutboundMessageProcessor_MXCH_SET_STATUS",
                             new Object[]
                             { "DONE", inout.getExchangeId() });
                }
                destination.getEndpointStatus().incrementSentDones();
                }
            } catch (Exception exc) {
                destination.getEndpointStatus().incrementSentErrors();
                mLog.log(Level.SEVERE, "OutboundMessageProcessor_MXCH_ERROR",
                         new Object[]
                    { inout.getExchangeId(), exc });
                try {
                    inout.setError(exc);
                    inout.setStatus(ExchangeStatus.ERROR);
                    if (mLog.isLoggable(Level.INFO)) {
                        mLog.log(Level.INFO,
                                 "OutboundMessageProcessor_MXCH_SET_STATUS",
                                 new Object[]
                                 { "ERROR", inout.getExchangeId() });
                    }
                } catch (MessagingException ex) {
                    mLog.log(Level.WARNING,
                             "OutboundMessageProcessor_MXCH_SET_STATUS_ERROR",
                             new Object[]
                             { "in-only", inout.getExchangeId(), ex });
                }
            } catch (Error error) {
                System.out.println(error.getMessage());
            } finally {
                try {
                    // Send back exchange status
                    mChannel.send(inout);
                } catch (Exception mex) {
                    mLog.log(Level.SEVERE,
                             "OutboundMessageProcessor_NMR_SEND_ERROR", new Object[]
                             { inout.getExchangeId(), mex });
                }
            }
	}

	private void processOneWayInbound(InOnly inonly, Endpoint endpoint,
			InboundReplyContext inbReplyContext)
	{
		/*
		 * if (inonly.getStatus().equals(ExchangeStatus.DONE)) {
		 * endpoint.getEndpointStatus().incrementReceivedDones(); String ackMode =
		 * endpoint.getSwiftProtocolProperties().getAckMode(); try { if
		 * ((ackMode.equals(ACK_MODE_ORIGINAL)) ||
		 * (ackMode.equals(ACK_MODE_ENHANCED) &&
		 * inbReplyContext.getAcceptAckCondition() != null &&
		 * (inbReplyContext.getAcceptAckCondition().equals( ALWAYS_CONDITION) ||
		 * inbReplyContext.getAcceptAckCondition().equals(SUCCESS_CONDITION)))) {
		 * MessageExchangeReplyListener listener =
		 * inbReplyContext.getMessageExchangeReplyListener();
		 * listener.onReply(inonly, true); } } catch (Exception ex) {
		 * mLog.log(Level.SEVERE, "OutboundMessageProcessor_ON_REPLY_FAILED",
		 * new Object[] { inonly.getExchangeId(), ex.getLocalizedMessage() }); } }
		 * else if (inonly.getStatus().equals(ExchangeStatus.ERROR)) {
		 * endpoint.getEndpointStatus().incrementReceivedErrors(); try {
		 * MessageExchangeReplyListener listener =
		 * inbReplyContext.getMessageExchangeReplyListener();
		 * listener.onReply(inonly, false); } catch (Exception ex) {
		 * mLog.log(Level.SEVERE, "OutboundMessageProcessor_ON_REPLY_FAILED",
		 * new Object[] { inonly.getExchangeId(), ex.getLocalizedMessage() }); } }
		 */
	}

	/**
	 * Find the exact Endpoint
	 * 
	 * @param msgExchange
	 *            the received Message Exchange
	 * @return Endpoint a Service Endpoint
	 */
	private Endpoint findEndpoint(MessageExchange msgExchange)
	{
		Endpoint endpoint = null;
		for (Iterator it = mServiceUnits.iterator(); it.hasNext();)
		{
			for (Iterator it2 = ((ServiceUnit) it.next()).getEndpoints()
					.iterator(); it2.hasNext();)
			{
				Endpoint aEndPoint = (Endpoint) it2.next();
				QName serviceName = msgExchange.getEndpoint().getServiceName();
				String endpointName = msgExchange.getEndpoint()
						.getEndpointName();

				if (aEndPoint.getServiceName().equals(serviceName)
						&& aEndPoint.getEndpointName().equals(endpointName))
				{
					endpoint = aEndPoint;
				}
			}
		}

		return endpoint;
	}

	private void setErrorStatusAndSend(MessageExchange msg)
	{
		try
		{
			msg.setStatus(ExchangeStatus.ERROR);

			if (mLog.isLoggable(Level.INFO))
			{
				mLog.log(Level.INFO,
						"OutboundMessageProcessor_MXCH_SET_STATUS",
						new Object[]
						{ "ERROR", msg.getExchangeId() });
			}

		} catch (MessagingException ex)
		{
			mLog.log(Level.WARNING,
					"OutboundMessageProcessor_MXCH_SET_STATUS_ERROR",
					new Object[]
					{ msg.getExchangeId(), ex });
		}

		try
		{
			// Send back exchange status
			mChannel.send(msg);
		} catch (MessagingException mex)
		{
			mLog.log(Level.WARNING,
					"OutboundMessageProcessor_NMR_SEND_STATUS_ERROR", msg
							.getExchangeId());
		}

	}

	private SwiftOperation locateSwiftOperation(QName opname, Endpoint endpoint)
	{
		return (SwiftOperation) endpoint.getSwiftOperations().get(opname);
	}

	private void logNormalizedMessage(String exchangeId, Source msgSrc)
	{
		try
		{
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
			if (mLog.isLoggable(Level.INFO))
			{
				mLog.log(Level.INFO, "InboundMessageProcessor_SEND_TO_NMR",
						new Object[]
						{ exchangeId, out.toString() });
			}
		} catch (Throwable t)
		{
			;
			;
		}
	}

	private Document getDocument(Source src) throws Exception
	{
		DOMResult result = null;// transformToDOMResult(mTrans, src);
		Node node = result.getNode();
		Document normalizedDoc = null;
		if (node instanceof Document)
		{
			normalizedDoc = (Document) node;
		} else
		{
			normalizedDoc = ((Element) node).getOwnerDocument();
		}
		return normalizedDoc;
	}


	/**
	 * Check whether node has the specified element
	 * 
	 * @param Node
	 *            org.w3c.dom.Node
	 * @param val
	 *            String
	 */
	private Node hasElement(Node node, String val)
	{
		NodeList list = node.getChildNodes();
		Node returnNode = null;
		for (int i = 0; i < list.getLength(); i++)
		{
			Node child = list.item(i);
			if (child.getLocalName().equals(val))
			{
				returnNode = child;
				break;
			}
		}
		return returnNode;

	}

	/**
	 * @param endpoint
	 *            A service end point.
	 * @return SwiftConnector an Swift Connector
	 * @throws Exception
	 *             any exception that creeps when creating a connector to the
	 *             Swift System
	 */
	private SwiftConnector getConnector(Endpoint endpoint) throws Exception
	{
		SwiftConnector swiftConnector = null;
		String transportProtocolName = endpoint.getSwiftAddress()
				.getTransportProtocolName();
		// right now only TCPIP transport protocol is supported
                /*
                swiftConnector = new SwiftConnector() {
                        public void connect(ProtocolInfo protocolInfo) 
                            throws Exception {
                            return;
                        }

                        public void sendSwiftMessage(String swiftMsg) 
                            throws Exception {
                            return;
                        }

                        public String recvSwiftMessage() throws Exception {
                            return null;
                        }

                        public String sendReceiveSwiftMessage(String SwiftMsg)
                            throws Exception {
                            return "<SwInt:ExchangeResponse xmlns:Sw=\"urn:swift:snl:ns.Sw\" xmlns:SwGbl=\"urn:swift:snl:ns.SwGbl\" xmlns:SwSec=\"urn:swift:snl:ns.SwSec\" xmlns:SwInt=\"urn:swift:snl:ns.SwInt\"><SwInt:SwiftRequestRef>SNL00514-2007-04-26T23:49:05.17537.006752Z</SwInt:SwiftRequestRef><SwGbl:Status>" +
"        <SwGbl:StatusAttributes>" +
"            <SwGbl:Severity>Transient</SwGbl:Severity>" +
"            <SwGbl:Code>Sw.Gbl.SnlServerAppFailure</SwGbl:Code>" +
"            <SwGbl:Parameter>256</SwGbl:Parameter>" +
"            <SwGbl:Parameter>Failure processing response from server application.</SwGbl:Parameter>" +
"            <SwGbl:Text>Error occurred at SNL server process.</SwGbl:Text>" +
"            <SwGbl:Details>" +
"                <SwGbl:Code>Sw.Wfa.SnlServerAppFailure</SwGbl:Code>" +
"                <SwGbl:Text>Error occurred while processing the request / response on SNL server., Failure processing response from server application.</SwGbl:Text>" +
"                <SwGbl:Action>To fix the error, check the server log.</SwGbl:Action>" +
"            </SwGbl:Details>" +
"        </SwGbl:StatusAttributes>" +
"    </SwGbl:Status></SwInt:ExchangeResponse>";
                        }

                        public void disconnect() throws Exception {
                            return;
                        }
                    };
                */
                swiftConnector = createSwiftConnector(TransportProtocolType.TCPIP);
		
		return swiftConnector;
	}

	/**
	 * Return the Map of protocol properties that are used in creating a channel
	 * to the Swift System
	 * 
	 * @param endpoint
	 *            A service end point.
	 * @return ProtocolInfo A Map of required protocol properties
	 */
	private ProtocolInfo getProtocolInfo(Endpoint endpoint)
	{
		ProtocolInfo protocolInfo = null;
		String transportProtocolName = endpoint.getSwiftAddress()
				.getTransportProtocolName();
		SwiftProtocolProperties swiftProtocolProperties = endpoint
				.getSwiftProtocolProperties();
		// right now only TCPIP transport protocol is supported
		if (transportProtocolName.equalsIgnoreCase("TCPIP"))
		{
			protocolInfo = new ProtocolInfo();
			// Server port
			protocolInfo.put(SwiftAddress.ATTR_SWIFT_SVR_LOCATION, endpoint
					.getSwiftAddress().getSwiftServerLocation());
			protocolInfo.put(SwiftAddress.ATTR_SWIFT_SVR_PORT, endpoint
					.getSwiftAddress().getSwiftServerPort().toString());
			// ProtocolInfo llpInfo =
			// Util.populateLLPInfo(SwiftProtocolProperties);
			// protocolInfo.putAll(llpInfo);
		}
		return protocolInfo;
	}

	/**
	 * Sends an acknowledgment to the Swift System
	 * 
	 * @param SwiftPayLoad
	 *            the Swift payload
	 * @param SwiftConnector
	 *            the channel to connect to Swift System
	 * @param protocolInfo
	 *            Map of properties required in establishing connection to the
	 *            Swift System
	 * @param endpoint
	 *            A service end point.
	 * @throws ApplicationException
	 *             any exception that occurs during opening a connection or
	 *             sending the message to the Swift System
	 */
	private void sendACKToExtSys(String SwiftPayLoad,
			SwiftConnector SwiftConnector, ProtocolInfo protocolInfo,
			Endpoint endpoint) throws ApplicationException
	{
		// TODO: handling recourse actions
		try
		{
			SwiftConnector.connect(protocolInfo);
		} catch (Exception ex)
		{
			mLog.log(Level.SEVERE, "OutboundMessageProcessor_CONNECT_FAILED");
			throw new ApplicationException(mMessages
					.getString("OutboundMessageProcessor_CONNECT_FAILED"));
		}
		try
		{
			SwiftConnector.sendSwiftMessage(SwiftPayLoad);
		} catch (Exception ex)
		{
			mLog.log(Level.SEVERE,
					"OutboundMessageProcessor_SEND_SwiftMSG_FAILED");
			throw new ApplicationException(mMessages
					.getString("OutboundMessageProcessor_SEND_SwiftMSG_FAILED"));
		} finally
		{
			try
			{
				// Once the message is sent close the handle to the Swift
				// external system
				SwiftConnector.disconnect();
			} catch (Exception ex)
			{
				// ignore
			}
		}

	}

	/**
	 * Sends and receive Swift Request/Ack to/from the Swift System
	 * 
	 * @param normalizedMsg
	 * @param SwiftPayLoad
	 *            the Swift payload
	 * @param SwiftConnector
	 *            the channel to connect to Swift System
	 * @param protocolInfo
	 *            Map of properties required in establishing connection to the
	 *            Swift System
	 * @param endpoint
	 *            A service end point.
	 * @throws ApplicationException
	 *             any exception that occurs during opening a connection or
	 *             sending the message to the Swift System
	 */
	private void start2WayChannelWithExtSystem(NormalizedMessage normalizedMsg,
			String SwiftPayLoad, SwiftConnector swiftConnector,
			ProtocolInfo protocolInfo, Endpoint endpoint) throws Exception
	{
		try
		{
			// TODO: handling recourse actions
			try
			{
				swiftConnector.connect(protocolInfo);
			} catch (Exception ex)
			{
				mLog.log(Level.SEVERE,
						"OutboundMessageProcessor_CONNECT_FAILED");
				throw new ApplicationException(mMessages
						.getString("OutboundMessageProcessor_CONNECT_FAILED"));
			}
			try
			{
				swiftConnector.sendSwiftMessage(SwiftPayLoad);
			} catch (Exception ex)
			{
				mLog.log(Level.SEVERE,
						"OutboundMessageProcessor_SEND_SwiftMSG_FAILED");
				throw new ApplicationException(
						mMessages
								.getString("OutboundMessageProcessor_SEND_SwiftMSG_FAILED"));
			}
			//String acceptACKCond =
			// InboundReplyContext.getAcceptAckCondtion(normalizedMsg);
			// String applicationAckCond =
			// InboundReplyContext.getAppAckCondtion(normalizedMsg);
			// When Acknowledgment mode is enhanced and acknowledgment condition
			// is NE (NEVER)
			// do not wait for the acknowledgment from external system
			// if (ackMode.equals(ACK_MODE_ENHANCED) &&
			// acceptACKCond.equals(NEVER_CONDITION))
			// return;
			String ackMsg = null;
			try
			{
				ackMsg = swiftConnector.recvSwiftMessage();
				if (mLog.isLoggable(Level.INFO))
				{
					mLog.log(Level.INFO, "Received Swift Message", ackMsg);
				}
			} catch (Exception ex)
			{
				mLog.log(Level.SEVERE,
						"OutboundMessageProcessor_RECV_SwiftACK_FAILED");
				throw new ApplicationException(
						mMessages
								.getString("OutboundMessageProcessor_RECV_SwiftACK_FAILED"));
			}
			// Disconnect the connection
			swiftConnector.disconnect();

		} finally
		{
			try
			{
				if (swiftConnector != null)
				{
					// Once the transaction with the external system is done;
					// close the handle to
					// the
					// Swift external system
					swiftConnector.disconnect();
				}
			} catch (Exception ex)
			{
				// ignore
			}

		}
	}

	/**
	 * Retruns the DOMResult
	 * 
	 * @param SwiftPayLoad
	 *            received acknowledgment
	 * @param endpoint
	 *            A service end point.
	 * @return String acknowledgment code
	 * @throws Exception
	 */

	private DOMResult getDOMResult(String SwiftPayLoad, Endpoint endpoint)
			throws Exception
	{
		Encoder encoder = null;
		try
		{
			// create the QName for ACK
			final QName ack = new QName("V2xTNS", "ACK");
			// get the xsd file location
			// final String xsdFileLoc = getXsdFileLocation(V2xTNS, ack,
			// endpoint.getXsdsList());
			// get the url of the xsd files
			String compInstallRoot = mComponentContext.getInstallRoot()
					.replace("\\", "/");
			// String SwiftVerDir =
			// getVersionDir(endpoint.getSwiftProtocolProperties().getVersionID());
			StringBuilder sb = null;// new
			// StringBuilder(URL_SCHEME).append(compInstallRoot).append(SCHEMAS_LOCATION).append(
			// SwiftVerDir);
			// sb.append("/").append(ACKXSD_FILENAME);
			final URL url = new URL(sb.toString());
			MetaRef metaRef = new MetaRef()
			{
				public String getPath()
				{
					return null;
				}

				public URL getURL()
				{
					return url;
				}

				public QName getRootElemName()
				{
					return ack;
				}
			};
			EncoderFactory encoderFactory = EncoderFactory.newInstance();
			encoder = encoderFactory.newEncoder(encoderFactory
					.makeType("SWIFT_ENCODINGSTYLE"), metaRef);
			// Decode raw data
			Source source = encoder.decodeFromString(SwiftPayLoad);
			DOMResult result = null;// transformToDOMResult(mTrans, source);
			return result;

		} finally
		{
			// call the encoder's dispose method for clean-up purpose
			encoder.dispose();
			encoder = null;
		}
	}

}// end of class
