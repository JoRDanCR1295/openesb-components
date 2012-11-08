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
 * @(#)InboundReplyContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc;

import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.sun.jbi.hl7bc.extservice.server.HL7Callback;

/**
 * Holds objects used by the asynchronous outbound message processor to process the NMR reply it
 * receives to complete an inbound JBI message exchange (hl7->nmr, nmr->hl7).
 * 
 * @author S. Nageswara Rao
 */
public class InboundReplyContext implements HL7Constants {

    private long mRequestInvocationTime;

    private Source mHL7Request;

    private MessageExchangeReplyListener mListener;

    private HL7Callback mHL7Callback;

    //Accept Acknowledgment condition
    private String mAcceptAckCond;

    //Application Acknowledgment condition
    private String mAppAckCond;

    /**
     * Creates a new instance of InboundReplyContext
     * 
     * @param requestInvocationTime The time in milliseconds when the MessageExchange was received
     * @param hl7Request The received HL7 message
     * @param listener The call back which will be processing the reply
     * @param hl7Callback a channel to route the acknowledgments to the HL7 external system
     */
    public InboundReplyContext(long requestInvocationTime, Source hl7Request,
            MessageExchangeReplyListener listener, HL7Callback hl7Callback) {
        this.mRequestInvocationTime = requestInvocationTime;
        this.mHL7Request = hl7Request;
        this.mListener = listener;
        this.mHL7Callback = hl7Callback;
        this.mAcceptAckCond = getAcceptAckCondtion(hl7Request);
        this.mAppAckCond = getAppAckCondtion(hl7Request);
    }

    /**
     * Creates a new instance of InboundReplyContext
     * 
     * @param requestInvocationTime The time in milliseconds when the MessageExchange was received
     * @param hl7Request The received HL7 message
     * @param listener The call back which will be processing the reply
     * @param hl7Callback a channel to route the acknowledgments to the HL7 external system
     * @param ackCond The ackowledgment condition
     */
    public InboundReplyContext(long requestInvocationTime, Source hl7Request,
            MessageExchangeReplyListener listener, HL7Callback hl7Callback, String ackCond) {
        this.mRequestInvocationTime = requestInvocationTime;
        this.mHL7Request = hl7Request;
        this.mListener = listener;
        this.mHL7Callback = hl7Callback;
        // In case of originail acknowledgment protocol,
        // MSH-15-accept acknowledgment type = NE (Never) and MSH-16-application acknowledgment type
        // = AL (Always)
        this.mAppAckCond = ackCond;
    }

    /**
     * Returns the time in milliseconds when the MessageExchange (request) was received.
     * 
     * @return The time in milliseconds when the MessageExchange (request) was received.
     */
    public long getRequestInvocationTime() {
        return mRequestInvocationTime;
    }

    /**
     * Checks whether client requested accept acknowledgment for the request
     * @param hl7Message The received HL7 message
     * @return Returns one of the values AL, NE, ER, SU or null
     */
    public static String getAcceptAckCondtion(Source hl7Message) {
        Node domMessage = ((DOMSource) hl7Message).getNode();
        NodeList MSHNodeList = domMessage.getFirstChild().getFirstChild().getFirstChild().getFirstChild().getChildNodes();
        Node mshField = null;
        String name = null;
        String msh15Value = null;
        for (int i = 0; i < MSHNodeList.getLength(); i++) {
            mshField = MSHNodeList.item(i);
            name = mshField.getLocalName();
            if (name.equals(MSH15)) {
                msh15Value = mshField.getFirstChild().getNodeValue();
                break;
            }
        }
        return msh15Value;
    }

    /**
     * Checks whether client requested application acknowledgment for the request
     * @param hl7Message The received HL7 message
     * @return String Returns one of the values AL, NE, ER, SU or Null
     */
    public static String getAppAckCondtion(Source hl7Message) {
        Node domMessage = ((DOMSource) hl7Message).getNode();
        NodeList MSHNodeList = domMessage.getFirstChild().getFirstChild().getFirstChild().getFirstChild().getChildNodes();
        Node mshField = null;
        String name = null;
        String msh16Value = null;
        for (int i = 0; i < MSHNodeList.getLength(); i++) {
            mshField = MSHNodeList.item(i);
            name = mshField.getLocalName();
            if (name.equals(MSH16)) {
                msh16Value = mshField.getFirstChild().getNodeValue();
                break;
            }
        }
        return msh16Value;
    }

    /**
     * Returns the received HL7 Message
     * @return The HL7 message.
     */
    public Source getHL7Request() {
        return mHL7Request;
    }

    /**
     * Returns the reply listener which will process the NMR reponse.
     * @return The inbound reply listener.
     */
    public MessageExchangeReplyListener getMessageExchangeReplyListener() {
        return mListener;
    }

    /**
     * Returns the channel to route the acknowledgments to the HL7 external system 
     * @return The HL7 Callback.
     */
    public HL7Callback getHL7Callback() {
        return mHL7Callback;
    }

    /**
     * Returns one of the values AL, NE, ER, SU
     */
    public String getAcceptAckCondition() {
        return mAcceptAckCond;
    }

    /**
     * Returns one of the values AL, NE, ER, SU
     */
    public String getAppAckCondition() {
        return mAppAckCond;
    }
}
