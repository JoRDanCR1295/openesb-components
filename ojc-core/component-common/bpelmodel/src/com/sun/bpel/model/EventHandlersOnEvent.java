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
 * @(#)EventHandlersOnEvent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;

import javax.xml.namespace.QName;

import com.sun.wsdl4j.ext.DeferredActionAccepter;

/**
 * Describes the &lt;onMessage&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface EventHandlersOnEvent extends Activity, SingleActivityHolder, PartnerLinkReference,
    PortTypeReference, OperationReference, VariableReference, FromPartReference, VariableScope,
    DeferredActionAccepter {
    /** Tag for this element. */
    public static final String TAG = "onEvent"; // NOI18N

    /** Ordinal position of partnerLink attribute. */
    public static final int PARTNERLINK = NUM_STANDARD_ATTRS;

    /** Ordinal position of portType attribute. */
    public static final int PORT_TYPE = PARTNERLINK + 1;

    /** Ordinal position of operation attribute. */
    public static final int OPERATION = PORT_TYPE + 1;

    /** Ordinal position of variable attribute. */
    public static final int VARIABLE = OPERATION + 1;

    /** Ordinal position of the messageExchange attributes */
    public static final int MESSAGE_EXCHANGE = VARIABLE + 1;

    /** Ordinal position of the messageType attributes */
    public static final int MESSAGE_TYPE = MESSAGE_EXCHANGE + 1;

    /** Ordinal position of the element attribute */
    public static final int ELEMENT_POSITION = MESSAGE_TYPE + 1;

    /** Total number of attributes */
    public static final int NUM_ATTRS = ELEMENT_POSITION + 1;

    /**
     * Getter for messageExchange attribute
     *
     * @return DOCUMENT ME!
     */
    String getMessageExchange();

    /**
     * Setter for messageExchange attribute
     *
     * @param value DOCUMENT ME!
     */
    void setMessageExchange(String value);

    /**
     * Getter for the correlations sub-element.
     *
     * @return correlations sub-element.
     */
    Correlations getCorrelations();

    /**
     * Setter for the correlations sub-element.
     *
     * @param c correlations sub-element.
     */
    void setCorrelations(Correlations c);

    /**
     * Set message type
     *
     * @param messageType
     */
    void setMessageType(QName messageType);

    /**
     * Get message type.
     *
     * @return message type.
     */
    QName getMessageType();

    /**
     * Set message type
     *
     * @param messageType
     */
    void setElement(QName messageType);

    /**
     * Get message type.
     *
     * @return message type.
     */
    QName getElement();

    /**
     * Describes the attributes of this element.
     */
    public interface ATTR extends PartnerLinkReference.ATTR, PortTypeReference.ATTR,
        OperationReference.ATTR, VariableReference.ATTR {
        /** messageExchange attribute token */
        public static final String MESSAGE_EXCHANGE = "messageExchange";

        /** messageType attribute token */
        public static final String MESSAGETYPE = "messageType";

        /** element attribute token */
        public static final String ELEMENT = "element";
    }
    
    /**
     * Initializes member variables
     */    
    void initMembers();
    
    /**
     * Creates member variables without resolving them further to XSD artifacts. 
     */
    void initMembersLocally();
}
