package com.sun.jbi.httpsoapbc;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.ws.handler.MessageContext;

public interface SoapNormalizer {

    // Settings for custom reliability header extensions
    public static final String CUSTOM_RELIABILITY_MESSAGE_ID_PROPERTY = "com.stc.jbi.messaging.messageid"; // NOI18N
    public static final String CUSTOM_RELIABILITY_HEADER_NAMESPACE_URI = "http://schemas.stc.com/ws/2005/07/custrm"; // NOI18N
    public static final String CUSTOM_RELIABILITY_HEADER_LOCAL_NAME = "MessageID"; // NOI18N
    
    /**
     * JBI message exchange properties for message grouping and sequencing (for
     * redelivery, throttling etc.)
     */
    public static final String CRMP_GROUP_ID = "com.sun.jbi.messaging.groupid";
    public static final String CRMP_MESSAGE_ID = "com.sun.jbi.messaging.messageid";
    
    /**
     * WS-ReliabilityMessaging message id and sequence soap:header elements.
     */
    // <wsrm:Sequence ...>
    // <wsrm:Identifier ...>http://Business456.com/RM/ABC</wsrm:Identifier>
    // <wsrm:MessageNumber>2</wsrm:MessageNumber>
    // </wsrm:Sequence>
    public static final String WSRM__1_0__NAMESPACE_URI = "http://schemas.xmlsoap.org/ws/2005/02/rm";
    public static final String WSRM__1_1__NAMESPACE_URI = "http://docs.oasis-open.org/ws-rx/wsrm/200702";
    public static final String WSRM_Sequence = "Sequence";
    public static final String WSRM_Sequence_Identifier = "Identifier";
    public static final String WSRM_Sequence_MessageNumber = "MessageNumber";
    public static final QName QNAME_WSRM__1_0__Sequence_Identifier = new QName(WSRM__1_0__NAMESPACE_URI, WSRM_Sequence_Identifier);
    public static final QName QNAME_WSRM__1_0__Sequence_MessageNumber = new QName(WSRM__1_0__NAMESPACE_URI, WSRM_Sequence_MessageNumber);
    public static final QName QNAME_WSRM__1_1__Sequence_Identifier = new QName(WSRM__1_1__NAMESPACE_URI, WSRM_Sequence_Identifier);
    public static final QName QNAME_WSRM__1_1__Sequence_MessageNumber = new QName(WSRM__1_1__NAMESPACE_URI, WSRM_Sequence_MessageNumber);
    

    public void setPropagateSoapHeader(boolean propagateSoapHeader);
    
    public abstract NormalizedMessage normalize(Object message, MessageExchange exchange, OperationMetaData meta, boolean inMsg, MessageContext context) throws MessagingException, SOAPException;

    public abstract Fault normalizeFault(SOAPFault soapFault, MessageExchange exchange, OperationMetaData meta, boolean failFast) throws MessagingException, SOAPException;

}