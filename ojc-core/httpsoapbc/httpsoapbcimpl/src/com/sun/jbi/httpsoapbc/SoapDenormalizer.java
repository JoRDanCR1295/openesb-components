package com.sun.jbi.httpsoapbc;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPMessage;

public interface SoapDenormalizer {

    /**
     * URI for the XSD Schema namespace, used in WS BP 1.0 and 1.1
     */
    public static final String SCHEMA_XSD_URI_2001 = "http://www.w3.org/2001/XMLSchema"; // NOI18N
    /**
     * URI for the XSD Schema instance namespace, used in WS BP 1.0 and 1.1
     */
    public static final String SCHEMA_XSI_URI_2001 = "http://www.w3.org/2001/XMLSchema-instance"; // NOI18N
    /**
     * URI for the draft XSD Schema namespace referenced by the original SOAP 1.1 release
     */
    public static final String SCHEMA_XSD_URI_1999 = "http://www.w3.org/1999/XMLSchema"; // NOI18N
    /**
     * URI for the draft XSD Schema instance namespace referenced by the original SOAP 1.1 release
     */
    public static final String SCHEMA_XSI_URI_1999 = "http://www.w3.org/1999/XMLSchema-instance"; // NOI18N
    /**
     * URI for the XMLNS Schema namespace
     */
    public static final String SCHEMA_XMLNS_URI_2000 = "http://www.w3.org/2000/xmlns/"; // NOI18N
    
    /**
      * URI for SOAP RPC encodingStyle
      */
    public static final String SOAP_RPC_ENCODING_STYLE = "http://schemas.xmlsoap.org/soap/encoding/";  // NOI18N
    
    /**
     * The BP 1.0 clarifies in R2729 that for RPC literal, the response wrapper
     * should be operation name + "Response"
     */
    public static final String RPC_RESPONSE_SUFFIX = "Response"; // NOI18N
    public static final String XOP_CONTENT_ID_SUFFIX = "org.glassfish.openesb.com";
    /**
     * Defines the JBI identifier for an 'in' message, for use with exchange.getMessage
     */
    public static final String IN_MSG = "in"; // NOI18N
    public static final String OUT_MSG = "out";

    /**
     * Only a single thread should invoke this method on the same instance at the same time
     * as it uses the builder instance member variable without guard against concurrency
     */
    public abstract SOAPMessage denormalize(NormalizedMessage normalizedMessage, MessageExchange exchange, OperationMetaData meta, MessageFactory messageFactory, boolean inMsg)
	    throws MessagingException;

}