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
 * @(#)HL7Constants.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc;

import javax.xml.namespace.QName;
import com.ibm.wsdl.Constants;
public interface HL7Constants {

	 // Namespaces
    public static final String NS_URI_HL7 = "http://schemas.sun.com/jbi/wsdl-extensions/hl7/";

	 // Local element names
    public static final String ELEM_ADDRESS = "address";
    public static final String ELEM_MESSAGE = "message";
	public static final String ELEM_PROTOCOLPROPERTIES = "protocolproperties";
	public static final String ELEM_COMMUNICATIONCONTROLS = "communicationcontrols";
	public static final String ELEM_COMMUNICATIONCONTROL = "communicationcontrol";

	public static final String ENV_VAR_TYPE_STRING = "STRING";
	public static final String ENV_VAR_TYPE_NUMBER = "NUMBER";
	public static final String ENV_VAR_TYPE_BOOLEAN = "BOOLEAN";
    

    // Qualified element names
    public static final QName QNAME_BINDING = new QName(NS_URI_HL7, Constants.ELEM_BINDING);
    public static final QName QNAME_OPERATION = new QName(NS_URI_HL7, Constants.ELEM_OPERATION);
    public static final QName QNAME_ADDRESS = new QName(NS_URI_HL7, ELEM_ADDRESS);
    public static final QName QNAME_MESSAGE = new QName(NS_URI_HL7, ELEM_MESSAGE);
	public static final QName QNAME_PROTOCOLPROPERTIES = new QName(NS_URI_HL7, ELEM_PROTOCOLPROPERTIES);
	public static final QName QNAME_COMMUNICATIONCONTROLS = new QName(NS_URI_HL7, ELEM_COMMUNICATIONCONTROLS);
	public static final QName QNAME_COMMUNICATIONCONTROL = new QName(NS_URI_HL7, ELEM_COMMUNICATIONCONTROL);

    public static final String HL7v21 = "2.1";

    public static final String HL7v22 = "2.2";

    public static final String HL7v23 = "2.3";

    public static final String HL7v231 = "2.3.1";

    public static final String HL7v24 = "2.4";

    public static final String HL7v25 = "2.5";

	public static final String HL7v251 = "2.5.1";

	public static final String HL7v26 = "2.6";

    public static final String V2xTNS = "urn:hl7-org:v2xml";

    public static final String ACK = "ACK";

    public static final String MSH = "MSH";

    public static final String MSA = "MSA";
    public static final String UAC = "UAC";
    public static final String SFT = "SFT";
    public static final String ERR = "ERR";

    public static final String ACK_MODE_ENHANCED = "enhanced";

    public static final String ACK_MODE_ORIGINAL = "original";

    public static final String SUCCESS = "success";

    public static final String TCPIP = "tcp-ip";

    public static final String SERVICE_NAME = "name";

    public static final String HL7_ENCODINGSTYLE = "hl7encoder-1.0";

    public static final String MSH15 = "MSH.15";

    public static final String MSH16 = "MSH.16";

    public static final String MSH13 = "MSH.13";

    public static final String MSA2 = "MSA.2";

    public static final String MSA3 = "MSA.3";

    public static final String MSH9 = "MSH.9";

    public static final String MSA4 = "MSA.4";

    public static final String MSH12 = "MSH.12";

    // Application Acknowledgments codes
    public static final String APP_ACCEPT = "AA";

    public static final String APP_REJECT = "AR";

    public static final String APP_ERROR = "AE";

    // Accept Acknowledgments codes
    public static final String COMMIT_ACCEPT = "CA";

    public static final String COMMIT_REJECT = "CR";

    public static final String COMMIT_ERROR = "CE";

    // Acknowledgment conditions
    public static final String ALWAYS_CONDITION = "AL";

    public static final String NEVER_CONDITION = "NE";

    public static final String ERROR_CONDITION = "ER";

    public static final String SUCCESS_CONDITION = "SU";

    public static final String MLLPv1 = "MLLPv1";

    public static final String MLLPv2 = "MLLPv2";

    public static final String HLLP = "HLLP";

    // Sequence number protocol releated constants
    public static final int INVALID_SEQNO = -2;

    public static final int INITIAL_ESN = -1;

    public static final int OUTBOUND_INITIAL_ESN = 0;

    public static final String NONE_ESN_STATE = "0";

    public static final String VALID_ESN_STATE = "1";

    public static final String RECV_INVALID_SEQNO = "Invalid sequence no. valid values are -1, 0 or >=1";

    public static final String MISSMATCH_SEQNO = "Sequence number mismatch";

    public static final String URL_SCHEME = "jar:file:";

    public static final String SCHEMAS_LOCATION = "/lib/hl7bcimpl.jar!/com/sun/jbi/hl7bc/extservice/ack/schemas/";

    public static final String HL7v21_DIR = "hl7_21_xsd";

    public static final String HL7v22_DIR = "hl7_22_xsd";

    public static final String HL7v23_DIR = "hl7_23_xsd";

    public static final String HL7v231_DIR = "hl7_231_xsd";

    public static final String HL7v24_DIR = "hl7_24_xsd";

    public static final String HL7v25_DIR = "hl7_25_xsd";

	 public static final String HL7v251_DIR = "hl7_251_xsd";

	 public static final String HL7v26_DIR = "hl7_26_xsd";

    public static final String ACKXSD_FILENAME = "ACK.xsd";

    public static final String TIME_TO_WAIT_FOR_RESPONSE = "TIME_TO_WAIT_FOR_A_RESPONSE";

    public static final String MAX_NO_RESPONSE = "MAX_NO_RESPONSE";

    public static final String MAX_NAK_RECEIVED = "MAX_NAK_RECEIVED";
    
    public static final String MAX_NAK_SENT = "MAX_NAK_SENT";
    
    public static final String MAX_CANNED_NAK_SENT = "MAX_CANNED_NAK_SENT";
    
    public static final String NAK_RECEIVED = "NAK_RECEIVED";

    public static final String MAX_EMPTY_READ_RETRY = "MAX_EMPTY_READ_RETRY";
    
    public static final String MAX_CONNECTION_RETRIES = "MAX_CONNECT_RETRIES";

	public static final String ACTION_RESET = "RESET";
    
    public static final String ACTION_RESEND = "RESEND";

	public static final String ACTION_SUSPEND = "SUSPEND";
	
	public static final String ACTION_ERROR = "ERROR";

    public static final String SKIPMESSAGE = "SKIPMESSAGE";

    public static final String V3 = "V3";

    public static final String CLASSESDIR="classes";
    
    public final static char DEFAULT_SEGMENT_TERMINATOR_CHAR = 13;
    
    public final static String DEFAULT_SEGMENT_TERMINATOR_STRING = String.valueOf(DEFAULT_SEGMENT_TERMINATOR_CHAR);

	public static final String MIN_POOL_SIZE = "MIN_POOL_SIZE";

	public static final String MAX_POOL_SIZE = "MAX_POOL_SIZE";

	public static final String MAX_IDLE_TIMEOUT = "MAX_IDLE_TIMEOUT";

	public static final String ALWAYS_CREATES_NEW_CONNECTION = "ALWAYS_CREATES_NEW_CONNECTION";
    
	public static final String PERSISTENCE_ENABLED = "PERSISTENCE_ENABLED";
    
}
