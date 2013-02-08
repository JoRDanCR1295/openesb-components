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
 * @(#)HL7Connector.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.hl7bc.extservice.client;

import com.sun.jbi.hl7bc.connection.Connection;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.Endpoint;

/***************************************************************************************************
 * An Interface that all the transport protocol specific implementation classes must implement to
 * provide communication with the HL7 External System
 * 
 * @author S. Nageswara Rao, Raghunadh
 * @version 
 */
public interface HL7Connector {

    // Establishes a connection with the HL7 Extenral System/Server
    void connect(ProtocolInfo protocolInfo) throws Exception;
	// Establishes a connection with the HL7 external System/Server, if we use connection pooling 
    void connect(ProtocolInfo protocolInfo, Endpoint endpoint) throws Exception;

    // Sends HL7Message to HL7 External System
    void sendHL7Message(String hl7Msg) throws Exception;

    // Receives HL7 Message from HL7 External System
    String recvHL7Message() throws Exception;
    // Receives HL7 Message from HL7 External System
    String recvHL7Message(long timeToWait) throws Exception;
    //Disconnect the connection with the HL7 External System/Server
    // and return to the pool
    void disconnect() throws Exception;
    // Disconnect the connection with the HL7 External System/Server
    void discardConnection() throws Exception;
    // get the connection associated with the connector    
    Connection getHL7Connection() throws Exception;
    // set the connection to the connector
    void setHL7Connection(Connection conn) throws Exception;
    //  set back the assoaciated IOSession on the connector
    void setIoSession(Connection conn) throws Exception;
    // get the protocol properties associated with the connector    
    ProtocolInfo getProtocolInfo() throws Exception;
    // get the ProtocalInfo to the connector
    void setProtocolInfo(ProtocolInfo pInfo) throws Exception;

}// end of the interface
