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
 * @(#)SwiftConnectorFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

 package com.sun.jbi.swiftbc.extservice.client;


import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.swiftbc.extservice.ProtocolInfo;

/**
 * Factory class for creating a SwiftConnector
 * 
 * @author S. Nageswara Rao
 */

 public class SwiftConnectorFactory {
	private static final Messages mMessages =
        Messages.getMessages(SwiftConnectorFactory.class);
    private static final Logger mLog =
        Messages.getLogger(SwiftConnectorFactory.class);


	// Even though UDPIP is there, at present we support TCP IP Protocol only
	public enum TransportProtocolType { TCPIP, UDPIP };
	
	/**
     * Construct the SwiftConnector given the Protocol Information
     * 
     * @param protocolType The transport protocol type
     */
	public static SwiftConnector createSwiftConnector(TransportProtocolType protocolType) throws Exception {
		  switch (protocolType) {
            case TCPIP:
                return new TCPIpSwiftConnector ();
            /*
             * case UDPIP: mLog.log (Level.INFO, "SwiftConnectorFactory_Swift_CONNECTOR_CLASS", new
             * Object[]{UDPIpSwiftConnector.class.getName()}); return new UDPIpSwiftConnector ();
             */
            default:
                mLog.log(Level.SEVERE,
                        "SwiftConnectorFactory_UNSUPPORTED_TRANSPORT_PROTOCOL_TYPE",
                        new Object[]{protocolType});

                String errMsg = mMessages.getString("SwiftConnectorFactory_UNSUPPORTED_TRANSPORT_PROTOCOL_TYPE",
                        new Object[]{protocolType});
                throw new Exception (errMsg);

		  }
	}

 }
