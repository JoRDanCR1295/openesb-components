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
 * @(#)SwiftServerFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.swiftbc.extservice.server;

import com.sun.jbi.internationalization.Messages;
import java.util.logging.Level;

import java.util.logging.Logger;


/**
 * Factory class for creating a SwiftServer
 *
 * @author S. Nageswara Rao
 */
public class SwiftServerFactory {
    private static final Messages mMessages = Messages.getMessages(SwiftServerFactory.class);
    private static final Logger mLog = Messages.getLogger(SwiftServerFactory.class);
    
    /**
     * Construct the SwiftServer given the Protocol Information
     *
     * @param protocolType The transport protocol type
     */
    public static SwiftServer createSwiftServer(
            TransportProtocolType protocolType) throws Exception {
        
        switch (protocolType) {
            
        case TCPIP:
            mLog.log(Level.INFO,
                    "SwiftServerFactory_Swift_SERVER_CLASS",
                    new Object[]{TcpIpSwiftServer.class.getName()});
            return new TcpIpSwiftServer();
            
        /*
         * case UDPIP: mLog.log (Level.INFO, "SwiftServerFactory_Swift_SERVER_CLASS", new
         * Object[]{UDPIpSwiftServer.class.getName()}); return new UDPIpSwiftServer ();
         */
        
        
        default:
            mLog.log(Level.SEVERE,
                    "SwiftServerFactory_UNSUPPORTED_TRANSPORT_PROTOCOL_TYPE",
                    new Object[]{protocolType});
            
            String errMsg = mMessages.getString("SwiftServerFactory_UNSUPPORTED_TRANSPORT_PROTOCOL_TYPE",
                    new Object[]{protocolType});
            throw new Exception(errMsg);
            
    }
   
}

// Even though UDPIP is there, at present we support TCP IP Protocol only
public enum TransportProtocolType {TCPIP,
        MOCK}

} // end of class
