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
 * @(#)GrizzlyEmbeddedWebContainer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.embedded;

import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;
import java.util.logging.Level;

import org.apache.catalina.Connector;

import com.sun.jbi.httpsoapbc.HttpSoapBindingLifeCycle;

/**
 * Represents an embedded Catalina web container within BC.
 */
public class GrizzlyEmbeddedWebContainer {

    Logger mLogger =
        Messages.getLogger(GrizzlyEmbeddedWebContainer.class);

    /**
     * Create a customized version of the Tomcat's 5 Coyote Connector. This
     * connector is required in order to support PE Web Programmatic login
     * functionality.
     * @param address InetAddress to bind to, or <code>null</code> if the
     * connector is supposed to bind to all addresses on this server
     * @param port Port number to listen to
     * @param protocol the http protocol to use.
     */
    public Connector createConnector(String address, int port,
				     String protocol,
                                     HttpSoapBindingLifeCycle lifeCycle) {

        if (address != null) {
            /*
             * InetAddress.toString() returns a string of the form
             * "<hostname>/<literal_IP>". Get the latter part, so that the
             * address can be parsed (back) into an InetAddress using
             * InetAddress.getByName().
             */
            int index = address.indexOf('/');
            if (index != -1) {
                address = address.substring(index + 1);
            }
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,"Creating connector (address: "
                        + ((address == null) ? "ALL" : address)
                        + ":" + port + ", protocol: " + protocol + ").");
        }

        BCCoyoteConnector connector = new BCCoyoteConnector(lifeCycle);
    
        String GRIZZLY_CONNECTOR = 
                "com.sun.enterprise.web.connector.grizzly.GrizzlyHttpProtocol";
        connector.setProtocolHandlerClassName(GRIZZLY_CONNECTOR);        
        
        
        if (address != null) {
            connector.setAddress(address);
        }

        connector.setPort(port);
    
        if (protocol.equalsIgnoreCase("https")) {
            connector.setScheme("https");
            connector.setSecure(true);
        }

        return (connector);
    }
}
