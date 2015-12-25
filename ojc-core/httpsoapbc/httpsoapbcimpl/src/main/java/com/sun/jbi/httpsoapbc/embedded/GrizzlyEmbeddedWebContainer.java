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
import java.util.logging.Level;
import java.util.logging.Logger;
import org.glassfish.grizzly.http.server.NetworkListener;

/**
 * Represents an embedded Catalina web container within BC.
 */
public class GrizzlyEmbeddedWebContainer {

    static final Logger mLogger =
        Messages.getLogger(GrizzlyEmbeddedWebContainer.class);

    public static final String LISTENER_NAME = "httpbc-listener";
    
    public synchronized static NetworkListener createNetworkListener(String address, int port,
				     String protocol) {

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
        } else {
            address = NetworkListener.DEFAULT_NETWORK_HOST;
        }

        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Creating listener (address: {0}:{1}, protocol: {2}).", new Object[]{address, port, protocol});
        }
        
        NetworkListener listener = new NetworkListener(LISTENER_NAME, address, port);
        listener.setScheme("http");
        
        // Override scheme according SSL
        if (protocol.equalsIgnoreCase("https")) {
            listener.setScheme("https");
            listener.setSecure(true);
        }

        return listener;
    }
}