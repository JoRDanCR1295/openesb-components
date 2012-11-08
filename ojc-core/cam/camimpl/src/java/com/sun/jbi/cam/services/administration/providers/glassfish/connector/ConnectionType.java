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
 * @(#)ConnectionType.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.services.administration.providers.glassfish.connector;

import com.sun.jbi.cam.common.resources.Messages;

/**
 * Defines the type of connection to use
 * @author graj
 */
public enum ConnectionType {
    HTTP("s1ashttp"), HTTPS("s1ashttps"), JRMP("jmxrmi");

    String protocol;

    /** @param protocolString */
    private ConnectionType(String protocolString) {
        this.protocol = protocolString;
    }

    /** @return the protocol */
    public String getProtocol() {
        return protocol;
    }

    /** @return the description */
    public String getDescription() {
        switch (this) {
        case HTTP:
            return Messages.getString("glassfish_httpProtocolDescription");
        case HTTPS:
            return Messages.getString("glassfish_httpSecureProtocolDescription");
        case JRMP:
            return Messages.getString("glassfish_jrmpProtocolDescription");
        default:
            return Messages.getString("glassfish_unknownProtocolDescription");
        }
    }

}
