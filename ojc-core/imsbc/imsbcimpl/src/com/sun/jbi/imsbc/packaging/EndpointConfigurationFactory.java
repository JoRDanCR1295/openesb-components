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

package com.sun.jbi.imsbc.packaging;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * Factory class for creating an EndpointConfiguration
 * 
 * @author Sun Microsystems
 */
public class EndpointConfigurationFactory {

    private static final Messages mMessages = Messages.getMessages(EndpointConfigurationFactory.class);

    private static final Logger mLog = Messages.getLogger(EndpointConfigurationFactory.class);

	public static EndpointConfiguration getEndpointConfiguration(String suRootDir) throws Exception {
			return new EndpointConfigurationSUDescriptor(suRootDir);
	}

    /**
     * Determines whether JBI routing is enabled (switch on)
     */
    public static boolean isJBIRoutingEnabled() {
        return EndpointConfigurationSUDescriptor.isJBIRoutingEnabled();
    }

}