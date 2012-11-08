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
 * @(#)EndpointConfigurationFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.packaging;

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

    // Types of configurations available

    /**
     * Use the portmap.xml file for gathering all endpoint information
     */
    public static final int END_POINT_CONFIG_TYPE_PORTMAP = 0;

    /**
     * Use the SU descriptor file, jb.xml, for gathering all endpoint information
     */
    public static final int END_POINT_CONFIG_TYPE_SU_DESCRIPTOR = 1;

    /**
     * Constructs an EndpointConfiguration given the type of configuration and SU root dir.
     * 
     * @param configType The type of configuration used for endpoints.
     * @param suRootDir The SU root directory path.
     * @throws Exception upon error.
     */
    public static EndpointConfiguration getEndpointConfiguration(int configType, String suRootDir) throws Exception {
        switch (configType) {
        case END_POINT_CONFIG_TYPE_PORTMAP:
            mLog.log(Level.INFO, "EndpointConfigurationFactory_END_POINT_CONFIG_CLASS",
                    new Object[] { EndpointConfigurationPortmap.class.getName() });
            return new EndpointConfigurationPortmap(suRootDir);
        case END_POINT_CONFIG_TYPE_SU_DESCRIPTOR:
            mLog.log(Level.INFO, "EndpointConfigurationFactory_END_POINT_CONFIG_CLASS",
                    new Object[] { EndpointConfigurationSUDescriptor.class.getName() });
            return new EndpointConfigurationSUDescriptor(suRootDir);
        default:
            mLog.log(Level.SEVERE, "EndpointConfigurationFactory_UNSUPPORTED_END_POINT_CONFIG_TYPE",
                    new Object[] { new Integer(configType) });

            String errMsg = mMessages.getString("EndpointConfigurationFactory_UNSUPPORTED_END_POINT_CONFIG_TYPE",
                    new Object[] { new Integer(configType) });
            throw new Exception(errMsg);
        }
    }

    /**
     * Determines whether JBI routing is enabled (switch on)
     */
    public static boolean isJBIRoutingEnabled() {
        return EndpointConfigurationSUDescriptor.isJBIRoutingEnabled();
    }
}
