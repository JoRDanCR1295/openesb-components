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

package com.sun.jbi.snmpbc.packaging;

import com.sun.jbi.internationalization.Messages;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Factory class for creating an EndpointConfiguration
 * 
 */
public class EndpointConfigurationFactory {
    
    private static final Messages mMessages =
        Messages.getMessages(EndpointConfigurationFactory.class);
    private static final Logger mLog =
        Logger.getLogger(EndpointConfigurationFactory.class.getName());

    
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
     * Constructs an EndpointConfiguration given the type of configuration and
     * SU root dir.
     *
     * @param configType The type of configuration used for endpoints.
     * @param suRootDir The SU root directory path.
     *
     * @return 
     * @throws Exception upon error.
     */
    public static EndpointConfiguration getEndpointConfiguration (int configType,
                                                                  String suRootDir) 
        throws Exception {
        switch (configType) {
            case END_POINT_CONFIG_TYPE_PORTMAP:
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log (Level.FINE,
                              "SNMPBC_C00101.UNSUPPORTED_END_POINT_CONFIG_TYPE",
                              EndpointConfigurationPortmap.class.getName());
                }
                return new EndpointConfigurationPortmap (suRootDir);
            case END_POINT_CONFIG_TYPE_SU_DESCRIPTOR:
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.log (Level.FINE,
                              "SNMPBC_C00102.END_POINT_CONFIG_CLASS",
                              EndpointConfigurationSUDescriptor.class.getName());
                }
                return new EndpointConfigurationSUDescriptor (suRootDir);
            default:
                String errMsg = mMessages.getString("SNMPBC_C00101.UNSUPPORTED_END_POINT_CONFIG_TYPE",
                                                    Integer.valueOf(configType));
                throw new Exception (errMsg);
        }
    }
    
    /**
     * Determines whether JBI routing is enabled (switch on)
     * @return 
     */
    public static boolean jbiRoutingEnabled() {
        return EndpointConfigurationSUDescriptor.jbiRoutingEnabled();
    }
}
