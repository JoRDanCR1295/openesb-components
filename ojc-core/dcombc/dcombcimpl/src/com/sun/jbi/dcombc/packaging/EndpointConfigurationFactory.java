/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.packaging;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

/**
 * Factory class for creating an EndpointConfiguration
 * 
 * @author Chandrakanth Belde
 */
public class EndpointConfigurationFactory {
	/**
	 *
	 */
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
            mLog.log(Level.INFO, "EndpointConfigurationFactory.END_POINT_CONFIG_CLASS",
                    new Object[] { EndpointConfigurationPortmap.class.getName() });
            return new EndpointConfigurationPortmap(suRootDir);
        case END_POINT_CONFIG_TYPE_SU_DESCRIPTOR:
            mLog.log(Level.INFO, "EndpointConfigurationFactory.END_POINT_CONFIG_CLASS",
                    new Object[] { EndpointConfigurationSUDescriptor.class.getName() });
            return new EndpointConfigurationSUDescriptor(suRootDir);
        default:
            mLog.log(Level.SEVERE, "EndpointConfigurationFactory.UNSUPPORTED_END_POINT_CONFIG_TYPE",
                    new Object[] { new Integer(configType) });

            String errMsg = mMessages.getString("EndpointConfigurationFactory.UNSUPPORTED_END_POINT_CONFIG_TYPE",
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
