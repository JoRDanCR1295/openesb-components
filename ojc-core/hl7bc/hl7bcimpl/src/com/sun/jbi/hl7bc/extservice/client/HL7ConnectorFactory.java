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
 * @(#)HL7ConnectorFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

 package com.sun.jbi.hl7bc.extservice.client;


import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.hl7bc.I18n;

/**
 * Factory class for creating a HL7Connector
 * 
 * @author S. Nageswara Rao, Raghunadh
 */

 public class HL7ConnectorFactory {
    private static final Logger mLog =
        Logger.getLogger(HL7ConnectorFactory.class.getName());


	// Even though UDPIP is there, at present we support TCP IP Protocol only
	public enum TransportProtocolType { MLLPV1, MLLPV2, UDPIP, HLLP };
	
	/**
     * Construct the HL7Connector given the Protocol Information
     * 
     * @param protocolType The transport protocol type
     */
	public static HL7Connector createHL7Connector(TransportProtocolType protocolType) throws Exception {
		  switch (protocolType) {
            case MLLPV1:
                mLog.log (Level.FINE,
                          I18n.msg("I0154: Creating HL7 Server object of class {0}",
                          TCPIpHL7Connector.class.getName()));
                return new TCPIpHL7Connector ();
                
            case MLLPV2:
                mLog.log (Level.FINE,
                          I18n.msg("I0154: Creating HL7 Server object of class {0}",
							TCPIpHL7MLLPV2Connector.class.getName()));
              return new TCPIpHL7MLLPV2Connector ();
                
            case HLLP:
                mLog.log (Level.FINE,
                          I18n.msg("I0154: Creating HL7 Server object of class {0}",
							TCPIpHL7Connector.class.getName()));
              return new TCPIpHL7Connector ();

            /*
             * case UDPIP: mLog.log (Level.INFO, "HL7ConnectorFactory_HL7_CONNECTOR_CLASS", new
             * Object[]{UDPIpHL7Connector.class.getName()}); return new UDPIpHL7Connector ();
             */
            default:
                mLog.log(Level.SEVERE,
                        I18n.msg("E0268: The transport protocol type {0} is not a valid type",
							protocolType));

                String errMsg = I18n.msg("E0268: The transport protocol type {0} is not a valid type",
                        protocolType);
                throw new Exception (errMsg);

		  }
	}

 }// end of class
