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
 * @(#)QosAssemblyDescriptor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor;

import javax.jbi.management.DeploymentException;

import org.xml.sax.InputSource;

import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.ServiceAssembly;
import com.sun.jbi.common.descriptor.parsers.sax.StackableHandler;
import com.sun.jbi.common.qos.descriptor.parsers.sax.QoSHandler;
import com.sun.jbi.common.util.I18n;

/**
 * Utility to parse and model a service assembly descriptor
 * with Sun's QoS configuration.
 * 
 * @author Kevan Simpson
 */
public class QosAssemblyDescriptor extends JbiDescriptor {
    /**
     * Parses service connections and QoS configurations.
     * 
     * @param source The descriptor to parse.
     * @return Assembly and QoS configuration.
     * @throws DeploymentException If an error occurs while parsing.
     */
    public static QoSAssembly parse(InputSource source) throws DeploymentException {
		try {
			QoSHandler qos = new QoSHandler();
			StackableHandler<ServiceAssembly> stack =
					new StackableHandler<ServiceAssembly>(qos);

			ServiceAssembly sa = stack.parse(source); 
		    return new QoSAssembly(sa, 
		    					   qos.getServiceQualities(),
		    					   qos.getQoSConnections());
		}
		catch (Exception e) {
		    throw error(I18n.loc("QOS-6003: Failed to parse QoS configuration: {0}",
		    				     e.getMessage()), 
		    			e);
		}
    }
}
