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
 * @(#)SUDescriptor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor;

import javax.jbi.management.DeploymentException;

import org.xml.sax.InputSource;

import com.sun.jbi.common.descriptor.parsers.sax.ServiceAssemblyHandler;
import com.sun.jbi.common.descriptor.parsers.sax.StackableHandler;
import com.sun.jbi.common.util.I18n;

/**
 * Utility to parse and model a service assembly descriptor.
 * 
 * @author Kevan Simpson
 */
public class ServiceAssemblyDescriptor extends JbiDescriptor {
    /**
     * Parses the specified source into a JBI service assembly.
     * 
     * @param source The service assembly descriptor.
     * @return a JBI service assembly.
     * @throws DeploymentException if an error occurs reading or parsing descriptor.
     */
    public static ServiceAssembly parse(InputSource source) throws DeploymentException {
        try {
            StackableHandler<ServiceAssembly> stack =
                    new StackableHandler<ServiceAssembly>(
                            new ServiceAssemblyHandler());
            return stack.parse(source);
        }
        catch (DeploymentException de) {
            throw de;    // already logged
        }
        catch (Exception e) {
            throw error(I18n.loc("UTIL-6001: Failed to parse service assembly descriptor: {0}",
                                 e.getMessage()), 
                        e);
        }
    }
}
