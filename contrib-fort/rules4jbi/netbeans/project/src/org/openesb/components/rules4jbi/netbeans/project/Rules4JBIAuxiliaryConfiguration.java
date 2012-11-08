/*
 * @(#)Rules4JBIAuxiliaryConfiguration.java        $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.project;

import java.util.logging.Logger;
import org.netbeans.spi.project.AuxiliaryConfiguration;
import org.w3c.dom.Element;

/**
 * A stub for <code>AuxiliaryConfiguration</code>; does nothing useful at the moment.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1.1.1 $ $Date: 2008/06/30 08:53:20 $
 * 
 * @since 0.1
 */
public class Rules4JBIAuxiliaryConfiguration implements AuxiliaryConfiguration {
    
    private static final Logger logger = Logger.getLogger(Rules4JBIAuxiliaryConfiguration.class.getName());

    public Element getConfigurationFragment(String elementName, String namespace, boolean shared) {
        logger.finest("Retrieving configuration fragment {" + namespace + "}" + elementName + ", "
                + (shared ? "shared" : "private"));
        
        return null;
    }

    public void putConfigurationFragment(Element fragment, boolean shared) throws IllegalArgumentException {
        logger.finest("Storing configuration fragment {" + fragment.getNamespaceURI() + "}"
                + fragment.getNodeName() + (shared ? "shared" : "private"));
    }

    public boolean removeConfigurationFragment(String elementName, String namespace, boolean shared) throws IllegalArgumentException {
        logger.finest("Removing configuration fragment {" + namespace + "}" + elementName + ", "
                + (shared ? "shared" : "private"));
        
        return true;
    }
}
