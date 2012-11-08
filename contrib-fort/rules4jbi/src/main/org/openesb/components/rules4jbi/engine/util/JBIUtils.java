/*
 * @(#)JBIUtils.java        $Revision: 1.3 $ $Date: 2008/11/11 00:25:28 $
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

package org.openesb.components.rules4jbi.engine.util;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.openesb.components.rules4jbi.shared.logging.Logger;

/**
 * This class provides various JBI specific utilities.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/11/11 00:25:28 $
 * 
 * @since 0.1
 */
public final class JBIUtils {

    /* We do not want to instantiate this class */
    private JBIUtils() {}
    
    /**
     * Compares two <code>ServiceEndpoint</code> interface implementations; returns true iff they
     * refer to the same addressable entity within the JBI system.
     * Note that because the <code>ServiceEndpoint</code> implementations might come from different
     * JBI system/component vendors, using <code>java.lang.Object.equals()</code> would not work as
     * expected. Also note that the comparison does not involve EPRs created by <code>getAsReference()</code>
     * method, nor does is involve interfaces returned by <code>getInterfaces()</code> method. That is,
     * two ServiceEndpoint implementations are considered equal if they have the same endpoint name
     * and the same service name.
     * <p>
     * This operation has similar properties as <code>java.lang.Object.equals()</code> method,
     * i.e. it is reflexive, symmetric, transitive, and consistent.
     * 
     * @param endpoint1 first service endpoint to compare; must be non null.
     * @param endpoint2 second service endpoint to compare with the first one; must be non null.
     * @return true iff the specified service endpoints represent
     */
    public static boolean equal(ServiceEndpoint endpoint1, ServiceEndpoint endpoint2) {
        if (endpoint1 == null || endpoint2 == null) {
            throw new NullPointerException("Compared endpoints must not be null");
        }
        
        if (!endpoint1.getEndpointName().equals(endpoint2.getEndpointName())) {
            return false;
        }
        
        if (!endpoint1.getServiceName().equals(endpoint2.getServiceName())) {
            return false;
        }
        
//        if (endpoint1.getInterfaces().length != endpoint2.getInterfaces().length) {
//            return false;
//        }
//        
//        for (int i = 0; i < endpoint1.getInterfaces().length; i++) {
//            if (!endpoint1.getInterfaces()[i].equals(endpoint2.getInterfaces()[i])) {
//                return false;
//            }
//        }
        
        return true;
    }
    
    public static void logServiceEndpoint(Logger logger, ServiceEndpoint serviceEndpoint) {
        if (serviceEndpoint != null) {
            logger.fine("Service Endpoint: ");
            
            QName[] interfaces = serviceEndpoint.getInterfaces();
            if (interfaces != null) {
                for (QName anInterface : interfaces) {
                    logger.fine("Interface: %s", anInterface);
                }
            } else {
                logger.fine("Interfaces is null");
            }

            logger.fine("Service name: %s", serviceEndpoint.getServiceName());
            logger.fine("Endpoint name: %s", serviceEndpoint.getEndpointName());
            
        } else {
            logger.fine("ServiceEndpoint is null");
        }
    }
}
