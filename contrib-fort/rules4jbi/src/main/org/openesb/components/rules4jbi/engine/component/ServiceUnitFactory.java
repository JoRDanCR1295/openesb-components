/*
 * @(#)ServiceUnitFactory.java        $Revision: 1.3 $ $Date: 2008/07/14 16:30:25 $
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

package org.openesb.components.rules4jbi.engine.component;

import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.name.Named;

import org.openesb.components.rules4jbi.shared.logging.Logger;

/**
 * Factory that produces <code>ServiceUnit</code> instances properly injected with dependencies.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.3 $ $Date: 2008/07/14 16:30:25 $
 * 
 * @since 0.1
 */
public class ServiceUnitFactory {
    
    @Inject @Named("ServiceUnitFactory")
    private Logger logger;
    
    @Inject
    private Provider<ServiceUnit> serviceUnitProvider;
    
    ServiceUnit createNewServiceUnit(String name, String rootPath) {
        logger.fine("Creating new service unit '%s' with rooth path '%s'", name, rootPath);
        
        ServiceUnit su = serviceUnitProvider.get();
        
        su.setName(name);
        su.setRootPath(rootPath);
        
        return su;
    }
}
