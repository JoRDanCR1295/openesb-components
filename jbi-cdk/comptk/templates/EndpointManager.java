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
 * @(#)%CLASS_PREFIX%EndpointManager.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package %PKG%;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Operation;

import com.sun.jbi.common.classloader.CustomClassLoaderUtil.SwitchType;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.common.util.EntryRegistry;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager;

/**
 * Manages %PROJ_SHORT_NAME% %COMP_TYPE_DESC% endpoints and acts as 
 * its own {@link Endpoint} factory.
 * 
 * @author %AUTHOR%
 */
public class %CLASS_PREFIX%EndpointManager extends AbstractEndpointManager {
    /** Constructs a %PROJ_SHORT_NAME% %COMP_TYPE_DESC% endpoint manager. */
    public %CLASS_PREFIX%EndpointManager(ComponentContext ctx) {
        super(ctx);	// this EndpointManager acts as its own EndpointFactory
    }

	/** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#createEndpoint(com.sun.jbi.common.descriptor.EndpointInfo, com.sun.jbi.common.descriptor.ServiceUnit) */
    public Endpoint<%SERVICE_DEF_NAME%> createEndpoint(EndpointInfo info, ServiceUnit srvcUnit)
            throws DeploymentException {
        // The following line switches classloaders to load application jars deployed with a service unit
        getContext().getCustomClassLoaderUtil()
                .switchClassLoader(srvcUnit.getName(), SwitchType.service_classloader);
    	try {
    		// TODO (required) Create an %CLASS_PREFIX%Endpoint instance
    	    %CLASS_PREFIX%Endpoint endpt = null;
    		return endpt;
    	} 
    	finally {
    		// switch back to original classloader...
    		getContext().getCustomClassLoaderUtil()
    		        .switchClassLoader(srvcUnit.getName(), SwitchType.context_classloader);
    	}
    }
}

