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
 * @(#)Extension.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.httpsoapbc.servletsupport.SynchronousServletRequestProcessor;
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Logger;
import java.util.logging.Level;

/**
 *
 */
public class Extension implements ExtensionMBean {

    private static final Logger mLogger =
        Messages.getLogger(Extension.class);
            
    private HttpSoapBindingLifeCycle mLifeCycle;
    protected Extension() {
        
    }
    
    public Extension(HttpSoapBindingLifeCycle lifeCycle) {
        mLifeCycle = lifeCycle;
    }
    
    /**
     * Obtain an instance of a synchronous servlet request processor backed by
     * this binding component.
     * @see SynchronousServletRequestProcessor
     *      
     * Beware, the SynchronousServletRequestProcessor returned is not
     * serializable and is only of use if the local MBeanServer implementation
     * used returns a direct reference.  If calling this from outside a context
     * where classloader can load this class, the return value has to be
     * handled via reflection.
     */
    public SynchronousServletRequestProcessor
            obtainSynchronousServletRequestProcessor(String targetDeploymentId) throws Exception {
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE, "Returning a request processor for target deployment Id: " + targetDeploymentId);
        }
        return new SynchronousServletRequestProcessor(mLifeCycle);
    }    
}
