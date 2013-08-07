/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-esb.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)CamelSERuntime.java 
 *
 * Copyright 2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.openesb.components.camelse;

import org.openesb.components.camelse.common.ComponentRuntime;
import javax.jbi.component.ComponentLifeCycle;
import javax.jbi.component.ServiceUnitManager;

/**
 * This class extends the ComponentRuntime that implements javax.jbi.component.Component
 * interface required for the component contract at runtime.
 *
 * This class provides the component specific ComponentLifeCycle implementation
 * as well as the component specific ServiceUnitManager implementation. 
 *
 * Add any additional component runtime specific functionality here.
 *
 * @see javax.jbi.component.Component
 * @see com.sun.jbi.sample.component.common.ComponentRuntime
 * @see com.sun.jbi.sample.component.common.BasicComponentLifecycle
 * @see com.sun.jbi.sample.component.common.deployment.AbstractServiceUnitManager
 * @author chikkala
 */
public class CamelSERuntime extends ComponentRuntime {
    
    /** Creates a new instance of MyEngineRuntime */
    public CamelSERuntime() {
        super();
    }
    
    /**
     * overriding the parent's createComponentLifeCycle to create
     * component specific component lifecycle implementation.
     */
    @Override
    protected ComponentLifeCycle createComponentLifeCycle() {
        return new CamelSEComponentLifeCycle(this);
    }
    
    /**
     * overrides the parent's createServiceUnitManager to create component 
     * specific service unit manager
     */
    @Override
    protected ServiceUnitManager createServiceUnitManager() {
        return new CamelSESUManager(this);
    }
    
}
