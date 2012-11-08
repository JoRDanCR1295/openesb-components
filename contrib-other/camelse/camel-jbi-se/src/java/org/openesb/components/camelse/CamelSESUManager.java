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
 * @(#)CamelSESUManager.java 
 *
 * Copyright 2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package org.openesb.components.camelse;

import org.openesb.components.camelse.common.RuntimeHelper;
import org.openesb.components.camelse.common.deployment.AbstractServiceUnitManager;
import org.openesb.components.camelse.common.deployment.ServiceUnit;
import java.util.logging.Logger;
import javax.jbi.management.DeploymentException;

/**
 * This class extends the AbstractServiceUnitManager to implement component specific 
 * service unit manager by creating the component specific ServiceUnit implementation.
 * @see com.sun.jbi.sample.component.common.deployment.AbstractServiceUnitManager
 * @see com.sun.jbi.sample.component.common.deployment.ServiceUnit
 * @author chikkala
 */
public class CamelSESUManager extends AbstractServiceUnitManager {
    
    private CamelSERuntime mRuntime;
    
    /** Creates a new instance of CamelSESUManager */
    public CamelSESUManager(CamelSERuntime compRuntime) {
        super();
        this.mRuntime = compRuntime;
    }
    protected Logger getLogger() {
        return RuntimeHelper.getLogger();
    }
    
    protected String getComponentName() {
        return RuntimeHelper.getComponentName();
    }    
    //TODO implement CamelSEServiceUnit
    protected ServiceUnit createServiceUnit(String suName, String suRootPath) throws DeploymentException {
        return new CamelSEServiceUnit(suName, suRootPath, this.mRuntime);
    }    
    
}
