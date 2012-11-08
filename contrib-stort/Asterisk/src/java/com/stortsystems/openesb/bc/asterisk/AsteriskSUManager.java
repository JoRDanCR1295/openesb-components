/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AsteriskSUManager.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk;

import com.sun.jbi.sample.component.common.RuntimeHelper;
import com.sun.jbi.sample.component.common.deployment.AbstractServiceUnitManager;
import com.sun.jbi.sample.component.common.deployment.ServiceUnit;
import java.util.logging.Logger;
import javax.jbi.management.DeploymentException;

public class AsteriskSUManager extends AbstractServiceUnitManager {
    private AsteriskRuntime mRuntime;
    
    public AsteriskSUManager(AsteriskRuntime compRuntime) {
        super();
        this.mRuntime = compRuntime;
    }
    
    protected Logger getLogger() {
        return RuntimeHelper.getLogger();
    }
    
    protected String getComponentName() {
        return RuntimeHelper.getComponentName();
    }
    
    protected ServiceUnit createServiceUnit(String suName, String suRootPath) throws DeploymentException {
        return new AsteriskServiceUnit(suName, suRootPath);
    }
    
}
