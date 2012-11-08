/*
 * JMXBindingSUManager.java
 *
 */

package com.sun.jbi.sample.binding;

import com.sun.jbi.sample.component.common.RuntimeHelper;
import com.sun.jbi.sample.component.common.deployment.AbstractServiceUnitManager;
import com.sun.jbi.sample.component.common.deployment.ServiceUnit;
import java.util.logging.Logger;
import javax.jbi.management.DeploymentException;

/**
 * This class extends the AbstractServiceUnitManager to implement component specific 
 * service unit manager by creating the component specific ServiceUnit implementation.
 * @see com.sun.jbi.sample.component.common.deployment.AbstractServiceUnitManager
 * @see com.sun.jbi.sample.component.common.deployment.ServiceUnit
 *
 * @author chikkala
 */
public class JMXBindingSUManager extends AbstractServiceUnitManager {
    private JMXBindingRuntime mRuntime;
    
    /** Creates a new instance of JMXBindingSUManager */
    public JMXBindingSUManager(JMXBindingRuntime compRuntime) {
        super();
        this.mRuntime = compRuntime;
    }
    
    protected Logger getLogger() {
        return RuntimeHelper.getLogger();
    }
    
    protected String getComponentName() {
        return RuntimeHelper.getComponentName();
    }
    
    //TODO implement ProviderSEServiceUnit
    protected ServiceUnit createServiceUnit(String suName, String suRootPath) throws DeploymentException {
        return new JMXBindingServiceUnit(suName, suRootPath);
    }
    
}
