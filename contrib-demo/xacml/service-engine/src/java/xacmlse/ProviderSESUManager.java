/*
 * ProviderSESUManager.java
 */

package xacmlse;

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
 * @author chikkala
 */
public class ProviderSESUManager extends AbstractServiceUnitManager {
    
    private ProviderSERuntime mRuntime;
    
    /** Creates a new instance of ProviderSESUManager */
    public ProviderSESUManager(ProviderSERuntime compRuntime) {
        super();
        this.mRuntime = compRuntime;
    }
    @Override
    protected Logger getLogger() {
        return RuntimeHelper.getLogger();
    }
    
    @Override
    protected String getComponentName() {
        return RuntimeHelper.getComponentName();
    }
    //TODO implement ProviderSEServiceUnit
    @Override
    protected ServiceUnit createServiceUnit(String suName, String suRootPath) throws DeploymentException {
        return new ProviderSEServiceUnit(suName, suRootPath);
    }
    
}
