/*
 * ProviderSEServiceUnit.java
 *
 */

package xacmlse;

import com.sun.jbi.sample.component.common.deployment.ServiceUnit;
import com.sun.jbi.sample.component.common.deployment.ProviderEndpoint;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor.Provides;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;

/**
 * This class extends the ServiceUnit to implement the component specific service unit processing.
 * It creates the component specific ProviderEndpoint implementation to configure the service
 * provider endpoint on this component deployed in the service unit. It also processes the deployment
 * artifacts that are specific to the service provided by the component.
 *
 * In this sample, this file is loading/using an XSLT file. So there is the locator that is specified
 * here. For reimaging this to another engine's purpose, we would define here how to obtain the other 
 * artifacts to be used. sblais 13 nov 2007.
 * @author chikkala
 */
public class ProviderSEServiceUnit extends ServiceUnit {
    
    private XACMLFileLocator mXACMLFileLocator;
    
    /** Creates a new instance of ProviderSEServiceUnit */
    public ProviderSEServiceUnit(String suName, String suRootPath) {
        super(suName, suRootPath);
    }
    protected XACMLFileLocator getXACMLFileLocator() {
        return this.mXACMLFileLocator;
    }
    @Override
    protected ProviderEndpoint createProviderEndpoint(Provides provides, Definition wsdlDef) {
        return new ProviderSEEndpoint(provides, wsdlDef, this);
    }
    /**
     * load and validates the component specific deployment artifacts. It loads the xsltmap
     * properties into XSLTFileLocator which will be used to find the xslt file corresponding
     * to a service operation.
     */
    @Override
    protected void loadOtherArtifacts() throws DeploymentException {
        super.loadOtherArtifacts();
        // load any component specific service unit artifacts
        // sblais, This is where we would load the policy files?
        // 13 nov 2007
        if ( this.mXACMLFileLocator == null) {
            try {
                this.mXACMLFileLocator =
                        new XACMLFileLocator(this.getSURootPath(), "xacmlmap.properties");
            } catch (Exception ex) {
                throw new DeploymentException(ex);
            }
        }
    }
    
}
