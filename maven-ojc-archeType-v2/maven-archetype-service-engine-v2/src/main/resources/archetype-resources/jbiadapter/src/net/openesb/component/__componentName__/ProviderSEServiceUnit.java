#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * ProviderSEServiceUnit.java
 *
 */
package net.openesb.component.${componentName};

import net.openesb.component.${componentName}.common.deployment.ServiceUnit;
import net.openesb.component.${componentName}.common.deployment.ProviderEndpoint;
import net.openesb.component.${componentName}.common.deployment.SUDescriptor.Provides;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;

/**
 * This class extends the ServiceUnit to implement the component specific
 * service unit processing. It creates the component specific ProviderEndpoint
 * implementation to configure the service provider endpoint on this component
 * deployed in the service unit. It also processes the deployment artifacts that
 * are specific to the service provided by the component.
 *
 * @author chikkala
 */
public class ProviderSEServiceUnit extends ServiceUnit {
    
    private XSLTFileLocator mXsltFileLocator;

    /**
     * Creates a new instance of ProviderSEServiceUnit
     */
    public ProviderSEServiceUnit(String suName, String suRootPath) {
        super(suName, suRootPath);
    }

    protected XSLTFileLocator getXsltFileLocator() {
        return this.mXsltFileLocator;
    }

    @Override
    protected ProviderEndpoint createProviderEndpoint(Provides provides, Definition wsdlDef) {
        return new ProviderSEEndpoint(provides, wsdlDef, this);
    }

    /**
     * load and validates the component specific deployment artifacts. It loads
     * the xsltmap properties into XSLTFileLocator which will be used to find
     * the xslt file corresponding to a service operation.
     */
    @Override
    protected void loadOtherArtifacts() throws DeploymentException {
        super.loadOtherArtifacts();
        // load any component specific service unit artifacts
        if (this.mXsltFileLocator == null) {
            try {
                this.mXsltFileLocator =
                        new XSLTFileLocator(this.getSURootPath(), "xsltmap.properties");
            } catch (Exception ex) {
                throw new DeploymentException(ex);
            }
        }
    }
}
