#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * JMXBindingServiceUnit.java
 *
 */
package net.openesb.component.${artifactId};

import net.openesb.component.${artifactId}.common.deployment.ConsumerEndpoint;
import net.openesb.component.${artifactId}.common.deployment.ProviderEndpoint;
import net.openesb.component.${artifactId}.common.deployment.SUDescriptor.Consumes;
import net.openesb.component.${artifactId}.common.deployment.SUDescriptor.Provides;
import net.openesb.component.${artifactId}.common.deployment.ServiceUnit;
import net.openesb.component.${artifactId}.common.wsdl.WSDLProcessor;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;

/**
 * This class extends the ServiceUnit to implement the component specific
 * service unit processing. It creates the component specific ProviderEndpoint
 * and ConsumerEndpoint implementation to configure the service provider and
 * consumer endpoints on this component deployed in the service unit.
 *
 * @author chikkala
 */
public class JMXBindingServiceUnit extends ServiceUnit {

    /**
     * Creates a new instance of ProviderSEServiceUnit
     */
    public JMXBindingServiceUnit(String suName, String suRootPath) {
        super(suName, suRootPath);
    }
    
    @Override
    protected WSDLProcessor createWSDLProcessor() {
        return new JMXBindingWSDLProcessor(this.getSURootPath());
    }
    
    @Override
    protected ConsumerEndpoint createConsumerEndpoint(Consumes consumes, Definition wsdlDef) {
        return new JMXBindingConsumerEndpoint(consumes, wsdlDef, this);
    }
    
    @Override
    protected ProviderEndpoint createProviderEndpoint(Provides provides, Definition wsdlDef) {
        return new JMXBindingProviderEndpoint(provides, wsdlDef, this);
    }
    
    @Override
    protected void loadOtherArtifacts() throws DeploymentException {
        //TODO: load any component specific service unit artifacts
    }
}
