/*
 * JMXBindingServiceUnit.java
 *
 */

package com.sun.jbi.sample.binding;

import com.sun.jbi.sample.component.common.deployment.ConsumerEndpoint;
import com.sun.jbi.sample.component.common.deployment.ProviderEndpoint;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor.Consumes;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor.Provides;
import com.sun.jbi.sample.component.common.deployment.ServiceUnit;
import com.sun.jbi.sample.component.common.wsdl.WSDLProcessor;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;

/**
 * This class extends the ServiceUnit to implement the component specific service unit processing.
 * It creates the component specific ProviderEndpoint and ConsumerEndpoint implementation to
 * configure the service provider and consumer endpoints on this component deployed in the
 * service unit.
 *
 * @author chikkala
 */
public class JMXBindingServiceUnit extends ServiceUnit {
    
    /** Creates a new instance of ProviderSEServiceUnit */
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
