/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AsteriskServiceUnit.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk;

import com.sun.jbi.sample.component.common.deployment.ConsumerEndpoint;
import com.sun.jbi.sample.component.common.deployment.ProviderEndpoint;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor.Consumes;
import com.sun.jbi.sample.component.common.deployment.SUDescriptor.Provides;
import com.sun.jbi.sample.component.common.deployment.ServiceUnit;
import com.sun.jbi.sample.component.common.wsdl.WSDLProcessor;
import javax.jbi.management.DeploymentException;
import javax.wsdl.Definition;

public class AsteriskServiceUnit extends ServiceUnit {
    
    /** Creates a new instance of ProviderSEServiceUnit */
    public AsteriskServiceUnit(String suName, String suRootPath) {
        super(suName, suRootPath);
    }
    
    @Override
    protected WSDLProcessor createWSDLProcessor() {
        return new AsteriskWSDLProcessor(this.getSURootPath());
    }
    
    @Override
    protected ConsumerEndpoint createConsumerEndpoint(Consumes consumes, Definition wsdlDef) {
        return new AsteriskConsumerEndpoint(consumes, wsdlDef, this);
    }
    
    @Override
    protected ProviderEndpoint createProviderEndpoint(Provides provides, Definition wsdlDef) {
        return new AsteriskProviderEndpoint(provides, wsdlDef, this);
    }
    
    @Override
    protected void loadOtherArtifacts() throws DeploymentException {
        //TODO: load any component specific service unit artifacts
    }
    
}
