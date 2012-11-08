/*
 * JMXBindingWSDLProcessor.java
 *
 */

package com.sun.jbi.sample.binding;

import com.sun.jbi.sample.binding.wsdlext.WSDLExtensionRegistry;
import com.sun.jbi.sample.component.common.wsdl.WSDLProcessor;
import javax.wsdl.extensions.ExtensionRegistry;

/**
 * This class extends form the WSDLProcessor to configure the wsdl processor use the 
 * wsdl extension registry configured to process the binding specific extension elements
 * in the wsdl documents. 
 * 
 * WSDLExtensionRegistry that is created here has the wsdl extension model required to 
 * process the extension elements specific to the binding used by this binding component.
 *
 * @author chikkala
 */
public class JMXBindingWSDLProcessor extends WSDLProcessor {
    private WSDLExtensionRegistry mExtRegistry;
    /** Creates a new instance of JMXBindingWSDLProcessor */
    public JMXBindingWSDLProcessor(String wsdlDir) {
        super(wsdlDir);
    }
    
    @Override
    protected ExtensionRegistry getExtensionRegistry() {
        if ( this.mExtRegistry == null ) {
            this.mExtRegistry = new WSDLExtensionRegistry();
        }
        return this.mExtRegistry;
    }    
}
