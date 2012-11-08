/*
 * WSDLExtensionRegistry.java
 */

package com.sun.jbi.sample.binding.wsdlext;

import com.sun.jbi.sample.component.common.wsdl.AbstractExtensionRegistry;
import com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer;
import java.util.ArrayList;
import java.util.List;

/**
 * This class extends <code>AbstractExtensionRegistry<code> to create the serializers and deserializers
 * for processing wsdl extensions. 
 * @see AbstractExtensionRegistry
 * @author chikkala
 */
public class WSDLExtensionRegistry extends AbstractExtensionRegistry {
    
    /** Creates a new instance of JMXBindingExtensionRegistry */
    public WSDLExtensionRegistry() {
        super();
    }
    /**
     * creates serializers and deserializers for all the extension elements 
     */
    protected List<AbstractExtensionSerializer> createSerializers() {
        List<AbstractExtensionSerializer> list = new ArrayList<AbstractExtensionSerializer>();
        
        list.add(new BindingExt.BindingExtSerializer());
        list.add(new OperationExt.OperationExtSerializer());
        list.add(new InputExt.InputExtSerializer());
        list.add(new OutputExt.OutputExtSerializer());
        list.add(new FaultExt.FaultExtSerializer());
        list.add(new PortExt.PortExtSerializer());
        
        return list;
    }
}
