/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: WSDLExtensionRegistry.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.wsdlext;

import com.sun.jbi.sample.component.common.wsdl.AbstractExtensionRegistry;
import com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer;
import java.util.ArrayList;
import java.util.List;

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
