package com.sun.jbi.restbc.jbiadapter.wsdl;

import javax.wsdl.extensions.ExtensionRegistry;

/**
 * RestExtensionRegistry.java
 *
 * @author Edward Chou
 */
public class RestExtensionRegistry extends ExtensionRegistry {

    private static final long serialVersionUID = 1L;    
    
    public RestExtensionRegistry() {
        super();
        RestExtSerializer extSerializer = new RestExtSerializer();
        extSerializer.registerSerializer(this);
    }        
}
