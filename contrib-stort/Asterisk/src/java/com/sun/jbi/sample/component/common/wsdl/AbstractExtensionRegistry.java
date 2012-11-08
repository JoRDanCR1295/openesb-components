/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: AbstractExtensionRegistry.java,v 1.2 2008/01/27 20:59:41 tiago_cury Exp $
 */

package com.sun.jbi.sample.component.common.wsdl;

import java.util.List;
import javax.wsdl.extensions.ExtensionRegistry;

public abstract class AbstractExtensionRegistry extends ExtensionRegistry {
    
    /** Creates a new instance of AbstractSerializer
     * it calls the <code>createSerializers<code> method to get the list of
     * serializers and then call registerSerializer on each AbstractExtensionSerializer
     * to register them with this registry.
     * @see AbstractExtensionSerializer#registerSerializer
     */
    protected AbstractExtensionRegistry() {
        super();
        List<AbstractExtensionSerializer> list = createSerializers();
        for (AbstractExtensionSerializer ser : list ) {
            ser.registerSerializer(this);
        }
        // register ServiceEngine Binding serializers
        AbstractExtensionSerializer seBindingSer = new SEBindingExt.SEBindingExtSerializer();
        seBindingSer.registerSerializer(this);
    }
    /**
     * create wsdl extension serializers for each extension element to register with the extension
     * registry.
     * @return List of AbstractExtensionSerializer objects for serializing/deserializing the wsdl extensions.
     * @see AbstractExtensionSerializer
     */
    protected abstract List<AbstractExtensionSerializer> createSerializers();
}
