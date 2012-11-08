/*
 * @(#)ExtensionRegistrySupport.java        $Revision: 1.1 $ $Date: 2008/11/24 12:49:27 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.shared.wsdl.extension;

import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensionRegistry;

import org.openesb.components.rules4jbi.shared.wsdl.extension.bpel.PartnerLinkExtensibilityElement;
import org.openesb.components.rules4jbi.shared.wsdl.extension.bpel.PartnerLinkExtensionDeserializer;
import org.openesb.components.rules4jbi.shared.wsdl.extension.bpel.PartnerLinkExtensionSerializer;
import org.openesb.components.rules4jbi.shared.wsdl.extension.jbi.ServiceEngineExtensibilityElement;
import org.openesb.components.rules4jbi.shared.wsdl.extension.jbi.ServiceEngineExtensionDeserializer;
import org.openesb.components.rules4jbi.shared.wsdl.extension.jbi.ServiceEngineExtensionSerializer;

/**
 * Allows to populate an <code>ExtensionRegistry</code> with the service engine binding and bpel extensions.
 * <p>
 * Note that a configured <code>ExtensionRegistry</code> should be set on the <code>WSDLReader</code>
 * prior to reading the document and it should be set on the <code>Definition</code>
 * prior to handing it off to a <code>WSDLWriter</code>. <code>Definition</code>s read (constructed) by
 * a <code>WSDLReader</code> will have an <code>ExtensionRegistry</code> of that <code>WSDLReader</code>.
 * 
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.1 $ $Date: 2008/11/24 12:49:27 $
 * 
 * @see javax.wsdl.extensions.ExtensionRegistry
 * @since 0.1
 */
public final class ExtensionRegistrySupport {

    private ExtensionRegistrySupport() {}
    
    public static void registerExtensions(ExtensionRegistry registry) {
        
        registry.registerDeserializer(Binding.class,
                ServiceEngineExtensibilityElement.QUALIFIED_NAME,
                new ServiceEngineExtensionDeserializer());

        registry.registerSerializer(Binding.class,
                ServiceEngineExtensibilityElement.QUALIFIED_NAME,
                new ServiceEngineExtensionSerializer());
        
        registry.mapExtensionTypes(Binding.class,
                ServiceEngineExtensibilityElement.QUALIFIED_NAME,
                ServiceEngineExtensibilityElement.class);
        
        registry.registerDeserializer(Definition.class,
                PartnerLinkExtensibilityElement.QUALIFIED_NAME,
                new PartnerLinkExtensionDeserializer());

        registry.registerSerializer(Definition.class,
                PartnerLinkExtensibilityElement.QUALIFIED_NAME,
                new PartnerLinkExtensionSerializer());
        
        registry.mapExtensionTypes(Definition.class,
                PartnerLinkExtensibilityElement.QUALIFIED_NAME,
                PartnerLinkExtensibilityElement.class);
    }
}
