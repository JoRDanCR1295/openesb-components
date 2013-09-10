/*
 * AbstractExtensionRegistry.java
 */

package net.openesb.component.ServiceEngine-archetype.common.wsdl;

import java.util.List;
import javax.wsdl.extensions.ExtensionRegistry;

/**
 * This is the abstract class used to add the wsdl extension serializers and deserializers to the
 * wsdl extension registry configured to read/write wsdl extensions into a know java model.
 * @see AbstractExtensionSerializer
 * @see WSDLProcessor
 * @author chikkala
 */
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
        // register ServiceEngine-archetype Binding serializers
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
