/*
 * SEBindingExt.java
 */

package org.openesb.components.camelse.common.wsdl;

import java.io.PrintWriter;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

/**
 * This interface and its implementation classes implement wsdl 1.1 extension model for the 
 * jbi service engine binding that is defined in jsr 208(sec. 5.5.6.1). to read/write concrete binding 
 * element in the wsdl definition for the service endpoint binding provided by the service engine. 
 * The wsdl definition with this binding information can be used in providing the service provider 
 * metadata returned by the service engine in <code>Component.getServiceDescription</code>.
 * <p>
 * <code>AbstractExtensionRegistry</code> by default include this extension serializers in the 
 * registry to read/write this service engine binding type in the wsdl definition.
 * <p>
 * During a service unit deployment/initialization in the service engine, the deployed wsdl definition
 * is loaded and modified to provide this binding for the service provided by the engine.
 *
 * @see WSDLProcessor#createServiceEngineBinding
 * @see AbstractExtensionRegistry
 * @see com.sun.jbi.sample.component.common.deployment.ServiceUnit#loadServiceDefinitions
 * @author chikkala
 */
public interface SEBindingExt  extends  ExtensibilityElement, java.io.Serializable {
    
    public static final String NS_URI = "http://java.sun.com/xml/ns/jbi/binding/service+engine";
    public static final String NS_DEF_PREFIX = "jbise";
    /** Element names. */
    public static final String EL_BINDING_EXT = "binding";
    /**Qualified element names.*/
    public static final QName QN_BINDING_EXT = new QName(NS_URI, EL_BINDING_EXT);
    
    public static class SEBindingExtImpl extends AbstractExtensibilityElement implements SEBindingExt {
        
        public SEBindingExtImpl() {
            setElementType(QN_BINDING_EXT);
        }
        @Override
        public String toString() {
            StringBuffer buff = new StringBuffer();
            buff.append("<"+NS_DEF_PREFIX+":"+EL_BINDING_EXT);
            buff.append("/>");
            return buff.toString();
        }
        /**
         * creates and adds the jbi service engine binding extensibility element to the wsdl definition
         * under specified binding definition.
         */
        public static SEBindingExtImpl addExtensibilityElement(Definition wsdlDef, Binding binding) {
            SEBindingExtImpl  bindingExt = new SEBindingExt.SEBindingExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, binding, bindingExt, SEBindingExt.NS_DEF_PREFIX);
            return bindingExt;
        }
    }
    /**
     * serializer and descrializer implementation for the binding extension element.
     */
    public static class SEBindingExtSerializer extends AbstractExtensionSerializer {
        
        public SEBindingExtSerializer() {
            super(Binding.class, QN_BINDING_EXT, SEBindingExtImpl.class);
        }
        
        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
            Definition def, ExtensionRegistry extReg) throws WSDLException {            
            SEBindingExt extObj = (SEBindingExt)extReg.createExtension(parentType, elementType);
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
            PrintWriter pw, Definition def, ExtensionRegistry extReg ) throws WSDLException {
            
            String elName = getQualifiedName(def, NS_URI, EL_BINDING_EXT);
            
            SEBindingExt extObj = (SEBindingExt)extension;
            
            StringBuffer buff = new StringBuffer();
            buff.append("<" + elName );
            buff.append("/>");
            pw.println(buff.toString());
        }
    }
    
}
