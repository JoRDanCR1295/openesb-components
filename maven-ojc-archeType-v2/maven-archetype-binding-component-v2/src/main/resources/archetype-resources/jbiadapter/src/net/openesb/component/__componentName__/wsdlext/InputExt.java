#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * InputExt.java
 */
package net.openesb.component.${componentName}.wsdlext;

import net.openesb.component.${componentName}.common.wsdl.AbstractExtensibilityElement;
import net.openesb.component.${componentName}.common.wsdl.AbstractExtensionSerializer;
import net.openesb.component.${componentName}.common.wsdl.WSDLProcessor;
import java.io.PrintWriter;
import javax.wsdl.BindingInput;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

/**
 * This interface and its implementation represents the java model for binding
 * operation input extension element. The inner classes InputExtImpl implements
 * the interface and the InputExtSerializer provides the serislizer and
 * deserializer implementation. The implementation and serializer classes will
 * be registered with the ExtensionRegistry to process the binding operation
 * input extension element in the wsdl definition.
 *
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensibilityElement
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensionRegistry
 * @author chikkala
 */
public interface InputExt extends ExtConstants, ExtensibilityElement, java.io.Serializable {

    //TODO: define get/set methods for properties for InputExt if the extension element has attributes.
    /**
     * This class is an implementation of InputExt interface that provides java
     * model for binding operation input extensibility element.
     */    
    public static class InputExtImpl extends AbstractExtensibilityElement implements InputExt {
        
        public InputExtImpl() {
            setElementType(QN_INPUT_EXT);
        }

        /**
         * creates and adds the binding operation fault extensibility element to
         * the wsdl definition
         */
        public static InputExtImpl addExtensibilityElement(Definition wsdlDef, BindingInput input) {
            InputExtImpl inputExt = new InputExt.InputExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, input, inputExt, NS_DEF_PREFIX);
            return inputExt;
        }        
    }

    /**
     * This class provides the serializer and deserializer implementation for
     * binding operation input extensibility element.
     */    
    public static class InputExtSerializer extends AbstractExtensionSerializer implements ExtConstants {

        public InputExtSerializer() {
            super(BindingInput.class, QN_INPUT_EXT, InputExtImpl.class);
        }

        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
                Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            InputExt extObj = (InputExt) extReg.createExtension(parentType, elementType);
            //TODO: set any attributes from the el to extension object
            // String myAttr = el.getAttribute(ATTR_MY_ATTR);
            // extObj.setMyAttr(myAttr);
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
                PrintWriter pw, Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            InputExt extObj = (InputExt) extension;
            StringBuffer buff = new StringBuffer();
            
            String elName = getQualifiedName(def, NS_URI, EL_INPUT_EXT);
            buff.append("<" + elName);

            //TODO: append any attributes from extension obj to the element
            // String myAttr = extObj.getMyAttr();
            // if ( myAttr != null && myAttr.trim().length() > 0 ) {
            //     buff.append(" myAttr=${symbol_escape}"" + myAttr + "${symbol_escape}"");
            // }
            //
            
            buff.append("/>");
            pw.println(buff.toString());
            
        }
    }
}
