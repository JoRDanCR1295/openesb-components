#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * FaultExt.java
 */
package net.openesb.component.${artifactId}.wsdlext;

import net.openesb.component.${artifactId}.common.wsdl.AbstractExtensibilityElement;
import net.openesb.component.${artifactId}.common.wsdl.AbstractExtensionSerializer;
import net.openesb.component.${artifactId}.common.wsdl.WSDLProcessor;
import java.io.PrintWriter;
import javax.wsdl.BindingFault;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

/**
 * This interface and its implementation represents the java model for binding
 * operation fault extension element. The inner classes FaultExtImpl implements
 * the interface and the FaultExtSerializer provides the serializer and
 * deserializer implementation. The implemenation and serializer classes will be
 * registered with the ExtensionRegistry to process the binding operation fault
 * extension element in the wsdl definition.
 *
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensibilityElement
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensionRegistry
 * @author chikkala
 */
public interface FaultExt extends ExtConstants, ExtensibilityElement, java.io.Serializable {

    //TODO: define getter methods for properties for FaultExt if the extension element has attributes.
    /**
     * This class is an implementation of FaultExt interface that provides java
     * model for binding operation fault extensibility element.
     */    
    public static class FaultExtImpl extends AbstractExtensibilityElement implements FaultExt {

        public FaultExtImpl() {
            setElementType(QN_FAULT_EXT);
        }

        /**
         * creates and adds the binding operation fault extensibility element to
         * the wsdl definition
         */
        public static FaultExtImpl addExtensibilityElement(Definition wsdlDef, BindingFault fault) {
            FaultExtImpl faultExt = new FaultExt.FaultExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, fault, faultExt, NS_DEF_PREFIX);
            return faultExt;
        }        
    }

    /**
     * This class provides the serializer and deserializer implementation for
     * binding operation fault extensibility element.
     */    
    public static class FaultExtSerializer extends AbstractExtensionSerializer implements ExtConstants {

        public FaultExtSerializer() {
            super(BindingFault.class, QN_FAULT_EXT, FaultExtImpl.class);
        }
        
        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
                Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            FaultExt extObj = (FaultExt) extReg.createExtension(parentType, elementType);
            //TODO: set any attributes from the el to extension object
            // String myAttr = el.getAttribute(ATTR_MY_ATTR);
            // extObj.setMyAttr(myAttr);
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
                PrintWriter pw, Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            FaultExt extObj = (FaultExt) extension;
            StringBuffer buff = new StringBuffer();
            
            String elName = getQualifiedName(def, NS_URI, EL_FAULT_EXT);
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
