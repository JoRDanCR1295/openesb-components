#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * OperationExt.java
 */
package net.openesb.component.${componentName}.wsdlext;

import net.openesb.component.${componentName}.common.wsdl.AbstractExtensibilityElement;
import net.openesb.component.${componentName}.common.wsdl.AbstractExtensionSerializer;
import net.openesb.component.${componentName}.common.wsdl.WSDLProcessor;
import java.io.PrintWriter;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

/**
 * This interface and its implementation represents the java model for binding
 * operation extension element. The inner classes OperationImpl implements the
 * interface and the OperationExtSerializer provides the serilizer and
 * deserializer implementation. The implementation and serializer classes will
 * be registered with the ExtensionRegistry to process the binding operation
 * extension element in the wsdl definition.
 *
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensibilityElement
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensionRegistry
 * @author chikkala
 */
public interface OperationExt extends ExtConstants, ExtensibilityElement, java.io.Serializable {

    /**
     * Getter for property action.
     *
     * @return Value of property action.
     */
    public String getAction();

    /**
     * Setter for property action.
     *
     * @param action New value of property action.
     */
    public void setAction(String action);

    /**
     * This class is an implementation of OperationExt interface that provides
     * java model for binding operation extensibility element.
     */
    public static class OperationExtImpl extends AbstractExtensibilityElement implements OperationExt {
        
        private String mAction;
        
        public OperationExtImpl() {
            setElementType(QN_OPERATION_EXT);
        }
        
        public String getAction() {
            return this.mAction;
        }
        
        public void setAction(String action) {
            this.mAction = action;
        }

        /**
         * creates and adds the binding operation fault extensibility element to
         * the wsdl definition
         */
        public static OperationExtImpl addExtensibilityElement(Definition wsdlDef, BindingOperation operation) {
            OperationExtImpl operationExt = new OperationExt.OperationExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, operation, operationExt, NS_DEF_PREFIX);
            return operationExt;
        }
    }

    /**
     * This class provides the serializer and deserializer implementation for
     * binding operation extensibility element.
     */
    public static class OperationExtSerializer extends AbstractExtensionSerializer implements ExtConstants {

        public OperationExtSerializer() {
            super(BindingOperation.class, QN_OPERATION_EXT, OperationExtImpl.class);
        }

        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
                Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            OperationExt extObj = (OperationExt) extReg.createExtension(parentType, elementType);
            String action = el.getAttribute(ATTR_ACTION);
            extObj.setAction(action);
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
                PrintWriter pw, Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            OperationExt extObj = (OperationExt) extension;
            StringBuffer buff = new StringBuffer();
            
            String elName = getQualifiedName(def, NS_URI, EL_OPERATION_EXT);
            buff.append("<" + elName);
            
            String action = extObj.getAction();
            if (action != null && action.trim().length() > 0) {
                buff.append(" " + ATTR_ACTION + "=${symbol_escape}"" + action + "${symbol_escape}"");
            }
            
            buff.append("/>");
            pw.println(buff.toString());
            
        }
    }
}
