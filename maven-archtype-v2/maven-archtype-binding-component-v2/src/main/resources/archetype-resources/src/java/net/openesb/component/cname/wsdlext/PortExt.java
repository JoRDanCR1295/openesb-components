#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * PortExt.java
 */
package net.openesb.component.${artifactId}.wsdlext;

import net.openesb.component.${artifactId}.common.wsdl.AbstractExtensibilityElement;
import net.openesb.component.${artifactId}.common.wsdl.AbstractExtensionSerializer;
import net.openesb.component.${artifactId}.common.wsdl.WSDLProcessor;
import java.io.PrintWriter;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

/**
 * This interface and its implementation represents the java model for port
 * extension element. The inner classes PortExtImpl implements the interface and
 * the PortExtSerializer provides the serializer and deserializer implemenation.
 * The implementation and serializer classes will be registered with the
 * ExtensionRegistry to process the port extension element in the wsdl
 * definition.
 *
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensibilityElement
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer
 * @see com.sun.jbi.sample.component.common.wsdl.AbstractExtensionRegistry
 * @author chikkala
 */
public interface PortExt extends ExtConstants, ExtensibilityElement, java.io.Serializable {

    /**
     * Getter for property serviceURL.
     *
     * @return Value of property serviceURL.
     */
    public String getServiceURL();

    /**
     * Setter for property serviceURL.
     *
     * @param serviceURL New value of property serviceURL.
     */
    public void setServiceURL(String serviceURL);

    /**
     * Getter for property username.
     *
     * @return Value of property username.
     */
    public String getUsername();

    /**
     * Setter for property username.
     *
     * @param username New value of property username.
     */
    public void setUsername(String username);

    /**
     * Getter for property password.
     *
     * @return Value of property password.
     */
    public String getPassword();

    /**
     * Setter for property password.
     *
     * @param password New value of property password.
     */
    public void setPassword(String password);

    /**
     * Getter for property name.
     *
     * @return Value of property name.
     */
    public String getMBean();

    /**
     * Setter for property mbean.
     *
     * @param name New value of property mbean.
     */
    public void setMBean(String mbean);

    /**
     * This class is an implementation of PortExt interface that provides java
     * model for port extensibility element.
     */
    public static class PortExtImpl
            extends AbstractExtensibilityElement implements PortExt {
        
        private String mServiceURL;
        private String mUsername;
        private String mPassword;
        private String mMBean;
        
        public PortExtImpl() {
            setElementType(QN_PORT_EXT);
        }
        
        public String getServiceURL() {
            return this.mServiceURL;
        }
        
        public void setServiceURL(String serviceURL) {
            this.mServiceURL = serviceURL;
        }
        
        public String getUsername() {
            return this.mUsername;
        }
        
        public void setUsername(String username) {
            this.mUsername = username;
        }
        
        public String getPassword() {
            return this.mPassword;
        }
        
        public void setPassword(String password) {
            this.mPassword = password;
        }
        
        public String getMBean() {
            return this.mMBean;
        }
        
        public void setMBean(String mbean) {
            this.mMBean = mbean;
        }

        /**
         * creates and adds the port extensibility element to the wsdl
         * definition
         */
        public static PortExtImpl addExtensibilityElement(Definition wsdlDef, Port port) {
            PortExtImpl portExt = new PortExt.PortExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, port, portExt, NS_DEF_PREFIX);
            return portExt;
        }
    }

    /**
     * This class provides the serializer and deserializer implementation for
     * port extensibility element.
     */
    public static class PortExtSerializer extends AbstractExtensionSerializer implements ExtConstants {

        public PortExtSerializer() {
            super(Port.class, QN_PORT_EXT, PortExtImpl.class);
        }

        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
                Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            PortExt extObj = (PortExt) extReg.createExtension(parentType, elementType);
            
            String serviceURL = el.getAttribute(ATTR_SERVICE_URL);
            extObj.setServiceURL(serviceURL);
            
            String mbean = el.getAttribute(ATTR_MBEAN);
            extObj.setMBean(mbean);
            
            String username = el.getAttribute(ATTR_USERNAME);
            extObj.setUsername(username);
            
            String password = el.getAttribute(ATTR_PASSWORD);
            extObj.setPassword(password);
            
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
                PrintWriter pw, Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            PortExt extObj = (PortExt) extension;
            StringBuffer buff = new StringBuffer();
            
            String elName = getQualifiedName(def, NS_URI, EL_PORT_EXT);
            buff.append("<" + elName);
            
            String serviceURL = extObj.getServiceURL();
            if (serviceURL != null && serviceURL.trim().length() > 0) {
                buff.append(" " + ATTR_SERVICE_URL + "=${symbol_escape}"" + serviceURL + "${symbol_escape}"");
            }
            
            String mbean = extObj.getMBean();
            if (mbean != null && mbean.trim().length() > 0) {
                buff.append(" " + ATTR_MBEAN + "=${symbol_escape}"" + mbean + "${symbol_escape}"");
            }
            
            String username = extObj.getUsername();
            if (username != null && username.trim().length() > 0) {
                buff.append(" " + ATTR_USERNAME + "=${symbol_escape}"" + username + "${symbol_escape}"");
            }
            
            String password = extObj.getUsername();
            if (password != null && password.trim().length() > 0) {
                buff.append(" " + ATTR_PASSWORD + "=${symbol_escape}"" + password + "${symbol_escape}"");
            }
            
            buff.append("/>");
            pw.println(buff.toString());
            
        }
    }
}
