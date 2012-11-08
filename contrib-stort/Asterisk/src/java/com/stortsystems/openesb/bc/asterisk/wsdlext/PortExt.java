/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: PortExt.java,v 1.1 2008/01/20 16:38:50 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.wsdlext;

import com.sun.jbi.sample.component.common.wsdl.AbstractExtensibilityElement;
import com.sun.jbi.sample.component.common.wsdl.AbstractExtensionSerializer;
import com.sun.jbi.sample.component.common.wsdl.WSDLProcessor;
import java.io.PrintWriter;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;

public interface PortExt extends ExtConstants, ExtensibilityElement, java.io.Serializable {
    /**
     * Getter for property serviceURL.
     * @return Value of property serviceURL.
     */
    public String getAddress();
    
    /**
     * Setter for property serviceURL.
     * @param serviceURL New value of property serviceURL.
     */
    public void setAddress(String address);
    
    /**
     * Getter for property username.
     * @return Value of property username.
     */
    public String getUsername();
    
    /**
     * Setter for property username.
     * @param username New value of property username.
     */
    public void setUsername(String username);
    
    /**
     * Getter for property password.
     * @return Value of property password.
     */
    public String getPassword();
    
    /**
     * Setter for property password.
     * @param password New value of property password.
     */
    public void setPassword(String password);
    
    /**
     * Getter for property name.
     * @return Value of property name.
     */
    public String getPort();
    
    /**
     * Setter for property mbean.
     * @param name New value of property mbean.
     */
    public void setPort(String port);
    
    /**
     * This class is an implementation of PortExt interface that provides java model
     * for port extensibility element.
     */
    public static class PortExtImpl
            extends AbstractExtensibilityElement implements PortExt {
        
        private String mAddress;
        private String mUsername;
        private String mPassword;
        private String mPort;
        
        public PortExtImpl() {
            setElementType(QN_PORT_EXT);
        }
        
        public String getAddress() {
            return this.mAddress;
        }
        
        public void setAddress(String address) {
            this.mAddress = address;
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
        
        public String getPort() {
            return this.mPort;
        }
        
        public void setPort(String port) {
            this.mPort = port;
        }
        
        /**
         * creates and adds the port extensibility element to the wsdl definition
         */
        public static PortExtImpl addExtensibilityElement(Definition wsdlDef, Port port) {
            PortExtImpl  portExt = new PortExt.PortExtImpl();
            WSDLProcessor.addExtensibilityElement(wsdlDef, port, portExt, NS_DEF_PREFIX);
            return portExt;
        }
        
    }
    /**
     * This class provides the serializer and deserializer implementation for port extensibility element.
     */
    public static class PortExtSerializer extends AbstractExtensionSerializer implements ExtConstants {
        public PortExtSerializer() {
            super(Port.class, QN_PORT_EXT, PortExtImpl.class);
        }
        @Override
        public ExtensibilityElement unmarshall(Class parentType, QName elementType, Element el,
                Definition def, ExtensionRegistry extReg) throws WSDLException {
            
            PortExt extObj = (PortExt)extReg.createExtension(parentType, elementType);
            
            String address = el.getAttribute(ATTR_ADDRESS);
            extObj.setAddress(address);
            
            String port = el.getAttribute(ATTR_PORT);
            extObj.setPort(port);
            
            String username = el.getAttribute(ATTR_USERNAME);
            extObj.setUsername(username);
            
            String password = el.getAttribute(ATTR_PASSWORD);
            extObj.setPassword(password);
            
            return extObj;
        }
        
        @Override
        public void marshall(Class parentType, QName elementType, ExtensibilityElement extension,
                PrintWriter pw, Definition def, ExtensionRegistry extReg ) throws WSDLException {
            
            PortExt extObj = (PortExt)extension;
            StringBuffer buff = new StringBuffer();
            
            String elName = getQualifiedName(def, NS_URI, EL_PORT_EXT);
            buff.append("<" + elName );
            
            String address = extObj.getAddress();
            if ( address != null && address.trim().length() > 0 ) {
                buff.append(" " + ATTR_ADDRESS + "=\"" + address + "\"");
            }
            
            String port = extObj.getPort();
            if ( port != null && port.trim().length() > 0 ) {
                buff.append(" " + ATTR_PORT + "=\"" + port + "\"");
            }
            
            String username = extObj.getUsername();
            if ( username != null && username.trim().length() > 0 ) {
                buff.append(" " + ATTR_USERNAME + "=\"" + username + "\"");
            }
            
            String password = extObj.getUsername();
            if ( password != null && password.trim().length() > 0 ) {
                buff.append(" " + ATTR_PASSWORD + "=\"" + password + "\"");
            }
            
            buff.append("/>");
            pw.println(buff.toString());
            
        }
        
    }
}
