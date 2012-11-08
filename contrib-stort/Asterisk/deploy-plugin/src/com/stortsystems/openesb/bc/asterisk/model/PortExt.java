/*
 * Asterisk JBI Binding Component.
 * Copyright (C) 2007 Stort Systems.
 * www.stortsystems.com
 * 
 * This library is distributed under the CDDL license.
 * 
 * $Id: PortExt.java,v 1.1 2008/01/20 16:40:08 tiago_cury Exp $
 */

package com.stortsystems.openesb.bc.asterisk.model;

import org.netbeans.modules.xml.wsdl.model.Port;
import org.netbeans.modules.xml.wsdl.model.WSDLModel;
import org.netbeans.modules.xml.xam.Component;
import org.w3c.dom.Element;
import org.w3c.dom.Element;

public interface PortExt extends ExtComponent {
    
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
     * for service port extensibility element.
     */
    public static class PortExtImpl extends ExtModelImpl implements PortExt {
        
        public PortExtImpl(WSDLModel model, Element e) {
            super(model, e);
        }
        
        public PortExtImpl(WSDLModel model) {
            this(model, createPrefixedElement(QN_PORT_EXT, model));
        }
        
        public void accept(ExtVisitor visitor) {
            visitor.visit(this);
        }
        
        @Override
        public boolean canBeAddedTo(Component target) {
            if (target instanceof Port) {
                return true;
            }
            return false;
        }
        
        public String getAddress() {
            return getAttribute(ExtAttribute.ADDRESS);
        }
        
        public void setAddress(String address) {
            setAttribute(ATTR_ADDRESS, ExtAttribute.ADDRESS, address);
        }
        
        public String getUsername() {
            return getAttribute(ExtAttribute.USERNAME);
        }
        
        public void setUsername(String username) {
            setAttribute(ATTR_USERNAME, ExtAttribute.USERNAME, username);
        }
        
        public String getPassword() {
            return getAttribute(ExtAttribute.PASSWORD);
        }
        
        public void setPassword(String password) {
            setAttribute(ATTR_PASSWORD, ExtAttribute.PASSWORD, password);
        }
        
        public String getPort() {
            return getAttribute(ExtAttribute.MBEAN);
        }
        
        public void setPort(String port) {
            setAttribute(ATTR_PORT, ExtAttribute.MBEAN, port);
        }
    }
}
