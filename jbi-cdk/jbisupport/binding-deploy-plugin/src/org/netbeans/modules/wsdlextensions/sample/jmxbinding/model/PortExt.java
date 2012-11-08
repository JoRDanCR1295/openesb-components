/*
 * PortExt.java
 */

package org.netbeans.modules.wsdlextensions.sample.jmxbinding.model;

import org.netbeans.modules.xml.wsdl.model.Port;
import org.netbeans.modules.xml.wsdl.model.WSDLModel;
import org.netbeans.modules.xml.xam.Component;
import org.w3c.dom.Element;
import org.w3c.dom.Element;

/**
 * This interface and its implementation represents the java model for service port
 * extension element.
 *
 * @author chikkala
 */
public interface PortExt extends ExtComponent {

    /**
     * Getter for property serviceURL.
     * @return Value of property serviceURL.
     */
    public String getServiceURL();
    
    /**
     * Setter for property serviceURL.
     * @param serviceURL New value of property serviceURL.
     */
    public void setServiceURL(String serviceURL);
    
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
    public String getMBean();
    
    /**
     * Setter for property mbean.
     * @param name New value of property mbean.
     */
    public void setMBean(String mbean);
    
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

        public String getServiceURL() {
            return getAttribute(ExtAttribute.SERVICE_URL);
        }

        public void setServiceURL(String serviceURL) {
            setAttribute(ATTR_SERVICE_URL, ExtAttribute.SERVICE_URL, serviceURL);
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

        public String getMBean() {
            return getAttribute(ExtAttribute.MBEAN);
        }

        public void setMBean(String mbean) {
            setAttribute(ATTR_MBEAN, ExtAttribute.MBEAN, mbean);
        }
    }
}
