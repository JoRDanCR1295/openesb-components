/*
 * PortExt.java
 */

package org.netbeans.modules.wsdlextensions.sample.binding.model;

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
     * Getter for property Location.
     * @return Value of property Location.
     */
    public String getLocation();
    
    /**
     * Setter for property Location.
     * @param Location New value of property Location.
     */
    public void setLocation(String location);
        
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

        public String getLocation() {
            return getAttribute(ExtAttribute.LOCATION);
        }

        public void setLocation(String location) {
            setAttribute(ATTR_LOCATION, ExtAttribute.LOCATION, location);
        }
    }
}
