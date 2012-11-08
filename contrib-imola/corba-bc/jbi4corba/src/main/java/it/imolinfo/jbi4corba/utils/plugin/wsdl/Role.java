/*
 * Role.java
 * 
 * Created on 12-set-2007, 17.29.28
 * 
 * This class represents the Role element
 */

package it.imolinfo.jbi4corba.utils.plugin.wsdl;

import java.io.Serializable;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 *
 * @author mcastaldi
 */
public class Role implements Serializable {
    
    private String name = null;
    private String portType = null;

    public Role() {
    }

    public Role(String name, String portType) {
        this.name = name;
        this.portType = portType;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPortType() {
        return portType;
    }

    public void setPortType(String portType) {
        this.portType = portType;
    }

    /**
    * Not used ... so not tested :-)
    */
    public static Role parse(Node roleElement) {
        Role role = null;
        // check if this is a correct element
        if (Jbi4CorbaPartnerLinkExtension.ROLE_ELEMENT.equals(roleElement.getNodeName())) 
        {
            // the provided element is correct
            role = new Role();
            NamedNodeMap attributes = roleElement.getAttributes();
            Node node = attributes.getNamedItem(Jbi4CorbaPartnerLinkExtension.ROLE_ATTRIBUTE_NAME);
            role.setName(node.getNodeValue());
            node = attributes.getNamedItem(Jbi4CorbaPartnerLinkExtension.ROLE_ATTRIBUTE_PORT);
            role.setPortType(node.getNodeValue());
        }
        return role;
    }

    /**
    * Not used ... so not tested :-)
    */
    public String toString() {
        StringBuffer buffer = new StringBuffer("Role: \n");
        buffer.append("name: ").append(this.getName()).append("\n");
        buffer.append("portType: ").append(this.getPortType()).append("\n");
        return buffer.toString();
    }
    
}
