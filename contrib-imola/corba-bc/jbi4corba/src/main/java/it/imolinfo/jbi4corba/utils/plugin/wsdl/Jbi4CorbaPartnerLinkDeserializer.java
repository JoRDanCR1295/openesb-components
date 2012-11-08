/*
 * Jbi4CorbaPartnerLinkDeserializer.java
 * 
 * Created on 12-set-2007, 17.45.50
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package it.imolinfo.jbi4corba.utils.plugin.wsdl;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author mcastaldi
 */
public class Jbi4CorbaPartnerLinkDeserializer implements ExtensionDeserializer {

    /**
    * Not used ... so not tested :-)
    */
    public ExtensibilityElement unmarshall(
        Class parentType, 
        QName elementType, 
        Element element, 
        Definition definition, 
        ExtensionRegistry registry) 
    throws WSDLException
    {
        // create the object
        Jbi4CorbaPartnerLink ptlnk = (Jbi4CorbaPartnerLink)
            registry.createExtension(parentType, elementType);

        ptlnk.setName(
            element.getAttribute(Jbi4CorbaPartnerLinkExtension.PRTLNK_NAME_ATTRIBUTE));

        ptlnk.setPrefix(elementType.getPrefix());

        NodeList nodeList = element.getChildNodes();
        Node node = null;
        Role role = null;
        int nodeNumber = nodeList.getLength();
        if (nodeNumber < 1 | nodeNumber > 2) {
            // parsing error! 
            return null;
        }
        for (int i = 0; i < nodeList.getLength(); i++) {
            node = nodeList.item(i);
            role = Role.parse(node);
            if (role == null) {
                break;
            }
            ptlnk.addRole(role);
        }
        return ptlnk;
    }

}
