/*
 * Jbi4CorbaPartnerLinkSerializer.java
 * 
 * Created on 13-set-2007, 8.45.51
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package it.imolinfo.jbi4corba.utils.plugin.wsdl;

import com.ibm.wsdl.util.xml.DOMUtils;
import java.io.PrintWriter;
import java.util.logging.Logger;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;


/**
 *
 * @author mcastaldi
 */
public class Jbi4CorbaPartnerLinkSerializer implements ExtensionSerializer {

    private Logger log = Logger.getLogger(this.getClass().getName());

    public void marshall(
        Class parentType, 
        QName elementType, 
        ExtensibilityElement extension, 
        PrintWriter pw, 
        Definition definition, 
        ExtensionRegistry registry) 
    throws WSDLException 
    {
        String prefix = null;
        Jbi4CorbaPartnerLink ptnlnk = null;
        
        // check if the type is correct
        if (extension instanceof Jbi4CorbaPartnerLink) {
            ptnlnk = (Jbi4CorbaPartnerLink)extension;
            prefix = ptnlnk.getPrefix();

            log.fine("Used prefix: "+prefix);

            // the element has a correct type
            pw.print("<" + prefix +":"+ Jbi4CorbaPartnerLinkExtension.PRTLNK_ELEMENT);
            DOMUtils.printAttribute(
                Jbi4CorbaPartnerLinkExtension.PRTLNK_NAME_ATTRIBUTE,
                ptnlnk.getName(),
                pw);
            pw.println(">");

            Role role = null;
            for (int i = 0; i < ptnlnk.getRolesNumber(); i++) {
                role = ptnlnk.getRole(0);
                printRoleElement(prefix, role, pw);
            }

            pw.println("</" + prefix +":"+ Jbi4CorbaPartnerLinkExtension.PRTLNK_ELEMENT+">");
        }
    }

    /**
    * The method prints the role element on the provided Writer.
    */
    private void printRoleElement(String prefix, Role role, PrintWriter pw) 
    {
        pw.print("<"+prefix+":");
        pw.print(Jbi4CorbaPartnerLinkExtension.ROLE_ELEMENT);
        DOMUtils.printAttribute(
            Jbi4CorbaPartnerLinkExtension.ROLE_ATTRIBUTE_NAME, 
            role.getName(), 
            pw);
            
        DOMUtils.printAttribute(
            Jbi4CorbaPartnerLinkExtension.ROLE_ATTRIBUTE_PORT, 
            role.getPortType(), 
            pw);

        pw.println("/>");
    }

}
