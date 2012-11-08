/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.wsdl;

import java.util.Enumeration;
import java.util.Properties;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;

import com.ibm.wsdl.util.xml.DOMUtils;

/**
 * Serializer for the Jbi4Corba WSDL Extension (addess element), according with
 * JWSDL specs. See JSR 110.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4CorbaAddressSerializer implements ExtensionSerializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG = LoggerFactory
            .getLogger(Jbi4CorbaAddressSerializer.class);

    /**
     * Default constructor.
     */
    public Jbi4CorbaAddressSerializer() {
    }

    /**
     * @param parentType
     *            The parent type
     * @param elementType
     *            The element tipe
     * @param extension
     *            The extension
     * @param pw
     *            The io print writer
     * @param def
     *            The definition
     * @param extReg
     *            The extension registry
     * @throws WSDLException
     *             The WSDL exception
     */
    @SuppressWarnings("unchecked")
    public void marshall(Class parentType, QName elementType,
            ExtensibilityElement extension, java.io.PrintWriter pw,
            Definition def, ExtensionRegistry extReg) throws WSDLException {

        // Gets the QN prefix
        String prefix = DOMUtils.getPrefix(Jbi4CorbaExtension.NS_URI_JBI4CORBA,
                def);
        prefix += ":";
        LOG.debug("prefix found: " + prefix);

        // If prefix is null, adds it
        if (prefix == null) {
            prefix = Jbi4CorbaExtension.DEFAULT_PREFIX;
            // Adds the namespace
            def.addNamespace(Jbi4CorbaExtension.DEFAULT_PREFIX,
                    Jbi4CorbaExtension.NS_URI_JBI4CORBA);
        }

        if (extension instanceof Jbi4CorbaAddress) {
            Jbi4CorbaAddress jbi4CorbaAddress = (Jbi4CorbaAddress) extension;

            pw.print("<" + prefix + Jbi4CorbaExtension.ADDRESS_ELEMENT);
            DOMUtils.printAttribute(Jbi4CorbaExtension.NAME_ATTRIBUTE,
                    jbi4CorbaAddress.getName(), pw);
            DOMUtils.printAttribute(
                    Jbi4CorbaExtension.LOCALIZATION_TYPE_ATTRIBUTE,
                    jbi4CorbaAddress.getLocalizationType(), pw);

            pw.print(">\n");

            // ORB Properties
            printORBElement(jbi4CorbaAddress, pw, prefix);

            pw
                    .print("</" + prefix + Jbi4CorbaExtension.ADDRESS_ELEMENT
                            + ">\n");
        } else {
            LOG.warn("CRB000300_The_extension_element_is_not_a",
                    Jbi4CorbaExtension.ADDRESS_ELEMENT);
        }

    }

    /**
     * Prints the ORB element.
     * 
     * @param jbi4CorbaBinding
     *            The jbi4Corba binding
     * @param pw
     *            The print writer
     * @param prefix
     *            The prefix
     */
    private void printORBElement(Jbi4CorbaAddress jbi4CorbaAddress,
            java.io.PrintWriter pw, String prefix) {
        if (jbi4CorbaAddress.getOrbProperties() != null) {
            pw.print("<" + prefix + Jbi4CorbaExtension.ORB_ELEMENT + ">\n");
            Properties orbProperties = jbi4CorbaAddress.getOrbProperties();
            Enumeration<Object> enKeys = orbProperties.keys();
            for (; enKeys.hasMoreElements();) {
                String key = (String) enKeys.nextElement();
                String prop = (String) orbProperties.get(key);
                pw.print("<" + prefix + Jbi4CorbaExtension.PROPERTY_ELEMENT);
                DOMUtils.printAttribute(Jbi4CorbaExtension.NAME_ATTRIBUTE, key,
                        pw);
                DOMUtils.printAttribute(Jbi4CorbaExtension.VALUE_ATTRIBUTE,
                        prop, pw);
                pw.print("/>\n");
            }

            pw.print("</" + prefix + Jbi4CorbaExtension.ORB_ELEMENT + ">\n");
        }
    }

}
