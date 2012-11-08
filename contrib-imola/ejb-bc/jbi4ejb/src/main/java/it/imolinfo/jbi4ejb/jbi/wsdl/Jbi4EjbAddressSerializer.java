/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.jbi.wsdl;

import it.imolinfo.jbi4ejb.Logger;
import it.imolinfo.jbi4ejb.LoggerFactory;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;

import com.ibm.wsdl.util.xml.DOMUtils;


/**
 * Serializer for the Jbi4Ejb WSDL Extension (addess element), according with
 * JWSDL specs.
 * See JSR 110.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbAddressSerializer implements ExtensionSerializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
    = LoggerFactory.getLogger(Jbi4EjbAddressSerializer.class);
    
    /**
     * Instantiates a new jbi4 ejb address serializer.
     */
    public Jbi4EjbAddressSerializer() {}

    /* (non-Javadoc)
     * @see javax.wsdl.extensions.ExtensionSerializer#marshall(java.lang.Class, javax.xml.namespace.QName, javax.wsdl.extensions.ExtensibilityElement, java.io.PrintWriter, javax.wsdl.Definition, javax.wsdl.extensions.ExtensionRegistry)
     */
    
    /**
     * Creates the WSDL fragment for the jbi4ejb address.
     * 
     * @param parentType the parent type
     * @param elementType the element type
     * @param extension the extension
     * @param pw the writer 
     * @param def the WSDL definition
     * @param extReg the extension registry
     * 
     * @throws WSDLException if some problem occurs in marshalling
     */
    @SuppressWarnings("unchecked")
    public void marshall(Class parentType, QName elementType,
            ExtensibilityElement extension, java.io.PrintWriter pw, Definition def, ExtensionRegistry extReg)
    throws WSDLException {

        // Gets the QN prefix
        String prefix = DOMUtils.getPrefix(Jbi4EjbExtension.NS_URI_JBI4EJB, def);
        prefix += ":";
        LOG.debug("prefix found: " + prefix);

        // If prefix is null, adds it
        if (prefix == null) {
            prefix = Jbi4EjbExtension.DEFAULT_PREFIX;
            // Adds the namespace
            def.addNamespace(Jbi4EjbExtension.DEFAULT_PREFIX, Jbi4EjbExtension.NS_URI_JBI4EJB);
        }

        if (extension instanceof Jbi4EjbAddress) {
            Jbi4EjbAddress jbi4EjbAddress = (Jbi4EjbAddress) extension;
            pw.print("<" + prefix + Jbi4EjbExtension.ADDRESS_ELEMENT);
            DOMUtils.printAttribute(Jbi4EjbExtension.NAME_ATTRIBUTE,jbi4EjbAddress.getName(), pw);
            DOMUtils.printAttribute(Jbi4EjbExtension.LOCALIZATION_TYPE_ATTRIBUTE,jbi4EjbAddress.getLocalizationType(), pw);
            pw.print("/>");
        } else {
        	LOG.warn("EJB000401_Error_in_extension_element", new Object[]{Jbi4EjbExtension.ADDRESS_ELEMENT});
        }
    }
}
