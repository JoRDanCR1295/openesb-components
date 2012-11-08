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

import java.util.Enumeration;
import java.util.Properties;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;

import com.ibm.wsdl.util.xml.DOMUtils;


/**
 * Serializer for the Jbi4Ejb WSDL Extension (types element), according with JWSDL specs.
 * See JSR 110.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbTypesSerializer implements ExtensionSerializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4EjbTypesSerializer.class);
    
    /**
     * Instantiates a new jbi4 ejb types serializer.
     */
    public Jbi4EjbTypesSerializer() {}    


    /**
     * Marshall the Jbi4EJB extensibility element in a WSDL fragment.
     * 
     * @param parentType
     *            the parent type
     * @param elementType
     *            the element type
     * @param extension
     *            the estensibility element
     * @param pw
     *            the Writer
     * @param def
     *            the WSDL definition
     * @param extReg
     *            the extension registry
     * 
     * @throws WSDLException
     *             if some problem occurs in marshalling
     */
    public void marshall(Class parentType, QName elementType,
            ExtensibilityElement extension, java.io.PrintWriter pw, Definition def, ExtensionRegistry extReg)
    throws WSDLException {

        // Gets the QN prefix
        String prefix = DOMUtils.getPrefix(Jbi4EjbExtension.NS_URI_JBI4EJB, def);

        // If prefix is null, adds it
        if (prefix == null) {
            prefix = Jbi4EjbExtension.DEFAULT_PREFIX;
            // Adds the namespace
            def.addNamespace(Jbi4EjbExtension.DEFAULT_PREFIX, Jbi4EjbExtension.NS_URI_JBI4EJB);
        }

        prefix += ":";

        LOG.debug("prefix found: " + prefix);

        if (extension instanceof Jbi4EjbTypes) {
            Jbi4EjbTypes jbi4EjbTypes = (Jbi4EjbTypes) extension;
            pw.print("<" + prefix + Jbi4EjbExtension.TYPES_ELEMENT);
            pw.print(">\n");

            // TYPES
            printTypesElement(jbi4EjbTypes, pw, prefix);
            
            pw.print("</" + prefix + Jbi4EjbExtension.TYPES_ELEMENT+">\n");

        } else {
        	LOG.warn("EJB000401_Error_in_extension_element", new Object[]{Jbi4EjbExtension.TYPES_ELEMENT});
        }

    }

    /**
     * Prints-out the Types element.
     * 
     * @param pw the writer
     * @param prefix the namepsace prefix
     * @param jbi4EjbBiTypes the extensibility element
     */
    private void printTypesElement(Jbi4EjbTypes jbi4EjbBiTypes, java.io.PrintWriter pw, String prefix) {
        if ((jbi4EjbBiTypes.getTypesSerialVersionUIDs() != null) && 
                (!jbi4EjbBiTypes.getTypesSerialVersionUIDs().isEmpty())) {            
            Properties typesProperties = jbi4EjbBiTypes.getTypesSerialVersionUIDs();
            Enumeration enKeys = typesProperties.keys();
            for (; enKeys.hasMoreElements() ;) {
                String key = (String)enKeys.nextElement();
                Object suid = typesProperties.get(key);
                String prop = null;
                if (suid instanceof Long) {
                    prop = ((Long)suid).toString();
                } else {
                    prop = (String)suid;
                }                
                pw.print("<" + prefix + Jbi4EjbExtension.SERIAL_VERSION_UID_ELEMENT);
                DOMUtils.printAttribute(Jbi4EjbExtension.CLASSNAME_ATTRIBUTE, key, pw);
                DOMUtils.printAttribute(Jbi4EjbExtension.UID_ATTRIBUTE, prop, pw);
                pw.print("/>\n");
            }           
        } 
    }    
}
