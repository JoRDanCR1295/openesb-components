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
 * Serializer for the Jbi4Ejb WSDL Extension (binding element), according with JWSDL specs.
 * See JSR 110.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbBindingSerializer implements ExtensionSerializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4EjbBindingSerializer.class);

    
    /**
     * Instantiates a new jbi4 ejb binding serializer.
     */
    public Jbi4EjbBindingSerializer() {}
    
    /*
     * (non-Javadoc)
     *
     * @see javax.wsdl.extensions.ExtORB_ELEMENTensionDeserializer#unmarshall(java.lang.Class,
     *      javax.xml.namespace.QName, org.w3c.dom.Element,
     *      javax.wsdl.Definition, javax.wsdl.extensions.ExtensionRegistry)
     */
    
    /**
     * Marshall the <code>Jbi4EjbBinding</code> in a WSDL binding fragment.
     * 
     * @param parentType the parent type
     * @param elementType the element type
     * @param extension the <code>Jbi4EjbBinding</code> extension
     * @param pw the writer
     * @param def the WSDL definition
     * @param extReg the ExtensionRegistry
     * 
     * @throws WSDLException if some problem occurs in marshalling
     */
    @SuppressWarnings("unchecked")
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

        if (extension instanceof Jbi4EjbBinding) {
            Jbi4EjbBinding jbi4EjbBinding = (Jbi4EjbBinding) extension;
            pw.print("<" + prefix + Jbi4EjbExtension.BINDING_ELEMENT);
            pw.print(">\n");

            // IDL
            printJNDIElement(jbi4EjbBinding, pw, prefix);

            // ORB Properties
            printORBElement(jbi4EjbBinding, pw, prefix);

            pw.print("</" + prefix + Jbi4EjbExtension.BINDING_ELEMENT+">\n");

        } else {
        	LOG.warn("EJB000401_Error_in_extension_element", new Object[]{Jbi4EjbExtension.BINDING_ELEMENT});
        }

    }

    /**
     * Prints the JNDI element.
     * 
     * @param jbi4EjbBinding the binding element
     * @param pw the writer
     * @param prefix the namespace prefix
     */
    @SuppressWarnings("unchecked")
    private void printJNDIElement(Jbi4EjbBinding jbi4EjbBinding, java.io.PrintWriter pw, String prefix) {
        if ((jbi4EjbBinding.getJndiProperties()!= null) && 
                (!jbi4EjbBinding.getJndiProperties().isEmpty())) {
            pw.print("<" + prefix + Jbi4EjbExtension.JNDI_ELEMENT+">\n");
            Properties jndiProperties = jbi4EjbBinding.getJndiProperties();
            Enumeration enKeys = jndiProperties.keys();
            for (; enKeys.hasMoreElements() ;) {
                String key = (String)enKeys.nextElement();
                String prop = (String) jndiProperties.get(key);
                pw.print("<" + prefix + Jbi4EjbExtension.PROPERTY_ELEMENT);
                DOMUtils.printAttribute(Jbi4EjbExtension.NAME_ATTRIBUTE, key, pw);
                DOMUtils.printAttribute(Jbi4EjbExtension.VALUE_ATTRIBUTE, prop, pw);
                pw.print("/>\n");
            }

            pw.print("</" + prefix + Jbi4EjbExtension.JNDI_ELEMENT+">\n");
        } 
    }

    /**
     * Prints the ORB element.
     * 
     * @param jbi4EjbBinding the binding element
     * @param pw the writer
     * @param prefix the namespace prefix
     */
    @SuppressWarnings("unchecked")
    private void printORBElement(Jbi4EjbBinding jbi4EjbBinding, java.io.PrintWriter pw, String prefix) {
        if (jbi4EjbBinding.getOrbProperties()!= null) {
            pw.print("<" + prefix + Jbi4EjbExtension.ORB_ELEMENT+">\n");
            Properties orbProperties = jbi4EjbBinding.getOrbProperties();
            Enumeration enKeys = orbProperties.keys();
            for (; enKeys.hasMoreElements() ;) {
                String key = (String)enKeys.nextElement();
                String prop = (String) orbProperties.get(key);
                pw.print("<" + prefix + Jbi4EjbExtension.PROPERTY_ELEMENT);
                DOMUtils.printAttribute(Jbi4EjbExtension.NAME_ATTRIBUTE, key, pw);
                DOMUtils.printAttribute(Jbi4EjbExtension.VALUE_ATTRIBUTE, prop, pw);
                pw.print("/>\n");
            }

            pw.print("</" + prefix + Jbi4EjbExtension.ORB_ELEMENT+">\n");
        }    
    }
}
