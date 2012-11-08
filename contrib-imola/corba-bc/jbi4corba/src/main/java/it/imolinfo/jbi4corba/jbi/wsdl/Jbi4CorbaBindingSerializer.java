/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.jbi.wsdl;

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
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>ss
 */
public class Jbi4CorbaBindingSerializer implements ExtensionSerializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG = LoggerFactory
            .getLogger(Jbi4CorbaBindingSerializer.class);

    /**
     * Default constructor.
     */
    public Jbi4CorbaBindingSerializer() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * javax.wsdl.extensions.ExtensionDeserializer#unmarshall(java.lang.Class,
     * javax.xml.namespace.QName, org.w3c.dom.Element, javax.wsdl.Definition,
     * javax.wsdl.extensions.ExtensionRegistry)
     */

    /**
     * @param parentType
     *            The parent type
     * @param elementType
     *            The element type
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

        // If prefix is null, adds it
        if (prefix == null) {
            prefix = Jbi4CorbaExtension.DEFAULT_PREFIX;
            // Adds the namespace
            def.addNamespace(Jbi4CorbaExtension.DEFAULT_PREFIX,
                    Jbi4CorbaExtension.NS_URI_JBI4CORBA);
        }

        prefix += ":";

        LOG.debug("prefix found: " + prefix);

        if (extension instanceof Jbi4CorbaBinding) {
            Jbi4CorbaBinding jbi4CorbaBinding = (Jbi4CorbaBinding) extension;
            pw.print("<" + prefix + Jbi4CorbaExtension.BINDING_ELEMENT);
            pw.print(">\n");

            // IDL
            printIDLElement(jbi4CorbaBinding, pw, prefix);

            pw
                    .print("</" + prefix + Jbi4CorbaExtension.BINDING_ELEMENT
                            + ">\n");

        } else {
            LOG.warn("CRB000300_The_extension_element_is_not_a",
                    Jbi4CorbaExtension.BINDING_ELEMENT);
        }

    }

    /**
     * Prints the IDL element.
     * 
     * @param jbi4CorbaBinding
     *            The jbi4Corba binding
     * @param pw
     *            The print writer
     * @param prefix
     *            The prefix
     */
    private void printIDLElement(Jbi4CorbaBinding jbi4CorbaBinding,
            java.io.PrintWriter pw, String prefix) {
        for (Jbi4CorbaIDLEntry jbi4CorbaIDLEntry:jbi4CorbaBinding.getJbi4CorbaDLEntryList()) {
            if (jbi4CorbaIDLEntry.isDefault()){
                pw.print("<" + prefix + Jbi4CorbaExtension.IDL_ENTRY + ">");
            } else {
                pw.print("<" + prefix + Jbi4CorbaExtension.IDL_ENTRY + " "+Jbi4CorbaExtension.ROOT+"=\""+jbi4CorbaIDLEntry.isRoot()+"\" "+Jbi4CorbaExtension.FILENAME+"=\""+jbi4CorbaIDLEntry.getIdlFilename()+"\" "+Jbi4CorbaExtension.RELATIVE_PATH+"=\""+jbi4CorbaIDLEntry.getRelativePath()+"\">");
            }
            pw.print(DOMUtils.cleanString(jbi4CorbaIDLEntry.getIDL()));
            pw.print("</" + prefix + Jbi4CorbaExtension.IDL_ENTRY + ">\n");
        }
    }

}
