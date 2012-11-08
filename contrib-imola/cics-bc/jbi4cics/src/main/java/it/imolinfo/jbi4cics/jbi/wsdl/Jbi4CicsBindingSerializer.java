/*
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi.wsdl;

import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.BINDING_ELEMENT;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.CODE_PAGE_NAME_ATTRIBUTE;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.COPY_COBOL_ELEMENT;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.DEFAULT_PREFIX;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.NS_URI_JBI4CICS;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.OUTPUT_COPY_COBOL_ELEMENT;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.SAME_COPY_COBOL_ATTRIBUTE;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.SERVICE_PACKAGE_NAME_ATTRIBUTE;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.jbi.Messages;
import java.io.PrintWriter;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;
import com.ibm.wsdl.util.xml.DOMUtils;

/**
 * Serializer for the address elemento of Jbi4Cics WSDL Extension, according
 * with JWSDL specs. See JSR 110.
 * <p>
 *
 * @author amedeocannone
 * @author marcopiraccini
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public class Jbi4CicsBindingSerializer implements ExtensionSerializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4CicsBindingSerializer.class);

    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(Jbi4CicsBindingSerializer.class);

    /**
     * Initializes a new instance of this class.
     */
    Jbi4CicsBindingSerializer() {
    }

    /**
     * Serializes Jbi4Cics extension-specific instances of
     * <code>ExtensibilityElement</code> into the specified
     * <code>PrintWriter</code>.
     *
     * @param   parentType     a class object indicating where in the WSDL
     *                         definition this extension was encountered. For
     *                         example, <code>javax.wsdl.Binding.class</code>
     *                         would be used to indicate this extensibility
     *                         element was found in the list of extensibility
     *                         elements belonging to a
     *                         <code>javax.wsdl.Binding</code>.
     * @param   elementType    the qname of the extensibility element.
     * @param   extension      the extensibility element to serialize.
     * @param   writer         the writer to write to.
     * @param   def            the definition this extensibility element was
     *                         encountered in.
     * @param   extReg         the <code>ExtensionRegistry</code> to use (if
     *                         needed again).
     * @throws  WSDLException  if <code>extension</code> is a
     *                         {@link Jbi4CicsBinding} instance but doesn't
     *                         contain mandatory values.
     */
    public void marshall(Class parentType, QName elementType,
            ExtensibilityElement extension, PrintWriter writer,
            Definition def, ExtensionRegistry extReg) throws WSDLException {
        String prefix = null;

        // Gets the QN prefix
        try {
            prefix = DOMUtils.getPrefix(NS_URI_JBI4CICS, def);
        } catch (WSDLException e) {
            LOG.warn("CIC001300_Jbi4cics_namespace_not_found",
                     new Object[] { NS_URI_JBI4CICS }, e);
        }

        // If prefix (i.e. the namespace) is null, adds it
        if (prefix == null) {
            prefix = DEFAULT_PREFIX;
            def.addNamespace(DEFAULT_PREFIX, NS_URI_JBI4CICS);
        }
        prefix = prefix.concat(":");

        if (LOG.isDebugEnabled()) {
            LOG.debug("Prefix found: " + prefix);
        }

        if (extension instanceof Jbi4CicsBinding) {
            Jbi4CicsBinding binding = (Jbi4CicsBinding) extension;

            doMarshall(binding, writer, prefix);
        } else {
            LOG.warn("CIC001308_Invalid_extension_element", BINDING_ELEMENT);
        }
    }

    /**
     * Serializes Jbi4Cics extension-specific instances of
     * <code>Jbi4CicsBinding</code> into the specified
     * <code>PrintWriter</code>.
     *
     * @param   binding        the extensibility element to serialize. Must be
     *                         not <code>null</code>.
     * @param   writer         the writer to write to. Must be not
     *                         <code>null</code>.
     * @param   prefix         the namespace prefix to use for WSDL tag. Must be
     *                         not <code>null</code>.
     * @throws  WSDLException  if <code>binding</code> doesn't contain mandatory
     *                         values.
     */
    private void doMarshall(Jbi4CicsBinding binding, PrintWriter writer,
            String prefix) throws WSDLException {
        Boolean sameCopyCobol = binding.getSameCopyCobol();
        String string;

        writer.print("<" + prefix + BINDING_ELEMENT);

        // servicePackageName
        string = binding.getServicePackageName();
        if (string != null) {
            DOMUtils.printAttribute(SERVICE_PACKAGE_NAME_ATTRIBUTE, string,
                                    writer);
        } else {
            throw createWSDLException("CIC001310_Invalid_service_package_name");
        }

        // codePage
        string = binding.getCodePage();
        if (string != null) {
            DOMUtils.printAttribute(CODE_PAGE_NAME_ATTRIBUTE, string, writer);
        } else {
            throw createWSDLException("CIC001311_Invalid_code_page");
        }

        // sameCopyCobol
        if (sameCopyCobol != null) {
            DOMUtils.printAttribute(SAME_COPY_COBOL_ATTRIBUTE,
                                    sameCopyCobol.toString(), writer);
        }
        writer.print(">\n");

        // copyCobol
        string = binding.getCopyCobol();
        if (string == null) {
            throw createWSDLException("CIC001312_Invalid_copy_cobol");
        }
        writer.print("<" + prefix + COPY_COBOL_ELEMENT + ">"
                     + DOMUtils.cleanString(string) + "</" + prefix
                     + COPY_COBOL_ELEMENT + ">\n");

        // outputCopyCobol
        string = binding.getOutputCopyCobol();
        if ((string == null) || (string.trim().length() == 0)) {
            if (Boolean.FALSE.equals(sameCopyCobol)) {
                throw createWSDLException("CIC001313_Invalid_copy_cobol");
            }
        } else {
            writer.print("<" + prefix + OUTPUT_COPY_COBOL_ELEMENT + ">"
                         + DOMUtils.cleanString(string) + "</" + prefix
                         + OUTPUT_COPY_COBOL_ELEMENT + ">\n");
        }

        writer.print("</" + prefix + BINDING_ELEMENT + ">\n");
    }

    /**
     * Creates a new <code>WSDLException</code> with the specific error message,
     * logging also the error message itself.
     *
     * @param   msgKey  the error message bundle key. Must be not
     *                  <code>null</code>.
     * @return  the newly created <code>WSDLException</code> containing the
     *          error message retrieved using the received key.
     */
    private static WSDLException createWSDLException(final String msgKey) {
        LOG.error(msgKey);
        return new WSDLException(WSDLException.INVALID_WSDL,
                                 MESSAGES.getString(msgKey));
    }
}
