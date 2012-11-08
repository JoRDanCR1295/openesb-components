/*
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.jbi.wsdl;

import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.CODE_PAGE_NAME_ATTRIBUTE;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.Q_ELEM_JBI4CICS_COPY_COBOL;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.Q_ELEM_JBI4CICS_OUTPUT_COPY_COBOL;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.SAME_COPY_COBOL_ATTRIBUTE;
import static it.imolinfo.jbi4cics.jbi.wsdl.Jbi4CicsExtension.SERVICE_PACKAGE_NAME_ATTRIBUTE;
import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.jbi.Messages;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;
import org.w3c.dom.Element;
import com.ibm.wsdl.util.xml.DOMUtils;
import com.ibm.wsdl.util.xml.QNameUtils;

/**
 * Deserializer for the Jbi4Cics WSDL Extension (binding element), according
 * with JWSDL specs. See JSR 110.
 *
 * @author marcopiraccini
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public final class Jbi4CicsBindingDeserializer
        implements ExtensionDeserializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4CicsBindingDeserializer.class);

    /**
     * The responsible to translate localized messages.
     */
    private static final Messages MESSAGES
            = Messages.getMessages(Jbi4CicsBindingDeserializer.class);

    /**
     * Initializes a new instance of this class.
     */
    Jbi4CicsBindingDeserializer() {
    }

    /**
     * Deserializes elements into instances of classes which implement the
     * <code>ExtensibilityElement</code> interface. The return value should be
     * explicitly cast to the more-specific implementing type.
     *
     * @param   parentType     a class object indicating where in the WSDL
     *                         document this extensibility element was
     *                         encountered. For example,
     *                         <code>javax.wsdl.Binding.class</code> would be
     *                         used to indicate this element was encountered as
     *                         an immediate child of a element.
     * @param   elementType    the qname of the extensibility element.
     * @param   elem           the extensibility element to deserialize.
     * @param   def            the definition this extensibility element was
     *                         encountered in.
     * @param   extReg         the <code>ExtensionRegistry</code> to use (if
     *                         needed again).
     * @return  the unmarshalled Jbi4Cics binding element, instance of
     *          {@link Jbi4CicsBinding} class.
     * @throws  WSDLException  if the copy Cobol is not present in the
     *                         unmarshalled Jbi4Cics binding element, the input 
     *                         or output copy Cobol are found more than one
     *                         time in the element, the output copy Cobol is
     *                         required but not contained in the document to 
     *                         parse.
     */
    public ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element elem, Definition def, ExtensionRegistry extReg)
            throws WSDLException {
        Jbi4CicsBinding binding = (Jbi4CicsBinding)
                extReg.createExtension(parentType, elementType);
        String sameCopyCobol = DOMUtils.getAttribute(elem,
                                                     SAME_COPY_COBOL_ATTRIBUTE);

        // Attributes
        binding.setServicePackageName(
                DOMUtils.getAttribute(elem, SERVICE_PACKAGE_NAME_ATTRIBUTE));
        binding.setCodePage(
                DOMUtils.getAttribute(elem, CODE_PAGE_NAME_ATTRIBUTE));
        if (isNullOrBlank(sameCopyCobol)) {
            binding.setSameCopyCobol(null);
        } else {
            binding.setSameCopyCobol(Boolean.valueOf(sameCopyCobol));
        }

        // Elements
        unmarshallCopiesCobol(binding, elem);

        return binding;
    }

    /**
     * Tests if the specified string is <code>null</code>, empty or contains 
     * only blank characters.
     * <p>
     * A <i>blank</i> character has code &lt;= <code>'&#92;u0020'</code> (the
     * space character).
     *
     * @param   str  the string to test.
     * @return  <code>true</code> if and only if <code>str</code> is 
     *          <code>null</code>, has length zero or is made only by blank 
     *          characters.
     */
    private static boolean isNullOrBlank(final String str) {
        if (str != null) {
            for (int i = str.length() - 1; i >= 0; --i) {
                if (str.charAt(i) > ' ') {
                    return false;
                }
            }
        }
        return true;
    }
    
    /**
     * Deserializes the input and output copies Cobol, updating the specified
     * <code>Jbi4CicsBinding</code> instance.
     *
     * @param   elem           the extensibility element to deserialize.
     * @param   binding        the unmarshalled Jbi4Cics binding element to 
     *                         update with copies Cobol.
     * @throws  WSDLException  if the (input) copy Cobol is not present in the
     *                         unmarshalled Jbi4Cics binding element, the input 
     *                         or output copy Cobol are found more than one
     *                         time in the element, the output copy Cobol is
     *                         required but not contained in the document to
     *                         parse.
     */
    private void unmarshallCopiesCobol(Jbi4CicsBinding binding, Element elem)
            throws WSDLException {
        boolean copyCobolFound = false;
        boolean outCopyCobolFound = false;

        for (Element e = DOMUtils.getFirstChildElement(elem);
             e != null;
             e = DOMUtils.getNextSiblingElement(e)) {
            if (QNameUtils.matches(Q_ELEM_JBI4CICS_OUTPUT_COPY_COBOL, e)) {
                if (outCopyCobolFound) {
                    throw createWSDLException(
                            "CIC001314_Element_found_many_times",
                            Q_ELEM_JBI4CICS_OUTPUT_COPY_COBOL);
                }
                binding.setOutputCopyCobol(e.getTextContent());
                outCopyCobolFound = true;
            } else if (QNameUtils.matches(Q_ELEM_JBI4CICS_COPY_COBOL, e)) {
                if (copyCobolFound) {
                    throw createWSDLException(
                            "CIC001314_Element_found_many_times",
                            Q_ELEM_JBI4CICS_COPY_COBOL);
                }
                binding.setCopyCobol(e.getTextContent());
                copyCobolFound = true;
            }
        }

        if (!copyCobolFound) {
            throw createWSDLException("CIC001309_Copy_cobol_not_found",
                                      Q_ELEM_JBI4CICS_COPY_COBOL);
        }
        if (Boolean.FALSE.equals(binding.getSameCopyCobol())
                && !outCopyCobolFound) {
            throw createWSDLException("CIC001315_Output_copy_Cobol_required",
                                      null);
        }
    }

    /**
     * Creates a new <code>WSDLException</code> with the specific error message,
     * logging also the error message itself.
     *
     * @param   msgKey  the error message bundle key. Must be not
     *                  <code>null</code>.
     * @param   msgArg  the optional error message argument, used to format the
     *                  resulting message. If not <code>null</code>, the message
     *                  identified by <code>msgKey</code> is assumed to be
     *                  parametric and it must contain one argument, needed to
     *                  compose the final message shown thru the returned
     *                  exception. Use <code>null</code> for messages that don't
     *                  contain parameters.
     * @return  the newly created <code>WSDLException</code> containing the
     *          error message retrieved using the received key and the optional
     *          parameter.
     */
    private static WSDLException createWSDLException(final String msgKey,
                                                     final Object msgArg) {
        String msg;

        if (msgArg == null) {
            LOG.error(msgKey);
            msg = MESSAGES.getString(msgKey);
        } else {
            LOG.error(msgKey, msgArg);
            msg = MESSAGES.getString(msgKey, msgArg);
        }

        // No fault code...
        return new WSDLException(null, msg);
    }
}
