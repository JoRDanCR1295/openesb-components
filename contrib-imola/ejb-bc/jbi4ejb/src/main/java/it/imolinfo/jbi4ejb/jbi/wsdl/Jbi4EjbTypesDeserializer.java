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
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.ibm.wsdl.util.xml.QNameUtils;

/**
 * Deserializer for the Jbi4Ejb WSDL Extension (Types element), according with JWSDL specs.
 * See JSR 110.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbTypesDeserializer implements ExtensionDeserializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4EjbTypesDeserializer.class);
        
    /**
     * Instantiates a new jbi4 ejb types deserializer.
     */
    public Jbi4EjbTypesDeserializer() {}

    /**
     * Unmarshall the Jbi4EjbTypes extensibility element.
     * 
     * @param parentType
     *            the parent type
     * @param elementType
     *            the element type
     * @param el
     *            the element
     * @param def
     *            the WSDL Definition
     * @param extReg
     *            the extension registry
     * 
     * @return the unmarhalled extensibility element
     * 
     * @throws WSDLException
     *             if some problem occurs in unmarshalling
     */
    public ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element el, Definition def, ExtensionRegistry extReg)
    throws WSDLException {

        Jbi4EjbTypes jbi4EjbTypes = (Jbi4EjbTypes) extReg
            .createExtension(parentType, elementType);

        Element element = DOMUtils.getFirstChildElement(el);
        while (element != null) {

            // serialVersioneUID Element
            if (QNameUtils.matches(Jbi4EjbExtension.Q_ELEM_JBI4EJB_SERIAL_VERSION_UID,
                    element)) {                
                String className = DOMUtils.getAttribute(
                        element, Jbi4EjbExtension.CLASSNAME_ATTRIBUTE);
                String uid = DOMUtils.getAttribute(element,
                        Jbi4EjbExtension.UID_ATTRIBUTE);
                LOG.debug("Found uid for className " + className +": " + uid);
 
                jbi4EjbTypes.getTypesSerialVersionUIDs().put(className, uid);
            }
            element = DOMUtils
                .getNextSiblingElement(element);

        }

        return jbi4EjbTypes;
    }
}
