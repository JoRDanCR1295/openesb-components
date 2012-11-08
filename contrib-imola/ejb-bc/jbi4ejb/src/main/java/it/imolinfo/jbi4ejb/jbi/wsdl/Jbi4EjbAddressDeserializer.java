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

/**
 * Deserializer for the Jbi4Ejb WSDL Extension (addess element), according with JWSDL specs.
 * See JSR 110.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbAddressDeserializer implements ExtensionDeserializer {

    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4EjbAddressDeserializer.class);
    
    
    /**
     * Instantiates a new jbi4 ejb address deserializer.
     */
    public Jbi4EjbAddressDeserializer() {}
   
    
    /**
     * Deserialize the jbi4ejb address from the WSDL.
     * 
     * @param parentType the parten type
     * @param elementType the elemnt type
     * @param el the element
     * @param def the whole WSDL definition
     * @param extReg the extension registry
     * 
     * @return the <code>Jbi4EjbAddress</code> extensibility element
     * 
     * @throws WSDLException if some problem occurs in unmarshalling
     * 
     * @see javax.wsdl.extensions.ExtensionDeserializer#unmarshall(java.lang.Class,
     *      javax.xml.namespace.QName, org.w3c.dom.Element,
     *      javax.wsdl.Definition, javax.wsdl.extensions.ExtensionRegistry)
     */
    @SuppressWarnings("unchecked")
    public ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element el, Definition def, ExtensionRegistry extReg)
    throws WSDLException {

        Jbi4EjbAddress jbi4EjbAddress = (Jbi4EjbAddress) extReg
        .createExtension(parentType, elementType);

        jbi4EjbAddress.setName(DOMUtils.getAttribute(el,
                Jbi4EjbExtension.NAME_ATTRIBUTE));

        jbi4EjbAddress.setLocalizationType(DOMUtils.getAttribute(el,
                Jbi4EjbExtension.LOCALIZATION_TYPE_ATTRIBUTE));

        LOG.debug("Loaded from address extension the name: " + jbi4EjbAddress.getName());

        return jbi4EjbAddress;
    }

}
