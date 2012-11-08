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

import java.util.Properties;

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
 * Deserializer for the Jbi4Ejb WSDL Extension (Binding element), according with JWSDL specs.
 * See JSR 110.
 *
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class Jbi4EjbBindingDeserializer implements ExtensionDeserializer {

    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(Jbi4EjbBindingDeserializer.class);

    
    /**
     * Instantiates a new jbi4 ejb binding deserializer.
     */
    public Jbi4EjbBindingDeserializer() {}
    
    /*
     * (non-Javadoc)
     *
     * @see javax.wsdl.extensions.ExtensionDeserializer#unmarshall(java.lang.Class,
     *      javax.xml.namespace.QName, org.w3c.dom.Element,
     *      javax.wsdl.Definition, javax.wsdl.extensions.ExtensionRegistry)
     */
    
    /**
     * Unmarshall the WSDL to obtain a <code>Jbi4EjbBinding</code> element.
     * 
     * @param parentType the parent type
     * @param elementType the element type
     * @param el the element to parse
     * @param def the WSDL definition
     * @param extReg the ExtensionRegistry
     * 
     * @return the <code>jbi4EjbBinding</code> Extensibility element
     * 
     * @see javax.wsdl.extensions.ExtensionDeserializer#unmarshall(java.lang.Class,
     *      javax.xml.namespace.QName, org.w3c.dom.Element,
     *      javax.wsdl.Definition, javax.wsdl.extensions.ExtensionRegistry)
     * 
     * @throws WSDLException if some problem occurs in unmarshalling
     */
    @SuppressWarnings("unchecked")
    public ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element el, Definition def, ExtensionRegistry extReg)
    throws WSDLException {

        Jbi4EjbBinding jbi4EjbBinding = (Jbi4EjbBinding) extReg
        .createExtension(parentType, elementType);

        Element tempEl = DOMUtils.getFirstChildElement(el);
        while (tempEl != null) {

            // ORB element
            if (QNameUtils.matches(Jbi4EjbExtension.Q_ELEM_JBI4EJB_ORB,
                    tempEl)) {

                // loads the ORB properties
                Properties orbProperties = new Properties();

                Element propertyElement = DOMUtils.getFirstChildElement(tempEl);

                while (propertyElement != null) {
                    String propertyName = DOMUtils.getAttribute(
                            propertyElement, Jbi4EjbExtension.NAME_ATTRIBUTE);
                    String propertyValue = DOMUtils
                    .getAttribute(propertyElement,
                            Jbi4EjbExtension.VALUE_ATTRIBUTE);
                    orbProperties.put(propertyName, propertyValue);
                    LOG.debug("Read ORB properties: [" + propertyName + "] "
                            + " [" + propertyValue + "]");
                    propertyElement = DOMUtils
                    .getNextSiblingElement(propertyElement);
                }

                // Todo implements
                LOG.debug("Read orb properties: " + orbProperties);
                jbi4EjbBinding.setOrbProperties(orbProperties);
            }

            // JNDI Element
            if (QNameUtils.matches(Jbi4EjbExtension.Q_ELEM_JBI4EJB_JNDI,
                    tempEl)) {

                // loads the ORB properties
                Properties jndiProperties = new Properties();

                Element propertyElement = DOMUtils.getFirstChildElement(tempEl);

                while (propertyElement != null) {
                    String propertyName = DOMUtils.getAttribute(
                            propertyElement, Jbi4EjbExtension.NAME_ATTRIBUTE);
                    String propertyValue = DOMUtils
                    .getAttribute(propertyElement,
                            Jbi4EjbExtension.VALUE_ATTRIBUTE);
                    jndiProperties.put(propertyName, propertyValue);
                    LOG.debug("Read ORB properties: [" + propertyName + "] "
                            + " [" + propertyValue + "]");
                    propertyElement = DOMUtils
                    .getNextSiblingElement(propertyElement);
                }

                // Todo implements
                LOG.debug("Read jndi properties: " + jndiProperties);
                jbi4EjbBinding.setOrbProperties(jndiProperties);
            }

            tempEl = DOMUtils.getNextSiblingElement(tempEl);
        }

        return jbi4EjbBinding;
    }
}
