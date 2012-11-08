/****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.jbi.wsdl;

import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.ibm.wsdl.util.xml.QNameUtils;

/**
 * Deserializer for the Address extesions configurations.
 * 
 * @author rchen
 */
public class Jbi4CorbaExtPreprocessDeserializer extends
        Jbi4CorbaAddressDeserializer {

    /** serialVersionUID. */
    private static final long serialVersionUID = -3031218478097036537L;

    private static final String APPVAR_TYPE_STRING = "STRING";

    // default constructor
    public Jbi4CorbaExtPreprocessDeserializer() {
        super();
    }

    /** Creates a new instance of Jbi4CorbaExtPreprocessDeserializer */
    public Jbi4CorbaExtPreprocessDeserializer(
            Map<String, String[]> envVariableMap) {
        super(envVariableMap);
    }

    @SuppressWarnings("unchecked")
    public javax.wsdl.extensions.ExtensibilityElement unmarshall(
            Class parentType, QName elementType, Element el, Definition def,
            ExtensionRegistry extReg) throws javax.wsdl.WSDLException {
        ExtensibilityElement returnValue = null;
        if (Jbi4CorbaExtension.ADDRESS_ELEMENT.equals(elementType)) {

            Jbi4CorbaAddress jbi4Address = new Jbi4CorbaAddress();

            collectEnvVars(el, Jbi4CorbaExtension.NAME_ATTRIBUTE,
                    APPVAR_TYPE_STRING);

            collectEnvVars(el, Jbi4CorbaExtension.LOCALIZATION_TYPE_ATTRIBUTE,
                    APPVAR_TYPE_STRING);

            // Reads the ORB element
            Element tempEl = DOMUtils.getFirstChildElement(el);
            while (tempEl != null) {
                // ORB element
                if (QNameUtils.matches(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_ORB,
                        tempEl)) {

                    Element propertyElement = DOMUtils
                            .getFirstChildElement(tempEl);

                    while (propertyElement != null) {
                        String propertyName = DOMUtils.getAttribute(
                                propertyElement,
                                Jbi4CorbaExtension.NAME_ATTRIBUTE);

                        collectEnvVars(propertyElement,
                                Jbi4CorbaExtension.VALUE_ATTRIBUTE,
                                APPVAR_TYPE_STRING);
                    }

                }
                tempEl = DOMUtils.getNextSiblingElement(tempEl);
            }

            returnValue = jbi4Address;
        }

        return returnValue;
    }

    protected void collectEnvVars(Element el, String attrName, String type)
            throws WSDLException {
        String s = DOMUtils.getAttribute(el, attrName);
        if (s != null) {
            try {
                if (hasMigrationEnvVarRef(s)) {
                    Object[] envVariableNames = getEnvVariableNames(attrName, s);
                    if (envVariableNames != null) {
                        for (Object envVariableName : envVariableNames) {
                            String name = envVariableName.toString();
                            if (!mEnvVariableMap.containsKey(name)) {
                                mEnvVariableMap.put(name, new String[] { "",
                                        type });
                            }
                        }
                    }
                }
            } catch (Exception e) {
                throw new WSDLException("INVALID_WSDL", e.getMessage());
            }
        }
    }
}
