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

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.ibm.wsdl.util.xml.QNameUtils;
import com.sun.jbi.internationalization.Messages;

/**
 * Deserializer for the Jbi4Corba WSDL Extension (addess element), according
 * with JWSDL specs. See JSR 110.
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
@SuppressWarnings("unchecked")
public class Jbi4CorbaAddressDeserializer implements ExtensionDeserializer,
        Serializable {
    
    /** serialVersionUID. */
    private static final long serialVersionUID = -8967121375407535073L;

    private static final Logger LOG = LoggerFactory
            .getLogger(Jbi4CorbaAddressDeserializer.class);

    // Pattern for finding application variable tokens
    private static final String ENV_VAR_REGEX = "\\$\\{([a-zA-Z0-9\\.\\-\\_^\\{\\}]+)\\}";
    private static final Pattern mPattern = Pattern.compile(ENV_VAR_REGEX);

    protected Map<String, String[]> mEnvVariableMap = new HashMap<String, String[]>();

    private static final Messages mMessages = Messages
            .getMessages(Jbi4CorbaAddressDeserializer.class);

    /**
     * Default constructor.
     */
    public Jbi4CorbaAddressDeserializer() {
    }

    public Jbi4CorbaAddressDeserializer(Map<String, String[]> envVariableMap) {
        this();
        mEnvVariableMap.putAll(envVariableMap);
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
     * @param el
     *            The elemnt
     * @param def
     *            The definition
     * @param extReg
     *            The extension registry
     * @return The return
     * @throws WSDLException
     *             The WSDL exception
     */

    public ExtensibilityElement unmarshall(Class parentType, QName elementType,
            Element el, Definition def, ExtensionRegistry extReg)
            throws WSDLException {

        Jbi4CorbaAddress jbi4CorbaAddress = (Jbi4CorbaAddress) extReg
                .createExtension(parentType, elementType);

        jbi4CorbaAddress.setName(getAttrAndResolveEnvVar(el,
                Jbi4CorbaExtension.NAME_ATTRIBUTE));

        jbi4CorbaAddress.setLocalizationType(getAttrAndResolveEnvVar(el,
                Jbi4CorbaExtension.LOCALIZATION_TYPE_ATTRIBUTE));

        // Reads the ORB element
        Element tempEl = DOMUtils.getFirstChildElement(el);
        while (tempEl != null) {
            // ORB element
            if (QNameUtils.matches(Jbi4CorbaExtension.Q_ELEM_JBI4CORBA_ORB,
                    tempEl)) {

                // loads the ORB properties
                Properties orbProperties = new Properties();

                Element propertyElement = DOMUtils.getFirstChildElement(tempEl);

                while (propertyElement != null) {
                    String propertyName = DOMUtils.getAttribute(
                            propertyElement, Jbi4CorbaExtension.NAME_ATTRIBUTE);
                    String propertyValue = getAttrAndResolveEnvVar(
                            propertyElement, Jbi4CorbaExtension.VALUE_ATTRIBUTE);
                    orbProperties.put(propertyName, propertyValue);
                    LOG.debug("Read ORB properties: [" + propertyName + "] "
                            + " [" + propertyValue + "]");
                    propertyElement = DOMUtils
                            .getNextSiblingElement(propertyElement);
                }

                LOG.debug("Read orb properties: " + orbProperties);
                jbi4CorbaAddress.setOrbProperties(orbProperties);
            }
            tempEl = DOMUtils.getNextSiblingElement(tempEl);
        }

        LOG.debug("Loaded from address extension the CORBA name: "
                + jbi4CorbaAddress.getName());
        return jbi4CorbaAddress;
    }

    public Map<String, String[]> getEnvVariableMap() {
        return Collections.unmodifiableMap(mEnvVariableMap);
    }

    // protected Object[] getEnvVariableNames(String attrName, String attrVal)

    protected Object[] getEnvVariableNames(String attrName, String attrVal)
            throws Exception {
        String tokenName = null;
        Matcher m = mPattern.matcher(attrVal);
        Vector refs = new Vector();
        while (m.find()) {
            tokenName = m.group(1);
            if (tokenName == null || tokenName.trim().length() == 0) {
                throw new Exception(mMessages.getString(
                        "CRB000304_Invalid_Token_Name", tokenName));
            }
            refs.add(tokenName);
        }

        if (attrVal.indexOf("${}") >= 0) {
            throw new Exception(mMessages.getString(
                    "CRB000305_Invalid_Empty_Token_Name", new Object[] {
                            attrVal, attrName }));
        }

        return refs.toArray();
    }

    protected String getAttrAndResolveEnvVar(Element el, String attrName)
            throws WSDLException {
        String attrVal = DOMUtils.getAttribute(el, attrName);
        if (attrVal != null) {
            try {
                if (hasMigrationEnvVarRef(attrVal)) {
                    // attribute contains env var reference(s)
                    
                    Object[] vars = getEnvVariableNames(attrName, attrVal);
                    if (vars != null) {
                        for (int i = 0; i < vars.length; i++) {
                            String[] varDesc = (String[]) mEnvVariableMap
                                    .get(vars[i]);
                            if (varDesc == null || varDesc.length != 2) {
                                throw new WSDLException(
                                        "INVALID_WSDL",
                                        mMessages
                                                .getString(
                                                        "CRB000306_Invalid_Env_Var_Ref_No_Def",
                                                        new Object[] { vars[i],
                                                                attrVal,
                                                                attrName }));
                            } else {
                                // check if the de-referenced value has ${ in it
                                String varVal = varDesc[0];
                                if (varVal == null) {
                                    throw new WSDLException(
                                            "INVALID_WSDL",
                                            mMessages
                                                    .getString(
                                                            "CRB000307_Invalid_Env_Var_Value_Null",
                                                            new Object[] {
                                                                    vars[i],
                                                                    attrName }));
                                }
                                if (varVal.indexOf("${") >= 0) {
                                    throw new WSDLException(
                                            "INVALID_WSDL",
                                            mMessages
                                                    .getString(
                                                            "CRB000308_Invalid_Var_Value_Contains_Var_Ref",
                                                            new Object[] {
                                                                    attrName,
                                                                    attrVal,
                                                                    vars[i],
                                                                    varVal }));
                                }
                                attrVal = attrVal.replace("${" + vars[i] + "}",
                                        varVal);
                            }
                        }
                    }
                }
            } catch (WSDLException e) {
                throw e;
            } catch (Exception e) {
                throw new WSDLException("INVALID_WSDL", e.getMessage());
            }
        }
        return attrVal;
    }

    protected boolean hasMigrationEnvVarRef(String attrVal) throws Exception {
        return mPattern.matcher(attrVal).find();
    }

}
