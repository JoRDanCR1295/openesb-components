/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)HL7ExtPreprocessDeserializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.I18n;

import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Sun Microsystems
 */

public class HL7ExtPreprocessDeserializer extends HL7ExtSerializer {

    private static final Logger mLogger = Logger.getLogger(HL7ExtPreprocessDeserializer.class.getName());

    private static String mAcceptAckXsd;

    private static String mApplicationAckXsd;

    // default constructor
    public HL7ExtPreprocessDeserializer() {
        super();
    }

    public HL7ExtPreprocessDeserializer(Map<String, String[]> envVariableMap) {
        super(envVariableMap);
    }

    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType,
                                                                 QName elementType,
                                                                 Element el,
                                                                 Definition def,
                                                                 ExtensionRegistry extReg) throws WSDLException {

        ExtensibilityElement returnValue = null;

        if (HL7Constants.QNAME_ADDRESS.equals(elementType)) {
            HL7Address hl7Address = new HL7Address();

            collectEnvVars(el, HL7Address.ATTR_HL7_SVR_LOCATIONURL, HL7Constants.ENV_VAR_TYPE_STRING);

            String transportProtocolName = DOMUtils.getAttribute(el, HL7Address.ATTR_HL7_TRANS_PROTOCOL_NAME);
            if (nonEmptyString(transportProtocolName)) {
                hl7Address.setTransportProtocolName(transportProtocolName);
            }
            returnValue = hl7Address;

        } else if (HL7Constants.QNAME_PROTOCOLPROPERTIES.equals(elementType)) {
            HL7ProtocolProperties hl7ProtocolProperties = new HL7ProtocolProperties();

            String acknowledgmentMode = DOMUtils.getAttribute(el, ATTR_ACK_MODE);
            if (nonEmptyString(acknowledgmentMode)) {
                hl7ProtocolProperties.setAckMode(acknowledgmentMode);
            }
            String llpType = DOMUtils.getAttribute(el, ATTR_LLP_TYPE);
            if (nonEmptyString(llpType)) {
                hl7ProtocolProperties.setLLPType(llpType);
            }

            collectEnvVars(el, ATTR_START_BLOCK_CHARACTER, HL7Constants.ENV_VAR_TYPE_NUMBER);
            collectEnvVars(el, ATTR_END_DATA_CHARACTER, HL7Constants.ENV_VAR_TYPE_NUMBER);
            collectEnvVars(el, ATTR_END_BLOCK_CHARACTER, HL7Constants.ENV_VAR_TYPE_NUMBER);

            String hllpCheckSumEnabled = DOMUtils.getAttribute(el, ATTR_HLLP_CHECKSUM_ENABLED);
            if (nonEmptyString(hllpCheckSumEnabled)) {
                hl7ProtocolProperties.setHLLPChkSumEnabled(new Boolean(hllpCheckSumEnabled));
            }

            collectEnvVars(el, ATTR_SEQNUM_ENABLED, HL7Constants.ENV_VAR_TYPE_BOOLEAN);
            collectEnvVars(el, ATTR_VALIDATE_MSH, HL7Constants.ENV_VAR_TYPE_BOOLEAN);
            collectEnvVars(el, ATTR_PROCESSING_ID, HL7Constants.ENV_VAR_TYPE_STRING);

            String version = DOMUtils.getAttribute(el, ATTR_VERSION_ID);
            if (nonEmptyString(version)) {
                hl7ProtocolProperties.setVersionID(version);
            }
            if (nonEmptyString(version) && version.startsWith(HL7Constants.V3)) {
                String accAckXsd = DOMUtils.getAttribute(el, ATTR_ACCEPT_ACK_XSD);
                String appAckXsd = DOMUtils.getAttribute(el, ATTR_APPLICATION_ACK_XSD);
                if (nonEmptyString(accAckXsd))
                    mAcceptAckXsd = accAckXsd;
                if (nonEmptyString(appAckXsd))
                    mApplicationAckXsd = appAckXsd;
            }

            collectEnvVars(el, ATTR_FIELD_SEPARATOR, HL7Constants.ENV_VAR_TYPE_NUMBER);
            collectEnvVars(el, ATTR_ENCODING_CHARACTERS, HL7Constants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_SENDING_APPLICATION, HL7Constants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_SENDING_FACILITY, HL7Constants.ENV_VAR_TYPE_STRING);

            collectEnvVars(el, ATTR_ENABLED_SFT, HL7Constants.ENV_VAR_TYPE_BOOLEAN);
            collectEnvVars(el, ATTR_SOFTWARE_VENDOR_ORGANIZATION, HL7Constants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_SOFTWARE_CERTIFIED_VERSION, HL7Constants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_SOFTWARE_PRODUCT_NAME, HL7Constants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_SOFTWARE_BINARY_ID, HL7Constants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_SOFTWARE_PRODUCT_INFORMATION, HL7Constants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_SOFTWARE_INSTALLED_DATE, HL7Constants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_MLLPV2_RETRIES_COUNT_ON_NAK, HL7Constants.ENV_VAR_TYPE_NUMBER);
            collectEnvVars(el, ATTR_MLLPV2_RETRY_INTERVAL, HL7Constants.ENV_VAR_TYPE_NUMBER);
            collectEnvVars(el, ATTR_MLLPV2_TIME_TO_WAIT_FOR_ACK_NAK, HL7Constants.ENV_VAR_TYPE_NUMBER);
            returnValue = hl7ProtocolProperties;
        } else if (HL7Constants.QNAME_COMMUNICATIONCONTROLS.equals(elementType)) {
            HL7CommunicationControls hl7CommunicationControls = new HL7CommunicationControls();
            Map<String, HL7CommunicationControl> commControlColl = getCommControls(el);
            if (commControlColl != null && commControlColl.size() > 0) {
                hl7CommunicationControls.setCommunicationControls(commControlColl);
            }
            returnValue = hl7CommunicationControls;
        }
        return returnValue;

    }

    /**
     * This method returns the accept acknowledgement xsd name
     * 
     * @return string
     */
    public String getAcceptAckXsdName() {
        if (nonEmptyString(mAcceptAckXsd))
            return mAcceptAckXsd;
        return null;
    }

    /**
     * This method returns the application acknowledgement xsd name
     * 
     * @return string
     */
    public String getApplicationAckXsdName() {
        if (nonEmptyString(mApplicationAckXsd))
            return mApplicationAckXsd;
        return null;
    }

    protected void collectEnvVars(Element el, String attrName, String envVarType) throws WSDLException {
        String s = DOMUtils.getAttribute(el, attrName);
        if (s != null) {
            try {
                if (hasMigrationEnvVarRef(s)) {
                    Object[] envVariableNames = getEnvVariableNames(attrName, s);
                    if (envVariableNames != null) {
                        for (Object envVariableName : envVariableNames) {
                            String name = envVariableName.toString();
                            if (!mEnvVariableMap.containsKey(name)) {
                                mEnvVariableMap.put(name, new String[] { null, envVarType });
                            }
                        }
                    }
                }
            } catch (Exception e) {
                throw new WSDLException("INVALID_WSDL", e.getMessage());
            }
        }
    }

    protected Map<String, HL7CommunicationControl> getCommControls(Element e1) throws WSDLException {
        Map<String, HL7CommunicationControl> commControlColl = new HashMap<String, HL7CommunicationControl>();
        if (e1 != null) {
            NodeList commControlList = e1.getChildNodes();
            if (commControlList != null && commControlList.getLength() > 0) {
                Node node = null;
                Element commControlElem = null;
                String localName = null, name = null, value = null;
                Attr attribute = null;
                for (int i = 0; i < commControlList.getLength(); i++) {
                    node = commControlList.item(i);
                    if (node instanceof Element) {
                        commControlElem = (Element) commControlList.item(i);
                        localName = commControlElem.getLocalName();
                        if (localName.equals(HL7Constants.ELEM_COMMUNICATIONCONTROL)) {
                            HL7CommunicationControl commControl = new HL7CommunicationControl();
                            // Get the communicationcontrol attributes (name,
                            // value, enabled,
                            // recourseAction)
                            List listOfNodes = DOMUtils.getAttributes(commControlElem);
                            if (listOfNodes != null && listOfNodes.size() > 0) {
                                for (Object obj : listOfNodes) {
                                    attribute = (Attr) obj;
                                    name = attribute.getNodeName();
                                    if (name.equals(HL7CommunicationControl.ATTR_NAME)) {
                                        value = attribute.getValue();
                                        if (nonEmptyString(value)) {
                                            commControl.setName(value);
                                        }
                                    } else if (name.equals(HL7CommunicationControl.ATTR_VALUE)) {
                                        collectEnvVars(commControlElem, HL7CommunicationControl.ATTR_VALUE,
                                                HL7Constants.ENV_VAR_TYPE_NUMBER);
                                    } else if (name.equals(HL7CommunicationControl.ATTR_ENABLED)) {
                                        value = attribute.getValue();
                                        if (nonEmptyString(value)) {
                                            commControl.setEnabled(new Boolean(value));
                                        }
                                    } else if (name.equals(HL7CommunicationControl.ATTR_RECOURSE_ACTION)) {
                                        value = attribute.getValue();
                                        if (nonEmptyString(value)) {
                                            commControl.setRecourseAction(value);
                                        }
                                    } else {
                                        mLogger.log(Level.WARNING, I18n.msg("W0104: Unexpected attribute {0} for {1}; ignoring it", name,
                                                HL7Constants.ELEM_COMMUNICATIONCONTROL));
                                    }
                                }
                            } else {
                                mLogger.log(Level.SEVERE, I18n.msg("E0126: {0} is being used but no attributes are defined for it",
                                         HL7Constants.ELEM_COMMUNICATIONCONTROL));

                                String errMsg = I18n.msg("E0126: {0} is being used but no attributes are defined for it",
                                        HL7Constants.ELEM_COMMUNICATIONCONTROL);

                                throw new WSDLException(WSDLException.INVALID_WSDL, errMsg);
                            }
                            // Add commControl to collection
                            commControlColl.put(commControl.getName(), commControl);
                        } else {
                            mLogger.log(Level.SEVERE, I18n.msg("E0127: Unexpected element {0} in the WSDL", node.getLocalName()));
                            String errMsg = I18n.msg("E0127: Unexpected element {0} in the WSDL", node.getLocalName());
                            throw new WSDLException(WSDLException.INVALID_WSDL, errMsg);
                        }
                    }
                } // for each hl7:communicationcontrol

            }
        }
        return commControlColl;

    }

}
