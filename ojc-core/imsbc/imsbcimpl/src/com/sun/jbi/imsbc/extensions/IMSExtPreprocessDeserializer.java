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

package com.sun.jbi.imsbc.extensions;


import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;


import com.sun.jbi.internationalization.Messages;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.imsbc.extensions.IMSConstants;

import org.w3c.dom.Element;

/**
 * @author Sun Microsystems
 */

public class IMSExtPreprocessDeserializer extends IMSExtSerializer {

	private static final Messages mMessages = Messages
			.getMessages(IMSExtPreprocessDeserializer.class);

    private static final String APPVAR_TYPE_STRING = "STRING";
    private static final String APPVAR_TYPE_NUMBER = "NUMBER";

	// default constructor
	public IMSExtPreprocessDeserializer() {
		super();
	}

	public IMSExtPreprocessDeserializer(Map<String, String[]> envVariableMap) {
		super(envVariableMap);
	}

	public javax.wsdl.extensions.ExtensibilityElement unmarshall(
			Class parentType, QName elementType, Element el, Definition def,
			ExtensionRegistry extReg) throws WSDLException {

		ExtensibilityElement returnValue = null;

		if (IMSConstants.QNAME_ADDRESS.equals(elementType)) {
			IMSAddress imsAddress = new IMSAddress();

			collectEnvVars(el, IMSAddress.ATTR_LOCTN,
					APPVAR_TYPE_STRING);

			returnValue = imsAddress;

		} else if (IMSConstants.QNAME_MESSAGE.equals(elementType)) {
            IMSMessage imsMessage = new IMSMessage();
            
            collectEnvVars(el, ATTR_IRM_LEN, APPVAR_TYPE_NUMBER);

            collectEnvVars(el, ATTR_IRM_ID, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_TIMER, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_SOCKET, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_CLIENTID, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_MOD, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_COMMITMODE, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_SYNCLEVEL, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_ACK, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_FLOW, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_TRANCODE, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_TRANCODESRC, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_DESTID, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_LTERM, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_RACFGRPNAME, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_RACFUSERID, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_RACFPWD, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_HEADERENCODING, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_SENDDATAENCODING, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_IRM_REPLYDATAENCODING, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_USE_TYPE, APPVAR_TYPE_STRING);

            collectEnvVars(el, ATTR_ENCODING_STYLE, APPVAR_TYPE_STRING);

			returnValue = imsMessage;

		}
		 return returnValue;
	}
        
	protected void collectEnvVars(Element el, String attrName, String envVarType)
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
                                mEnvVariableMap.put(name, new String[]{null, envVarType});
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
