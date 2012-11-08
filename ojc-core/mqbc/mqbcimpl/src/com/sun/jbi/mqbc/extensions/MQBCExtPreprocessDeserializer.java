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
 * @(#)MQBCExtPreprocessDeserializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.util.Map;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import com.ibm.wsdl.util.xml.DOMUtils;
import org.w3c.dom.Element;


/**
 *
 * @author rchen
 */
public class MQBCExtPreprocessDeserializer extends MQExtSerializer {
    private static final String APPVAR_TYPE_STRING = "STRING";
    private static final String APPVAR_TYPE_NUMBER = "NUMBER";

    /** Creates a new instance of MQBCExtPreprocessDeserializer */
     public MQBCExtPreprocessDeserializer(Map<String, String[]> envVariableMap) {
        super(envVariableMap);
    }
     
     public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType, 
                                                                 QName elementType,
                                                                 Element el, 
                                                                 Definition def, 
                                                                 ExtensionRegistry extReg) 
                                                                 throws javax.wsdl.WSDLException {
        ExtensibilityElement returnValue = null;
        if (MQConstants.QNAME_OPERATION.equals(elementType)) {
            MQBCOperation mqOperation = new MQBCOperation();
            collectEnvVars(el, MQBCOperation.ATTR_QUEUE_NAME, APPVAR_TYPE_STRING);     
            collectEnvVars(el, MQBCOperation.ATTR_QUEUE_OPEN_OPTIONS, APPVAR_TYPE_NUMBER);
            collectEnvVars(el, MQBCOperation.ATTR_POLLINGINTERVAL, APPVAR_TYPE_NUMBER);
            collectEnvVars(el, MQBCOperation.ATTR_TRANSACTION, APPVAR_TYPE_STRING);
            returnValue = mqOperation;
            
        } else if (MQConstants.QNAME_REDELIVERY.equals(elementType)) {
            MQBCRedelivery mqRedelivery = new MQBCRedelivery();
            collectEnvVars(el, MQBCRedelivery.ATTR_COUNT, APPVAR_TYPE_NUMBER);     
            collectEnvVars(el, MQBCRedelivery.ATTR_DELAY, APPVAR_TYPE_NUMBER);
            collectEnvVars(el, MQBCRedelivery.ATTR_TARGET, APPVAR_TYPE_STRING);
            returnValue = mqRedelivery;
            
        } else if (MQConstants.QNAME_ADDRESS.equals(elementType)) {
            MQBCAddress mqAddress = new MQBCAddress();
            collectEnvVars(el, MQBCAddress.ATTR_HOST_NAME, APPVAR_TYPE_STRING);
            collectEnvVars(el, MQBCAddress.ATTR_PORT_NUMBER, APPVAR_TYPE_NUMBER);
            collectEnvVars(el, MQBCAddress.ATTR_QUEUE_MANAGER_NAME, APPVAR_TYPE_STRING);
            collectEnvVars(el, MQBCAddress.ATTR_CHANNEL_NAME, APPVAR_TYPE_STRING);
            collectEnvVars(el, MQBCAddress.ATTR_CODED_CHARACTER_SET_ID, APPVAR_TYPE_NUMBER);
            collectEnvVars(el, MQBCAddress.ATTR_USER_ID, APPVAR_TYPE_STRING);
            collectEnvVars(el, MQBCAddress.ATTR_PASSWORD, APPVAR_TYPE_STRING);
            collectEnvVars(el, MQBCAddress.ATTR_CIPHERSUITE, APPVAR_TYPE_STRING);
            collectEnvVars(el, MQBCAddress.ATTR_SSLPEERNAME, APPVAR_TYPE_STRING);
            returnValue = mqAddress;
        }

        return returnValue;
    }
     
    protected void collectEnvVars(Element el, String attrName, String type)
            throws WSDLException {
        String s = DOMUtils.getAttribute(el, attrName);
        if (s != null) {
            try {
                if (hasMigrationEnvVarRef(s)) {
                    String[] envVariableNames = getEnvVariableNames(attrName, s);
                    if ( envVariableNames != null ) {
                        for (String name : envVariableNames) {
                            if (!mEnvVariableMap.containsKey(name)) {
                                mEnvVariableMap.put(name, new String[]{"", type});
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
