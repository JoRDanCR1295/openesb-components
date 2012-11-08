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

package com.sun.jbi.ldapbc.extensions;

import java.util.Map;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.xml.namespace.QName;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;
import org.w3c.dom.Element;


/**
 *
 * @author rchen
 */
public class LDAPExtPreprocessDeserializer extends LDAPExtSerializer {
    private static final Messages mMessages = Messages.getMessages(LDAPExtPreprocessDeserializer.class);
    private static final String APPVAR_TYPE_STRING = "STRING";
    private static final String APPVAR_TYPE_NUMBER = "NUMBER";

    /** Creates a new instance of LDAPExtPreprocessDeserializer */
    public LDAPExtPreprocessDeserializer() {
         super();
    }
    
     public LDAPExtPreprocessDeserializer(Map<String, String[]> envVariableMap) {
        super(envVariableMap);
    }
     
     public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType, 
                                                                 QName elementType,
                                                                 Element el, 
                                                                 Definition def, 
                                                                 ExtensionRegistry extReg) 
                                                                 throws javax.wsdl.WSDLException {
        ExtensibilityElement returnValue = null;
		if (LDAPConstants.QNAME_ADDRESS.equals(elementType)) {
            LDAPAddress ldapAddress = new LDAPAddress();           
                        
            collectEnvVars(el, ATTR_LOCATION, LDAPConstants.APP_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_PRINCIPAL, LDAPConstants.APP_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_CREDENTIAL, LDAPConstants.APP_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_SSL_TYPE, LDAPConstants.APP_VAR_TYPE_STRING);
			collectEnvVars(el, ATTR_AUTHENTICATION, LDAPConstants.APP_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_PROTOCOL, LDAPConstants.APP_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_TRUSTSTORE, LDAPConstants.APP_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_TRUSTSTORE_PWD, LDAPConstants.APP_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_TRUSTSTORE_TYPE, LDAPConstants.APP_VAR_TYPE_STRING);
			collectEnvVars(el, ATTR_KEYSTORE, LDAPConstants.APP_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_KEYSTORE_PWD, LDAPConstants.APP_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_KEYSTORE_USERNAME, LDAPConstants.APP_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_KEYSTORE_TYPE, LDAPConstants.APP_VAR_TYPE_STRING);     
			collectEnvVars(el, ATTR_TLS_SECURITY, LDAPConstants.APP_VAR_TYPE_BOOLEAN);
            returnValue = ldapAddress;
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
                    if ( envVariableNames != null ) {
                        for (Object envVariableName : envVariableNames) {
                            String name = envVariableName.toString();
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
