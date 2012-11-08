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
 * @(#)ExecExtPreprocessDeserializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.extensions;

import java.util.Map;

import com.sun.jbi.internationalization.Messages;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

import com.ibm.wsdl.util.xml.DOMUtils;

import org.w3c.dom.Element;

/**
 *
 * @author Sherry Weng
 * @author Qian Fu jim.fu@sun.com
 */
public class ExecExtPreprocessDeserializer extends ExecExtSerializer {
    private static final Messages mMessages = Messages.getMessages(ExecExtPreprocessDeserializer.class);
    
    // default constructor
    public ExecExtPreprocessDeserializer() {
        super();
    }
    
    public ExecExtPreprocessDeserializer(Map envVariableMap) {
        super(envVariableMap);
    }
    
    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType,
            QName elementType,
            Element el,
            Definition def,
            ExtensionRegistry extReg)
            throws javax.wsdl.WSDLException {
        ExtensibilityElement returnValue = null;
        if (ExecConstants.QNAME_MESSAGE.equals(elementType)) {
            ExecMessage execMessage = new ExecMessage();
            
            collectEnvVars(el, ATTR_EXEC_USE, ExecConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_EXEC_ENCODING_STYLE, ExecConstants.ENV_VAR_TYPE_STRING);
            
            returnValue = execMessage;
        } else if (ExecConstants.QNAME_ADDRESS.equals(elementType)) {
            ExecAddress execAddress = new ExecAddress();
            
            collectEnvVars(el, ATTR_HOST_NAME, ExecConstants.ENV_VAR_TYPE_STRING);
            collectEnvVars(el, ATTR_USER_NAME, ExecConstants.ENV_VAR_TYPE_STRING);
            
            returnValue = execAddress;
        }
        
        return returnValue;
    }
    
    protected void collectEnvVars(Element el, String attrName, String envVarType) throws WSDLException {
        String s = DOMUtils.getAttribute(el, attrName);
        if (s != null) {
            try {
                if (hasMigrationEnvVarRef(s)) {
                    String token = s;
                    Object[] envVariableNames = getEnvVariableNames(attrName, s);
                    if ( envVariableNames != null ) {
                        for ( int i = 0; i < envVariableNames.length; i++ ) {
                            if (!mEnvVariableMap.containsKey(envVariableNames[i])) {
                                mEnvVariableMap.put(envVariableNames[i], new String[] {null, envVarType});
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
