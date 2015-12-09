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
 * @(#)JDBCExtPreprocessDeserializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.extensions;


import java.util.Map;

import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;


import com.sun.jbi.internationalization.Messages;

import com.ibm.wsdl.util.xml.DOMUtils;
import org.glassfish.openesb.databasebc.extensions.JDBCConstants;

import org.w3c.dom.Element;

/**
 * @author Sun Microsystems
 */

public class DBExtPreprocessDeserializer extends JDBCExtSerializer {

    private static final Messages mMessages = Messages
        .getMessages(DBExtPreprocessDeserializer.class);
    
    private static final String APPVAR_TYPE_STRING = "STRING";
    private static final String APPVAR_TYPE_NUMBER = "NUMBER";
    
    // default constructor
    public DBExtPreprocessDeserializer() {
        super();
    }

    public DBExtPreprocessDeserializer(Map<String, String[]> envVariableMap) {
        super(envVariableMap);
    }

    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType, 
                                                                 QName elementType, 
                                                                 Element el, 
                                                                 Definition def,
                                                                 ExtensionRegistry extReg)
        throws WSDLException {

        ExtensibilityElement returnValue = null;

        if (JDBCConstants.QNAME_ADDRESS.equals(elementType)) {
            JDBCAddress jdbcAddress = new JDBCAddress();
            collectEnvVars(el, JDBCAddress.ATTR_JNDI_NAME, APPVAR_TYPE_STRING); 
            returnValue = jdbcAddress;

        } else if (JDBCConstants.QNAME_BINDING.equals(elementType)) {

        } else if (JDBCConstants.QNAME_OPERATION.equals(elementType)) {

        } else if (JDBCConstants.QNAME_OPERATION_INPUT.equals(elementType)) {
            JDBCOperationInput opInput = new JDBCOperationInput();
            collectEnvVars(el, JDBCOperationInput.ATTR_OPERATION_TYPE, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.ATTR_NUMBER_OF_RECORDS, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.ATTR_PARAM_ORDER, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.ATTR_SQL, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.PK_NAME, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.MARK_COLUMN_NAME, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.TABLE_NAME, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.MOVE_TABLE_NAME, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.POLLING_POST_PROCESS, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.MARK_COLUMN_VALUE, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.TRANSACTION, APPVAR_TYPE_STRING);
            collectEnvVars(el, JDBCOperationInput.POLLMILLISECONDS, APPVAR_TYPE_NUMBER);
            returnValue = opInput;
        } else if (JDBCConstants.QNAME_OPERATION_OUTPUT.equals(elementType)) {
            JDBCOperationOutput opOutput = new JDBCOperationOutput();
            collectEnvVars(el, JDBCOperationOutput.ATTR_RETURN_PART_NAME, APPVAR_TYPE_STRING);
            returnValue = opOutput;
        }
		
        //return returnValue;
        return null;
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
