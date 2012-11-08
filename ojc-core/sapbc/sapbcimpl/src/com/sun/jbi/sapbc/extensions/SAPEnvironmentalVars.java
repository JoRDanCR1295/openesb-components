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
 * @(#)SAPEnvironmentalVars.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;


import java.util.Map;

import com.sun.jbi.internationalization.Messages;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 *
 * @author Julie Knight
 */
public class SAPEnvironmentalVars {
    /*
<sap:binding transactionalMode="Non-Transactional"/>
<sap:address applicationServerHostname="sap50uni" clientNumber="800" systemNumber="00" systemID="EUC" user="DEMO" password="DEMO" language="EN" 
     enableABAPDebugWindow="No" isSAPSystemUnicode="Yes" gatewayHostname="sap50uni" gatewayService="sapgw00" routerString="routerstr">
   <sap:clientparams useLoadBalancing="No" applicationServerGroup="appServGroup" messageServerHostname="msgServHostName"/>
   <sap:serverparams programID=""/>
</sap:address>
     */
    
    private static final long serialVersionUID = 1L;
    
    private static final String ENV_VAR_REGEX = "\\$\\{([a-zA-Z0-9\\.\\-\\_^\\{\\}]+)\\}";
    private Pattern mPattern;
    
    // environment variable configurations
    protected Map mEnvVariableMap;
    private static final Messages mMessages = Messages.getMessages(SAPEnvironmentalVars.class);
    
    /**
     * Creates a new instance of SAPEnvironmentalVars.
     *
     * @param envVariableMap  The environmental variable map from the 
     *                        RuntimeConfiguration. The element whose extensibility elements are searched
     */
    public SAPEnvironmentalVars(Map envVariableMap) {
        mPattern = Pattern.compile(ENV_VAR_REGEX);
        mEnvVariableMap = envVariableMap;
    }
    
    /**
     * Returns the environmental variable map.
     *
     * @return The environmental variable map.
     */
    public Map getEnvVariableMap() {
        return mEnvVariableMap;
    }
    
            
    protected boolean hasMigrationEnvVarRef(String attrVal) throws Exception {
        return mPattern.matcher(attrVal).find();
    }
    
    protected Object[] getEnvVariableNames(String attrName, String attrVal) throws Exception {
        String tokenName = null;
        Matcher m = mPattern.matcher(attrVal);
        Vector refs = new Vector();
        while ( m.find() ) {
            tokenName = m.group(1);
            if (tokenName == null || tokenName.trim().length() == 0 ) {
                throw new Exception(mMessages.getString("SAPEnvironmentalVar.Invalid_token_name", tokenName));
            }
            refs.add(tokenName);
        }
        
        if ( attrVal.indexOf("${}") >= 0 ) {
            throw new Exception(mMessages.getString("SAPEnvironmentalVar.Invalid_empty_token_name", new Object[] {attrVal, attrName}));
        }
        
        return refs.toArray();
    }
    
    protected String getAttrAndResolveEnvVar(Map attrMap, String attrName) throws WSDLException {
        String attrVal = (String) attrMap.get(attrName);
        if (attrVal != null) {
            try {
                if (hasMigrationEnvVarRef(attrVal)) {
                    // attribute contains env var reference(s)
                    String token = attrVal;
                    Object[] vars = getEnvVariableNames(attrName, attrVal);
                    if ( vars != null ) {
                        for ( int i = 0; i < vars.length; i++ ) {
                            String varVal = (String) mEnvVariableMap.get(vars[i]);
                            if (varVal == null) {
                                throw new WSDLException("INVALID_WSDL",
                                        mMessages.getString("SAPEnvironmentalVar.Invalid_env_var_ref_no_def", new Object[] {vars[i], attrVal, attrName}));
                            } else {
                                // check if the de-referenced value has ${ in it
                                if ( varVal.indexOf("${") >= 0 ) {
                                    throw new WSDLException("INVALID_WSDL",
                                            mMessages.getString("SAPEnvironmentalVar.Invalid_var_value_contains_var_ref", new Object[] {attrName, attrVal, vars[i], varVal}));
                                }
                                attrVal = attrVal.replace("${" + vars[i] + "}", varVal);
                            }
                        }
                    }
                    if ( hasMigrationEnvVarRef(attrVal) ) {
                        // still has ref un-resolved
                        throw new WSDLException("INVALID_WSDL",
                                mMessages.getString("SAPEnvironmentalVar.Invalid_attr_value_contains_unresolvable_ref", new Object[] {attrVal, attrName}));
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
    
    /**
     * Retrieves the environmental variables from the WSDL.
     *
     * @param elementType  The class of the qualifying extensibility element
     * @param attrMap      The attribute map for the extensibility elements
     *
     * @throws WSDLException 
     */
    public void preProcessEnvVars(QName elementType, Map attrMap)
            throws javax.wsdl.WSDLException {

        if (SAPAddress.TYPE.equals(elementType)) {            
            collectEnvVars(attrMap, SAPAddress.ATTR_APPSERVER_NAME);
            collectEnvVars(attrMap, SAPAddress.ATTR_CLIENT_NUMBER);
            collectEnvVars(attrMap, SAPAddress.ATTR_SYSTEM_NUMBER);
            collectEnvVars(attrMap, SAPAddress.ATTR_SYSTEM_ID);
            collectEnvVars(attrMap, SAPAddress.ATTR_USERNAME);
            collectEnvVars(attrMap, SAPAddress.ATTR_PASSWORD);
            collectEnvVars(attrMap, SAPAddress.ATTR_LANGUAGE);
            collectEnvVars(attrMap, SAPAddress.ATTR_ENABLE_ABAP_DEBUG);
            collectEnvVars(attrMap, SAPAddress.ATTR_IS_SAP_UNICODE);
            collectEnvVars(attrMap, SAPAddress.ATTR_GATEWAY_NAME);
            collectEnvVars(attrMap, SAPAddress.ATTR_GATEWAY_SERVICE);
            collectEnvVars(attrMap, SAPAddress.ATTR_ROUTER_STRING);
        } else if (SAPAddressClient.TYPE.equals(elementType)) {
            collectEnvVars(attrMap, SAPAddressClient.ATTR_USE_LOAD_BALANCING);
            collectEnvVars(attrMap, SAPAddressClient.ATTR_USE_LOAD_BALANCING);
            collectEnvVars(attrMap, SAPAddressClient.ATTR_MESSAGE_SERVER_HOSTNAME);
        } else if (SAPAddressServer.TYPE.equals(elementType)) {
            collectEnvVars(attrMap, SAPAddressServer.ATTR_USE_PROGRAM_ID);
        } else if (SAPBinding.TYPE.equals(elementType)) {
            collectEnvVars(attrMap, SAPBinding.ATTR_TRANSACTIONAL_MODE);
            collectEnvVars(attrMap, SAPBinding.ATTR_TRANSACTION_ID_DB);
            collectEnvVars(attrMap, SAPBinding.ATTR_MAX_TID_ROWS);
        }
    }
    
    protected void collectEnvVars(Map attrMap, String attrName) throws WSDLException {
        String s = (String) attrMap.get(attrName);
        
        if (s != null) {
            try {
                if (hasMigrationEnvVarRef(s)) {
                    String token = s;
                    Object[] envVariableNames = getEnvVariableNames(attrName, s);
                    if ( envVariableNames != null ) {
                        for ( int i = 0; i < envVariableNames.length; i++ ) {
                            if (!mEnvVariableMap.containsKey(envVariableNames[i])) {
                                mEnvVariableMap.put(envVariableNames[i], null);
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
