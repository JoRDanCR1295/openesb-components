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
 * @(#)ExecExtSerializer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.extensions;

import java.io.Serializable;
import java.io.PrintWriter;
import java.util.Map;

import com.sun.jbi.internationalization.Messages;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.Definition;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOutput;
import javax.wsdl.BindingOperation;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.w3c.dom.Element;

public class ExecExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {
    private static final long serialVersionUID = 1L;
    public static final String ATTR_COMMAND = "command";
    public static final String ATTR_EXEC_USE = "use";
    public static final String ATTR_EXEC_ENCODING_STYLE = "encodingStyle";
    public static final String ATTR_RECORDS_TO_BE_SKIPPED = "recordsToBeSkipped";
    public static final String ATTR_DELIMITERS_OF_RECORD = "delimitersOfRecord";
    public static final String ATTR_INJECT_CONTEXT_INFO = "injectContextInfo";
    public static final String ATTR_POLLING_INTERVAL = "pollingInterval";
    public static final String ATTR_POLLING_PATTERN = "pollingPattern";
    public static final String ATTR_HOST_NAME = "hostName";
    public static final String ATTR_USER_NAME = "userName";
    public static final String ATTR_PASSWORD = "password";

    private static final String ENV_VAR_REGEX = "\\$\\{([a-zA-Z0-9\\.\\-\\_^\\{\\}]+)\\}";
    private Pattern mPattern;
    
    // environment variable configurations
    protected Map mEnvVariableMap;
    private static final Messages mMessages = Messages.getMessages(ExecExtSerializer.class);
    
    /** Creates a new instance of ExecExtSerializer */
    public ExecExtSerializer() {
        mPattern = Pattern.compile(ENV_VAR_REGEX);
    }
    
    public ExecExtSerializer(Map envVariableMap) {
        this();
        mEnvVariableMap = envVariableMap;
    }
    
    /**
     * Registers the serializers / deserializers
     */
    public void registerSerializer(ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, ExecConstants.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, ExecConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, ExecConstants.QNAME_BINDING, ExecBinding.class);
        
        registry.registerSerializer(BindingOperation.class, ExecConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, ExecConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class, ExecConstants.QNAME_OPERATION, ExecOperation.class);
        
        registry.registerSerializer(BindingInput.class, ExecConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingInput.class, ExecConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingInput.class, ExecConstants.QNAME_MESSAGE, ExecMessage.class);
        
        registry.registerSerializer(BindingOutput.class, ExecConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingOutput.class, ExecConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingOutput.class, ExecConstants.QNAME_MESSAGE, ExecMessage.class);
        
        registry.registerSerializer(Port.class, ExecConstants.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, ExecConstants.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, ExecConstants.QNAME_ADDRESS, ExecAddress.class);
    }
    
    public Map getEnvVariableMap() {
        return mEnvVariableMap;
    }
    
    public void marshall(Class parentType,
            QName elementType,
            ExtensibilityElement extension,
            PrintWriter pw,
            javax.wsdl.Definition def,
            ExtensionRegistry extReg) throws WSDLException {
        if (extension == null) {
            return;
        }
        
        if (extension instanceof ExecBinding) {
            ExecBinding execBinding = (ExecBinding) extension;
            pw.print("      <exec:binding");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            pw.println("/>");
        } else if (extension instanceof ExecOperation) {
            ExecOperation execOperation = (ExecOperation) extension;
            pw.print("      <exec:operation");
            DOMUtils.printAttribute(ATTR_COMMAND,
                    execOperation.getCommand(),
                    pw);
            DOMUtils.printAttribute(ATTR_POLLING_INTERVAL,
                    String.valueOf(execOperation.getPollingInterval()),
                    pw);
            pw.println("/>");
        } else if (extension instanceof ExecMessage) {
            ExecMessage execMessage = (ExecMessage) extension;
            pw.print("      <exec:message");
            if (execMessage.getExecUseType() != null) {
                DOMUtils.printAttribute(ATTR_EXEC_USE,
                        execMessage.getExecUseType(),
                        pw);
            }
            
            if (execMessage.getExecEncodingStyle() != null) {
                DOMUtils.printAttribute(ATTR_EXEC_ENCODING_STYLE,
                        execMessage.getExecEncodingStyle(),
                        pw);
            }
            
            if (extension.getRequired() != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                        extension.getRequired().toString(),
                        def,
                        pw);
            }
            
            pw.println("/>");
        } else if (extension instanceof ExecAddress) {
            ExecAddress execAddress = (ExecAddress) extension;
            pw.print("      <exec:address");
            
            if (execAddress.getHostName() != null) {
                DOMUtils.printAttribute(ATTR_HOST_NAME,
                        execAddress.getHostName().toString(),
                        pw);
            }
            
            if (execAddress.getUserName() != null) {
                DOMUtils.printAttribute(ATTR_USER_NAME,
                        execAddress.getUserName(),
                        pw);
            }
            
            if (execAddress.getPassword() != null) {
                DOMUtils.printAttribute(ATTR_PASSWORD,
                        execAddress.getPassword(),
                        pw);
            }
            
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED,
                        required.toString(),
                        def,
                        pw);
            }
            
            pw.println("/>");
        }
    }
    
    
    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType,
            QName elementType,
            Element el,
            Definition def,
            ExtensionRegistry extReg)
            throws javax.wsdl.WSDLException {
        
        ExtensibilityElement returnValue = null;
        if (ExecConstants.QNAME_BINDING.equals(elementType)) {
            ExecBinding execBinding = new ExecBinding();
            returnValue = execBinding;
        } else if (ExecConstants.QNAME_OPERATION.equals(elementType)) {
            ExecOperation execOperation = new ExecOperation();
            
            String attr = getAttrAndResolveEnvVar(el, ATTR_COMMAND);
            if ( attr != null ) {
                execOperation.setCommand(attr);
            }
            attr = getAttrAndResolveEnvVar(el, ATTR_POLLING_INTERVAL);
            if ( attr != null ) {
                try {
                    execOperation.setPollingInterval(new Integer(attr));
                } catch (NumberFormatException e) {
                    throw new WSDLException("INVALID_WSDL",
                            mMessages.getString("FES_Invalid_number_value",
                                    new Object[]{attr, ATTR_POLLING_INTERVAL}));
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }
            }
            attr = getAttrAndResolveEnvVar(el, ATTR_POLLING_PATTERN);
            if ( attr != null ) {
                execOperation.setPollingPattern(attr);
            }
            
            returnValue = execOperation;
        } else if (ExecConstants.QNAME_MESSAGE.equals(elementType)) {
            ExecMessage execMessage = new ExecMessage();
            
            String attr = getAttrAndResolveEnvVar(el, ATTR_EXEC_USE);
            if ( attr != null ) {
                execMessage.setExecUseType(attr);
            }
            
            attr = getAttrAndResolveEnvVar(el, ATTR_EXEC_ENCODING_STYLE);
            if ( attr != null ) {
                execMessage.setExecEncodingStyle(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_RECORDS_TO_BE_SKIPPED);
            if ( attr != null ) {
                try {
                    execMessage.setRecordsToBeSkipped(new Integer(attr));
                } catch (NumberFormatException e) {
                    throw new WSDLException("INVALID_WSDL",
                            mMessages.getString("FES_Invalid_number_value",
                                    new Object[]{attr, ATTR_RECORDS_TO_BE_SKIPPED}));
                } catch (Exception e) {
                    throw new WSDLException("INVALID_WSDL", e.getMessage());
                }
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_DELIMITERS_OF_RECORD);
            if ( attr != null && attr.length() > 0) {
                execMessage.setDelimitersOfRecord(attr);
            }

            attr = getAttrAndResolveEnvVar(el, ATTR_INJECT_CONTEXT_INFO);
            if ( attr != null ) {
                execMessage.setInjectContextInfo(Boolean.parseBoolean(attr));
            }

            returnValue = execMessage;
        } else if (ExecConstants.QNAME_ADDRESS.equals(elementType)) {
            ExecAddress execAddress = new ExecAddress();
            
            String attr = getAttrAndResolveEnvVar(el, ATTR_HOST_NAME);
            
            if ( attr != null ) {
                execAddress.setHostName(attr);
            }
            
            attr = getAttrAndResolveEnvVar(el, ATTR_USER_NAME);
            
            if ( attr != null ) {
                execAddress.setUserName(attr);
            }
            
            attr = getAttrAndResolveEnvVar(el, ATTR_PASSWORD);
            
            if ( attr != null ) {
                execAddress.setPassword(attr);
            }
            
            returnValue = execAddress;
        }
        
        return returnValue;
    }
    
    String removeExtraEscapeCharacter(String delim) {
        String returnValue = delim;
        
        try {
            byte[] returnBytes = new byte[ delim.length() ];
            byte[] delimBytes = delim.getBytes("UTF-8");  // UTF-8
            int len  = delim.length();
            boolean found = false;
            int index = 0;
            for (int ii = 0; ii < len && ii + 1 < len; ii++) {
                if (delimBytes[ ii ] == '\\') {
                    if (delimBytes[ ii + 1 ] == 'r') {
                        returnBytes[index] = '\r';
                        ii++;
                        index++;
                        found = true;
                        continue;
                    }
                    if (delimBytes[ ii + 1 ] == 'n') {
                        returnBytes[index] = '\n';
                        ii++;
                        index++;
                        found = true;
                        continue;
                    }
                    if (delimBytes[ ii + 1 ] == 't') {
                        returnBytes[index] = '\t';
                        ii++;
                        index++;
                        found = true;
                        continue;
                    }
                    if (delimBytes[ ii + 1 ] == 'b') {
                        returnBytes[index] = '\b';
                        ii++;
                        index++;
                        found = true;
                        continue;
                    }
                    
                    if (delimBytes[ ii + 1 ] == 'f') {
                        returnBytes[index] = '\f';
                        if (++ii >= len) {break;}
                        index++;
                        found = true;
                        continue;
                    }
                } else {
                    returnBytes[ index ] = delimBytes[ ii ];
                    index++;
                }
            }
            if (found) {
                returnValue = new String(returnBytes, 0, index, "UTF-8");
            }
        } catch (Exception e) {
            // Only support UTF-8
        }
        
        return returnValue;
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
                throw new Exception(mMessages.getString("FES_Invalid_token_name", tokenName));
            }
            refs.add(tokenName);
        }
        
        if ( attrVal.indexOf("${}") >= 0 ) {
            throw new Exception(mMessages.getString("FES_Invalid_empty_token_name", new Object[] {attrVal, attrName}));
        }
        
        return refs.toArray();
    }
    
    protected String getAttrAndResolveEnvVar(Element el, String attrName) throws WSDLException {
        String attrVal = DOMUtils.getAttribute(el, attrName);
        if (attrVal != null) {
            try {
                if (hasMigrationEnvVarRef(attrVal)) {
                    // attribute contains env var reference(s)
                    String token = attrVal;
                    Object[] vars = getEnvVariableNames(attrName, attrVal);
                    if ( vars != null ) {
                        for ( int i = 0; i < vars.length; i++ ) {
                            String[] varDesc = (String[]) mEnvVariableMap.get(vars[i]);
                            if ( varDesc == null || varDesc.length != 2 ) {
                                throw new WSDLException("INVALID_WSDL",
                                        mMessages.getString("FES_Invalid_env_var_ref_no_def", new Object[] {vars[i], attrVal, attrName}));
                            } else {
                                // check if the de-referenced value has ${ in it
                                String varVal = varDesc[0];
                                if ( varVal.indexOf("${") >= 0 ) {
                                    throw new WSDLException("INVALID_WSDL",
                                            mMessages.getString("FES_Invalid_var_value_contains_var_ref", new Object[] {attrName, attrVal, vars[i], varVal}));
                                }
                                attrVal = attrVal.replace("${" + vars[i] + "}", varVal);
                            }
                        }
                    }
                    if ( hasMigrationEnvVarRef(attrVal) ) {
                        // still has ref un-resolved
                        throw new WSDLException("INVALID_WSDL",
                                mMessages.getString("FES_Invalid_attr_value_contains_unresolvable_ref", new Object[] {attrVal, attrName}));
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
}
