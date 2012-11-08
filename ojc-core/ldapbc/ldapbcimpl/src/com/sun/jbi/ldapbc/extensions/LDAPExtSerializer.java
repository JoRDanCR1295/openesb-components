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
 * @(#)LDAPExtSerializer.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ldapbc.extensions;

import java.util.LinkedList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.io.PrintWriter;
import java.io.Serializable;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.xml.namespace.QName;

import org.w3c.dom.Element;

import com.ibm.wsdl.util.xml.DOMUtils;
import com.sun.jbi.internationalization.Messages;


/**
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class LDAPExtSerializer implements ExtensionSerializer,
        ExtensionDeserializer, Serializable {
    /**
     *
     */
    private static final long serialVersionUID = 1L;

	private static final String ENV_VAR_REGEX = "\\$\\{([a-zA-Z0-9\\.\\-\\_^\\{\\}]+)\\}";

	private static final Messages mMessages = Messages.getMessages(LDAPExtSerializer.class);

	public static final String ATTR_LOCATION = "location";
    public static final String ATTR_PRINCIPAL = "principal";
    public static final String ATTR_CREDENTIAL = "credential";
	public static final String ATTR_SSL_TYPE = "ssltype";
	public static final String ATTR_AUTHENTICATION= "authentication";
	public static final String ATTR_PROTOCOL = "protocol";
	public static final String ATTR_TRUSTSTORE = "truststore";
	public static final String ATTR_TRUSTSTORE_PWD= "truststorepassword";
	public static final String ATTR_TRUSTSTORE_TYPE= "truststoretype";
	public static final String ATTR_KEYSTORE= "keystore";
	public static final String ATTR_KEYSTORE_PWD= "keystorepassword";
	public static final String ATTR_KEYSTORE_USERNAME= "keystoreusername";
	public static final String ATTR_KEYSTORE_TYPE= "keystoretype";
	public static final String ATTR_TLS_SECURITY= "tlssecurity";

    private static final Pattern mPattern = Pattern.compile(ENV_VAR_REGEX);
    
 	 // environment variable configurations
    protected final Map<String, String[]> mEnvVariableMap = new HashMap<String, String[]>();

    /** Creates a new instance of LDAPExtSerializer */
    public LDAPExtSerializer() {
    }

    public LDAPExtSerializer(Map<String, String[]> envVariableMap) {
		this();
		mEnvVariableMap.putAll(envVariableMap);
    }
    
    /**
     *
     * @param registry
     */
    public void registerSerializer(final ExtensionRegistry registry) {
        registry.registerSerializer(Binding.class, LDAPConstants.QNAME_BINDING,
                this);
        registry.registerDeserializer(Binding.class,
                LDAPConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, LDAPConstants.QNAME_BINDING,
                LDAPBinding.class);
        registry.registerSerializer(BindingOperation.class,
                LDAPConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class,
                LDAPConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class,
                LDAPConstants.QNAME_OPERATION, LDAPOperation.class);
        
        registry.registerSerializer(BindingInput.class,
                LDAPConstants.QNAME_OPERATION_INPUT, this);
        registry.registerDeserializer(BindingInput.class,
                LDAPConstants.QNAME_OPERATION_INPUT, this);
        registry.mapExtensionTypes(BindingInput.class,
                LDAPConstants.QNAME_OPERATION_INPUT, LDAPOperationInput.class);
        registry.registerSerializer(BindingOutput.class,
                LDAPConstants.QNAME_OPERATION_OUTPUT, this);
        registry.registerDeserializer(BindingOutput.class,
                LDAPConstants.QNAME_OPERATION_OUTPUT, this);
        registry.mapExtensionTypes(BindingOutput.class,
                LDAPConstants.QNAME_OPERATION_OUTPUT, LDAPOperationOutput.class);
        
        registry.registerSerializer(Port.class, LDAPConstants.QNAME_ADDRESS,
                this);
        registry.registerDeserializer(Port.class, LDAPConstants.QNAME_ADDRESS,
                this);
        registry.mapExtensionTypes(Port.class, LDAPConstants.QNAME_ADDRESS,
                LDAPAddress.class);
    }

	public Map<String, String[]> getEnvVariableMap() {
        return Collections.unmodifiableMap(mEnvVariableMap);
    }
    
    /**
     * @param parentType
     * @param elementType
     * @param extension
     * @param pw
     * @param def
     * @param extReg
     * @throws WSDLException
     */
    public void marshall(final Class parentType, final QName elementType,
            final ExtensibilityElement extension, final PrintWriter pw, final Definition def,
            final ExtensionRegistry extReg) throws WSDLException {
        
        
    }
    
    /**
     * @param parentType
     * @param elementType
     * @param el
     * @param def
     * @param extReg
     * @throws WSDLException
     */
    public ExtensibilityElement unmarshall(final Class parentType, final QName elementType,
            final Element el, final Definition def, final ExtensionRegistry extReg)
            throws WSDLException {
        ExtensibilityElement returnValue = null;
        
        if (LDAPConstants.QNAME_BINDING.equals(elementType)) {
            final LDAPBinding ldapBinding = new LDAPBinding();
            returnValue = ldapBinding;
        } else if (LDAPConstants.QNAME_OPERATION.equals(elementType)) {
            final LDAPOperation ldapOperation = new LDAPOperation();
             final String operationType = DOMUtils.getAttribute(el,
                    LDAPOperation.ATTR_OPERATION_TYPE);
            
            if (operationType != null) {
                ldapOperation.setOperationType(operationType);
            }
            returnValue = ldapOperation;
        } else if (LDAPConstants.QNAME_OPERATION_INPUT.equals(elementType)) {
            final LDAPOperationInput input = new LDAPOperationInput();
           
            returnValue = input;
        } else if (LDAPConstants.QNAME_OPERATION_OUTPUT.equals(elementType)) {
            final LDAPOperationOutput output = new LDAPOperationOutput();
            
            final String returnPartName = DOMUtils.getAttribute(el,
                    LDAPOperationOutput.ATTR_RETURN_PART_NAME);

            final String attributes = DOMUtils.getAttribute(el, 
                    LDAPOperationOutput.ATTR_ATTRIBUTES);

            if (returnPartName != null) {
                output.setReturnPartName(returnPartName);
            }
            
            output.setAttributes(attributes);
            
            returnValue = output;
        } else if (LDAPConstants.QNAME_ADDRESS.equals(elementType)) {
            final LDAPAddress ldapAddress = new LDAPAddress();
            String[] fields = ldapAddress.getPropertyNames();
            for (int i = 0; i < fields.length; i++) {
                String field = fields[i];
                String value = getAttrAndResolveEnvVar(el, field);
                if (null != value) {
                    ldapAddress.setProperty(field, value);
                }
            }
             returnValue = ldapAddress;
        }
        
        return returnValue;
    }
	
	
	protected boolean hasMigrationEnvVarRef(String attrVal) throws Exception {
        return mPattern.matcher(attrVal).find();
    }
    
    protected String[] getEnvVariableNames(String attrName, String attrVal) throws Exception {
		if ( attrVal.indexOf("${}") != -1 ) {
            throw new Exception(mMessages.getString("LDAPBC-E00103.Invalid_Empty_Token_Name", new Object[]{attrVal, attrName}));
		}
		String tokenName;
		Matcher m = mPattern.matcher(attrVal);
		List<String> refs = new LinkedList<String>();
		while ( m.find() ) {
			tokenName = m.group(1);
			if (tokenName != null) {
				tokenName = tokenName.trim();
			}
			refs.add(tokenName);
		}

		return refs.toArray(new String[refs.size()]);
    }

	  protected String getAttrAndResolveEnvVar(Element el, String attrName) throws WSDLException {
        String attrVal = DOMUtils.getAttribute(el, attrName);
        if (attrVal != null) {
            try {
				// attribute contains env var reference(s)
				String token = attrVal;
				String[] vars = getEnvVariableNames(attrName, attrVal);
				for (String var : vars) {
					String[] valAndType =  mEnvVariableMap.get(var);
					String varVal = valAndType[0];
					if (varVal == null) {
						throw new WSDLException("INVALID_WSDL", mMessages.getString(
								"LDAPBCES_Invalid_env_var_value_null", new Object[] { var, attrName }));
					}
					if (varVal.indexOf("${") >= 0) {
                                    throw new WSDLException("INVALID_WSDL",
                                            mMessages.getString("LDAPBC-E00105.Invalid_Var_Value_Contains_Var_Ref", new Object[] {attrName, attrVal, varVal}));
					}
					attrVal = attrVal.replace("${" + var + "}", varVal);
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