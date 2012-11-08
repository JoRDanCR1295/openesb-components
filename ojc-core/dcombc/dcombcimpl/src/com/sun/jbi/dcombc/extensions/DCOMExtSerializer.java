/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.extensions;

import java.io.Serializable;
import java.io.PrintWriter;
import java.text.MessageFormat;
import java.util.Map;
import java.util.StringTokenizer;

import javax.wsdl.extensions.ExtensionDeserializer;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.extensions.ExtensionSerializer;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.Port;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOutput;
import javax.wsdl.BindingOperation;
import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.util.xml.DOMUtils;

import org.w3c.dom.Element;

import com.sun.jbi.internationalization.Messages;

/**
 * @author Chandrakanth Belde
 */
public class DCOMExtSerializer implements ExtensionSerializer, ExtensionDeserializer, Serializable {
	/**
	 * 
	 */
    private static final long serialVersionUID = 1L;

    public static final String ATT_UUID = "uuid";
    
    public static final String ATT_METHOD = "method";
    
    public static final String ATT_DOMAIN = "domain";
    
    public static final String ATT_SERVER = "server";
    
    public static final String ATT_USERNAME = "username";
    
    public static final String ATT_PASSWORD = "password";    
    
    public static final String ATT_TRANSACTION = "transaction";
    

    // environment variable configurations
    protected Map mEnvVariableMap;

    private static final Messages mMessages = Messages.getMessages(DCOMExtSerializer.class);

	/**
     * Creates a new instance of DCOMExtSerializer
     */
    public DCOMExtSerializer() {
    }

	/**
     * Creates a new instance of DCOMExtSerializer
     */
	public DCOMExtSerializer(Map envVariableMap) {
		this();
        mEnvVariableMap = envVariableMap;
    }

    /**
     * Contruction of Register Serializer class
     * 
     * @param ExtensionRegistry
     */
    public void registerSerializer(ExtensionRegistry registry) {

        // Register and map DCOM Binding
        registry.registerSerializer(Binding.class, DCOMConstants.QNAME_BINDING, this);
        registry.registerDeserializer(Binding.class, DCOMConstants.QNAME_BINDING, this);
        registry.mapExtensionTypes(Binding.class, DCOMConstants.QNAME_BINDING, DCOMBinding.class);

        // Register and map DCOM Operation
        registry.registerSerializer(BindingOperation.class, DCOMConstants.QNAME_OPERATION, this);
        registry.registerDeserializer(BindingOperation.class, DCOMConstants.QNAME_OPERATION, this);
        registry.mapExtensionTypes(BindingOperation.class, DCOMConstants.QNAME_OPERATION, DCOMOperation.class);

        // Register and map DCOM Input
        registry.registerSerializer(BindingInput.class, DCOMConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingInput.class, DCOMConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingInput.class, DCOMConstants.QNAME_MESSAGE, DCOMInput.class);

        // Register and map DCOM Output
        registry.registerSerializer(BindingOutput.class, DCOMConstants.QNAME_MESSAGE, this);
        registry.registerDeserializer(BindingOutput.class, DCOMConstants.QNAME_MESSAGE, this);
        registry.mapExtensionTypes(BindingOutput.class, DCOMConstants.QNAME_MESSAGE, DCOMOutput.class);

        // Register and map DCOM Address
        registry.registerSerializer(Port.class, DCOMConstants.QNAME_ADDRESS, this);
        registry.registerDeserializer(Port.class, DCOMConstants.QNAME_ADDRESS, this);
        registry.mapExtensionTypes(Port.class, DCOMConstants.QNAME_ADDRESS, DCOMAddress.class);
    }

    /**
     * Marshall the wsdl extensability elements
     * 
     * @param Class
     * @param QName
     * @param ExtensibilityElement
     * @param PrintWriter
     * @param Definition
     * @param ExtensionRegistry
     * @throws WSDLException
     */
    public void marshall(Class parentType,
                         QName elementType,
                         ExtensibilityElement extension,
                         PrintWriter pw,
                         javax.wsdl.Definition def,
                         ExtensionRegistry extReg) throws WSDLException {
        if (extension == null) {
            return;
        }

        if (extension instanceof DCOMBinding) {
            DCOMBinding DCOMBinding = (DCOMBinding) extension;
            pw.print("      <dcom:binding");
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }

            if (DCOMBinding.getUUID() != null) {
                DOMUtils.printAttribute(DCOMBinding.ATTR_UUID, DCOMBinding.getUUID(), pw);
            }
            
            pw.println("/>");
        } else if (extension instanceof DCOMOperation) {
            DCOMOperation DCOMOperation = (DCOMOperation) extension;
            pw.print("      <dcom:operation");
            
            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }

            if (DCOMOperation.getMethod() != null) {
                DOMUtils.printAttribute(DCOMOperation.ATTR_METHOD, DCOMOperation.getMethod(), pw);
            }
            
            pw.println("/>");
        } else if (extension instanceof DCOMMessage) {
            DCOMMessage DCOMMessage = (DCOMMessage) extension;
            pw.print("      <dcom:message");


            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }

            pw.println("/>");
        } else if (extension instanceof DCOMAddress) {
            DCOMAddress DCOMAddress = (DCOMAddress) extension;
            pw.print("      <dcom:address");

            if (DCOMAddress.getDomain() != null) {
                DOMUtils.printAttribute(DCOMAddress.ATTR_DOMAIN, DCOMAddress.getDomain(), pw);
            }
            if (DCOMAddress.getServer() != null) {
                DOMUtils.printAttribute(DCOMAddress.ATTR_SERVER, DCOMAddress.getServer(), pw);
            }
            if (DCOMAddress.getUsername() != null) {
                DOMUtils.printAttribute(DCOMAddress.ATTR_USERNAME, DCOMAddress.getUsername(), pw);
            }
            if (DCOMAddress.getPassword() != null) {
                DOMUtils.printAttribute(DCOMAddress.ATTR_PASSWORD, DCOMAddress.getPassword(), pw);
            }

            Boolean required = extension.getRequired();
            if (required != null) {
                DOMUtils.printQualifiedAttribute(Constants.Q_ATTR_REQUIRED, required.toString(), def, pw);
            }
            
            pw.println("/>");
        } 
    }

    /**
     * Unmarshall and element and return the extension type
     * 
     * @param Class
     * @param QName
     * @param Element
     * @param Definition
     * @param ExtensionRegistry
     * @return ExtensibilityElement
     * @throws WSDLException
     */

    public javax.wsdl.extensions.ExtensibilityElement unmarshall(Class parentType,
                                                                 QName elementType,
                                                                 Element el,
                                                                 Definition def,
                                                                 ExtensionRegistry extReg) throws WSDLException {

        ExtensibilityElement returnValue = null;

        if (DCOMConstants.QNAME_BINDING.equals(elementType)) {
            DCOMBinding DCOMBinding = new DCOMBinding();
            
            String mInterface = DOMUtils.getAttribute(el, DCOMBinding.ATTR_UUID);
            if (nonEmptyString(mInterface)) {
                DCOMBinding.setUUID(mInterface);
            }            
            returnValue = DCOMBinding;
        } else if (DCOMConstants.QNAME_OPERATION.equals(elementType)) {
            DCOMOperation DCOMOperation = new DCOMOperation();

            String method = DOMUtils.getAttribute(el, DCOMOperation.ATTR_METHOD);
            if (nonEmptyString(method)) {
                DCOMOperation.setMethod(method);
            }
            
            returnValue = DCOMOperation;
        } else if (DCOMConstants.QNAME_ADDRESS.equals(elementType)) {
            DCOMAddress DCOMAddress = new DCOMAddress();
            
            String domain = DOMUtils.getAttribute(el, DCOMAddress.ATTR_DOMAIN);
            if (nonEmptyString(domain)) {
                DCOMAddress.setDomain(domain);
            }
            
            String server = DOMUtils.getAttribute(el, DCOMAddress.ATTR_SERVER);
            if (nonEmptyString(server)) {
                DCOMAddress.setServer(server);
            }
            
            String username = DOMUtils.getAttribute(el, DCOMAddress.ATTR_USERNAME);
            if (nonEmptyString(username)) {
                DCOMAddress.setUsername(username);
            }
            
            String password = DOMUtils.getAttribute(el, DCOMAddress.ATTR_PASSWORD);
            if (nonEmptyString(password)) {
                DCOMAddress.setPassword(password);
            }

            returnValue = DCOMAddress;
        } else if (DCOMConstants.QNAME_MESSAGE.equals(elementType)) {
            DCOMMessage DCOMMessage = new DCOMMessage();

            returnValue = DCOMMessage;
        }
        return returnValue;
    }

	private boolean nonEmptyString(String strToTest) {
        boolean nonEmpty = false;
        if (strToTest != null && strToTest.length() > 0) {
            nonEmpty = true;
        }
        return nonEmpty;
    }
	
	public Map getEnvVariableMap() {
        return mEnvVariableMap;
    }

	protected boolean isAToken(String name) throws Exception {
    	boolean isToken = false;

        if (name.startsWith("${")) {
            if (name.endsWith("}")) {
                isToken = true;
            } else {
                throw new Exception(mMessages.getString("DCOMES.INVALID_TOKEN_NAME", name));
            }
        }

        return isToken;
    }

	protected String getEnvVariableName(String aToken) throws Exception {
        String tokenName = null;

        if (aToken == null || "".equals(aToken)) {
            throw new Exception(mMessages.getString("DCOMES.INVALID_TOKEN_NAME", aToken));
        }

        tokenName = aToken.substring(2, aToken.length() - 1);
        if ("".equals(tokenName)) {
            throw new Exception(mMessages.getString("DCOMES.INVALID_EMPTY_TOKEN_NAME", aToken));
        }

        return tokenName;

    }
}
