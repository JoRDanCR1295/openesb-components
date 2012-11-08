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
 * @(#)JMSAddress.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jmsbc.extensions;

import java.io.Serializable;
import java.net.URL;
import java.net.URLClassLoader;

import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author jtran
 */
public class JMSAddress implements ExtensibilityElement, Serializable {

    public static final String ATTR_CONNECTION_URL = "connectionURL";
    public static final String ATTR_USERNAME = "username";
    public static final String ATTR_PASSWORD = "password";
    public static final String ATTR_JNDI_CONNECTION_FACTORY_NAME = "connectionFactoryName";
    public static final String ATTR_JNDI_INITIAL_CONTEXT_FACTORY = "initialContextFactory";
    public static final String ATTR_JNDI_PROVIDER_URL            = "providerURL";
    public static final String ATTR_JNDI_SECURITY_PRINCIPAL      = "securityPrincipal";
    public static final String ATTR_JNDI_SECURITY_CRDENTIALS     = "securityCredentials";

    private static final long serialVersionUID = 1L;
    
    QName fieldElementType = JMSConstants.QNAME_ADDRESS;

    Boolean fieldRequired = null;

    // nested elements
    private JMSJNDIEnv jndiEnv;

    // jmsjca options element
    private JMSJCAOptions jmsjcaOptions;
    
    // attributes
    String connectionURL;
    String username;
    String password;
    String connectionFactoryName;
    String providerURL;
    String initialContextFactory;
    String securityPrincipal;
    String securityCredentials;
    
    String service;
    String endpoint;
    ClassLoader classloader;
    
    public JMSAddress() {
    }

    /** 
     * Get the extensibility element type
     * @return the extensibility element's type 
     */
    public QName getElementType() {
        return fieldElementType;
    }

    /** 
     * Set the extensibility element type
     * @param elementType the type 
     */
    public void setElementType(QName elementType) {
        fieldElementType = elementType;
    }
    
    /** 
     * Get whether required (for wsdl:required)
     */
    public Boolean getRequired() {
        return fieldRequired;
    }

    /** 
     * Set whether required (for wsdl:required) 
     */
    public void setRequired(Boolean required) {
        fieldRequired = required;
    }

    /**
     * Get jndienv extensiblity element
     * @return JMSJNDIEnv
     */
    public JMSJNDIEnv getJndiEnv() {
        return jndiEnv;
    }
    
    /**
     * Set options extensiblity element
     * @param val JMSOptions
     */
    public void setJndiEnv(JMSJNDIEnv val) {
        jndiEnv = val;
    }
    
    
    public String getConnectionURL() {
        return connectionURL;
    }

    public void setConnectionURL(String val) {
        connectionURL= val;
    }    
    
    public String getUsername() {
        return username;
    }

    public void setUsername(String val) {
        username= val;
    }        

    public String getPassword() {
        return password;
    }

    public void setPassword(String val) {
        password= val;
    }        
    
    public String getConnectionFactoryName() {
        return connectionFactoryName;
    }

    public void setConnectionFactoryName(String val) {
        connectionFactoryName= val;
    }        

    public String getInitialContextFactory() {
        return initialContextFactory;
    }
    
    public void setInitialContextFactory(String val) {
        initialContextFactory = val;
    }

    public String getProviderURL() {
        return providerURL;
    }
    
    public void setProviderURL(String val) {
        providerURL = val;
    }

    public String getSecurityPrincial() {
        return securityPrincipal;
    }
    
    public void setSecurityPrincipal(String val) {
        securityPrincipal = val;
    }

    public String getSecurityCredentials() {
        return securityCredentials;
    }
    
    public void setSecurityCredentials(String val) {
        securityCredentials = val;
    }
    
    public String getService() {
        return service;
    }
    
    public void setService (String val) {
        service = val;
    }
    
    public String getEndpoint() {
        return endpoint;
    }
    
    public void setEndpoint(String val) {
        endpoint = val;
    }
    
    public String toString() {
        StringBuffer strBuf = new StringBuffer(super.toString());
        strBuf.append("\nJMS address (" + fieldElementType + "):");
        strBuf.append("\nRequired=" + fieldRequired);
        strBuf.append("\nConnectionURL=" + connectionURL);
        strBuf.append("\nUsername=" + username);
        strBuf.append("\nPassword=" + password);
        strBuf.append("\nConnectionFactoryName=" + connectionFactoryName);
        strBuf.append("\nInitialContextFactory=" + initialContextFactory);
        strBuf.append("\nProviderURL=" + providerURL);
        strBuf.append("\nSecurityPrincipal=" + securityPrincipal);
        strBuf.append("\nSecurityCredentials=" + securityCredentials);
        strBuf.append("\nJNDIEnv=" + 
                (jndiEnv!=null?jndiEnv.toString():"null"));
        strBuf.append("\nService=" + service); 
        strBuf.append("\nEndpoint=" + endpoint); 
        return strBuf.toString();
    }

	public JMSJCAOptions getJmsjcaOptions() {
		return jmsjcaOptions;
	}

	public void setJmsjcaOptions(JMSJCAOptions jmsjcaOptions) {
		this.jmsjcaOptions = jmsjcaOptions;
	}

	public ClassLoader getClassLoader(){
		return classloader;
	}

	public void setClassLoader(ClassLoader cls){
		this.classloader = cls;
	}

}
