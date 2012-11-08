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
 * @(#)JMSJndiConnectionInfoRecord.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.jmsbc.recovery;

import java.util.Properties;

/**
 *
 * JNDI connection information
 */
public class JMSJndiConnectionInfoRecord extends JMSConnectionInfoRecord{

    private String connectionFactoryName;
    private String providerURL;
    private String initialContextFactory;
    private String securityPrincipal;
    private String securityCredentials;
    private Properties jndiEnv;
    
    public JMSJndiConnectionInfoRecord() {
        super();
    }
    
    public JMSJndiConnectionInfoRecord(String connectionURL,
                                       String username,
                                       String password,
                                       String jmsjcaOptions,
                                       String connectionFactoryName,
                                       String providerURL,
                                       String initialContextFactory,
                                       String securityPrincipal,
                                       String securityCredentials,
                                       Properties jndiEnv) {
        super (connectionURL, username, password, jmsjcaOptions);
        this.connectionFactoryName = connectionFactoryName;
        this.providerURL = providerURL;
        this.initialContextFactory = initialContextFactory;
        this.securityPrincipal = securityPrincipal;
        this.securityCredentials = securityCredentials;
        this.jndiEnv = jndiEnv;
    }

    public String getConnectionFactoryName() {
        return connectionFactoryName;
    }

    public String getProviderURL () {
        return providerURL;
    }
    
    public String getInitialContextFactory() {
        return initialContextFactory;
    }

    public String getSecurityPrincipal() {
        return securityPrincipal;
    }

    public String getSecurityCredentials() {
        return securityCredentials;
    }
    
    public Properties getJndiEnv() {
        return jndiEnv;
    }
    
    public void setConnectionFactoryName(String connectionFactoryName) {
        this.connectionFactoryName = connectionFactoryName;
    }
    
    public void setProviderURL(String providerURL) {
        this.providerURL = providerURL;
    }
    
    public void setInitialContextFactory(String initialContextFactory) {
        this.initialContextFactory = initialContextFactory;
    }

    public void setSecurityPrincipal(String securityPrincipal) {
        this.securityPrincipal = securityPrincipal;
    }

    public void setSecurityCredentials(String securityCredentials) {
        this.securityCredentials = securityCredentials;
    }
    
    public void setJndiEnv(Properties jndiEnv) {
        this.jndiEnv = jndiEnv;
    }    
    
    public boolean equals(ConnectionInfoRecord other) {
        boolean ret = false;        
        if (other instanceof JMSJndiConnectionInfoRecord) {
            JMSJndiConnectionInfoRecord otherJMSRec = (JMSJndiConnectionInfoRecord)other;
            ret = super.equals(other) &&
                  stringsAreEqual(this.connectionFactoryName, otherJMSRec.getConnectionFactoryName()) &&
                  stringsAreEqual(this.providerURL, otherJMSRec.getProviderURL()) &&
                  stringsAreEqual(this.initialContextFactory, otherJMSRec.getInitialContextFactory()) &&
                  stringsAreEqual(this.securityPrincipal, otherJMSRec.getSecurityPrincipal()) &&
                  stringsAreEqual(this.securityCredentials, otherJMSRec.getSecurityCredentials()) &&
                  propertiesAreEqual(this.jndiEnv, otherJMSRec.getJndiEnv());
        }        
        return ret;
    }
    
    public int hashCode() {
    	int hashCode = super.hashCode();
    	hashCode += connectionFactoryName==null?"".hashCode():connectionFactoryName.hashCode();
    	hashCode += providerURL==null?"".hashCode():providerURL.hashCode();
    	hashCode += initialContextFactory==null?"".hashCode():initialContextFactory.hashCode();
    	hashCode += securityPrincipal==null?"".hashCode():securityPrincipal.hashCode();
    	hashCode += securityCredentials==null?"".hashCode():securityCredentials.hashCode();
    	hashCode += (jndiEnv==null?"".hashCode():jndiEnv.hashCode());
    	return hashCode;
//        return super.hashCode() +
//        
//               connectionFactoryName==null?"".hashCode():connectionFactoryName.hashCode() +
//               providerURL==null?"".hashCode():providerURL.hashCode()+
//               initialContextFactory==null?"".hashCode():initialContextFactory.hashCode()+
//               securityPrincipal==null?"".hashCode():securityPrincipal.hashCode() +
//               securityCredentials==null?"".hashCode():securityCredentials.hashCode()+
//               (jndiEnv==null?"".hashCode():jndiEnv.hashCode());
    }
 
    private boolean propertiesAreEqual (Properties left, Properties right) {
        boolean propsEqual = false;
        if (left == null && right == null) {
            propsEqual = true;
        } else if (left != null && right != null) {
            propsEqual = left.equals(right);
        }
        return propsEqual;
    }
}
