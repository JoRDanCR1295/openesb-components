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
 * @(#)LDAPComponentContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ldapbc;

import com.sun.jbi.common.qos.messaging.MessagingChannel;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;

/**
 * singleton used to share the component context within the bc
 */
public class LDAPComponentContext {

	private static LDAPComponentContext instance = new LDAPComponentContext();
    
    private ComponentContext context;
    private MessagingChannel channel;
    private ComponentLifeCycle lifeCycle;

	public static final String NM_PROP_LDAP_USE_DYN_EP_BINDING = "org.glassfish.openesb.ldap.use.dynamic.endpoint";
    
    // ldap:address
    public static final String NM_PROP_LDAP_LOCATION = "org.glassfish.openesb.ldap.location";
    public static final String NM_PROP_LDAP_PRINCIPAL = "org.glassfish.openesb.ldap.principal";
    public static final String NM_PROP_LDAP_CREDENTIAL = "org.glassfish.openesb.ldap.credential";
    public static final String NM_PROP_LDAP_SSLTYPE = "org.glassfish.openesb.ldap.ssltype";
    public static final String NM_PROP_LDAP_AUTHENTICATION = "org.glassfish.openesb.ldap.authentication";
    public static final String NM_PROP_LDAP_PROTOCOL = "org.glassfish.openesb.ldap.protocol";
    public static final String NM_PROP_LDAP_TRUSTSTORE_LOC = "org.glassfish.openesb.ldap.truststore.location";
    public static final String NM_PROP_LDAP_TRUSTSTORE_PASSWORD = "org.glassfish.openesb.ldap.truststore.password";
    public static final String NM_PROP_LDAP_TRUSTSTORE_TYPE = "org.glassfish.openesb.ldap.truststore.type";
    public static final String NM_PROP_LDAP_KEYSTORE_LOC = "org.glassfish.openesb.ldap.keystore.location";
    public static final String NM_PROP_LDAP_KEYSTORE_PASSWORD = "org.glassfish.openesb.ldap.keystore.password";
    public static final String NM_PROP_LDAP_KEYSTORE_USERNAME = "org.glassfish.openesb.ldap.keystore.username";
    public static final String NM_PROP_LDAP_KEYSTORE_TYPE = "org.glassfish.openesb.ldap.keystore.type";
    public static final String NM_PROP_LDAP_TLS_SECURITY = "org.glassfish.openesb.ldap.tls.security";   
   
     /** Creates a new instance of LDAPComponentContext */
    private LDAPComponentContext() {
    }

	 public static LDAPComponentContext getInstance() {
        return instance;
    }

    public ComponentContext getContext() {
        return context;
    }

    public void setContext(ComponentContext context) {
        this.context = context;
    }
    
    
    /**
     * @return the component lifecycle associated with this context
     * if it has been initialized
     */
    public ComponentLifeCycle getAssociatedLifeCycle() {
        return lifeCycle;
    }
    
    /**
     * Set the component lifecycle associated with this context
     */
    public void setAssociatedLifeCycle(ComponentLifeCycle aLifeCycle) {
        lifeCycle = aLifeCycle;
    }

    
    
    /**
     * @return Obtain the channel associated with this context
     * if it has been initialized
     */
    public MessagingChannel getBindingChannel() {
        return channel;
    }
    
    /**
     * Set the initizalied channel associated with this context
     */
    public void setBindingChannel(MessagingChannel aChannel) {
        channel = aChannel;
    }    
      
}
