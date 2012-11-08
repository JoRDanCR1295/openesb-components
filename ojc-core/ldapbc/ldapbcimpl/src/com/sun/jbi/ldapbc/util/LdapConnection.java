
/*

 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.

 * 

 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.

 * 

 * The contents of this file are subject to the terms of either the GNU

 * General Public License Version 2 only ("GPL") or the Common

 * Development and Distribution License("CDDL") (collectively, the

 * "License"). You may not use this file except in compliance with the

 * License. You can obtain a copy of the License at

 * http://www.netbeans.org/cddl-gplv2.html

 * or nbbuild/licenses/CDDL-GPL-2-CP. See the License for the

 * specific language governing permissions and limitations under the

 * License.  When distributing the software, include this License Header

 * Notice in each file and include the License file at

 * nbbuild/licenses/CDDL-GPL-2-CP.  Sun designates this

 * particular file as subject to the "Classpath" exception as provided

 * by Sun in the GPL Version 2 section of the License file that

 * accompanied this code. If applicable, add the following below the

 * License Header, with the fields enclosed by brackets [] replaced by

 * your own identifying information:

 * "Portions Copyrighted [year] [name of copyright owner]"

 * 

 * If you wish your version of this file to be governed by only the CDDL

 * or only the GPL Version 2, indicate your decision by adding

 * "[Contributor] elects to include this software in this distribution

 * under the [CDDL or GPL Version 2] license." If you do not indicate a

 * single choice of license, a recipient has the option to distribute

 * your version of this file under either the CDDL, the GPL Version 2 or

 * to extend the choice of license to its licensees as provided above.

 * However, if you add GPL Version 2 code and therefore, elected the GPL

 * Version 2 license, then the option applies only if the new code is

 * made subject to such option by the copyright holder.

 * 

 * Contributor(s):

 * 

 * Portions Copyrighted 2008 Sun Microsystems, Inc.

 */
package com.sun.jbi.ldapbc.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.naming.Context;
import javax.naming.ldap.InitialLdapContext;
import javax.naming.ldap.LdapContext;
import javax.naming.ldap.StartTlsRequest;
import javax.naming.ldap.StartTlsResponse;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.component.jbiext.KeyStoreUtilClient;
import com.sun.jbi.ldapbc.LdapConnectionProperties;
import com.sun.jbi.ldapbc.OutboundMessageProcessor;
import com.sun.jbi.ldapbc.OperationMetaData;
import com.sun.jbi.ldapbc.PagedResultInfo;


/**

 *

 * @author Gary Zheng

 */
public class LdapConnection extends LdapConnectionProperties {

    private static final Logger mLogger = Messages.getLogger(LdapConnection.class);
    private static final Messages mMessages = Messages.getMessages(LdapConnection.class);
    
    private LdapContext connection;
    private String dn;
    public static String SSL_TYPE_NONE = "None";
    public static String SSL_TYPE_SSL = "Enable SSL";
    public static String SSL_TYPE_TLS = "TLS on demand";

    public LdapConnection() {
    }

    public String getDn() {
        return dn;
    }

    public void setDn(String dn) {
        this.dn = dn;
    }

    public boolean isProperty(String property) {

        String[] flds = getPropertyNames();
        for (int i = 0; i < flds.length; i++) {
            if (flds[i].equals(property)) {
                return true;
            }
        }
        return false;
    }

    public void setProperty(String property, String value) {

        try {
            Class cls = this.getClass();
            property = property.substring(0, 1).toUpperCase() + property.substring(1);
            Method method = null;
            method = cls.getMethod("set" + property, String.class);
            method.invoke(this, (Object[]) new String[]{value});

        } catch (IllegalAccessException ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalArgumentException ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvocationTargetException ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NoSuchMethodException ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        } catch (SecurityException ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        }

    }

    public Object getProperty(String property) {

        Class cls = this.getClass();
        property = property.substring(0, 1).toUpperCase() + property.substring(1);
        Method method = null;

        try {
            method = cls.getMethod("get" + property);
            return method.invoke(this, (java.lang.Object[]) null);

        } catch (IllegalAccessException ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalArgumentException ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvocationTargetException ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NoSuchMethodException ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        } catch (SecurityException ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        }
        return "";

    }

    private boolean isEmpty(String str) {

        if (null == str) {
            return true;
        }
        if (str.length() == 0) {
            return true;
        }
        return false;
    }
    
    private boolean isEmpty(Integer str) {

        if (null == str) {
            return true;
        }
        return false;
    }    

    public void closeConnection() {

        try {
            if (connection != null) {
                connection.close();
                connection = null;
            }
            PagedResultInfo.removeCookie(toString());
        } catch (Exception ex) {
            ex.printStackTrace();
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public LdapContext getConnection(OperationMetaData meta, KeyStoreUtilClient mKeyStoreUtil) throws Exception {

        if (null != connection && !meta.isIsConnectionRecreate()) {
            return connection;
        }

        closeConnection();
        clearSystemProperties(meta);
        LdapContext ret = null;

        try {

            if (!isEmpty(this.getTruststore())) {
            	mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0220_Trust_Store", this.getTruststore()));
                System.setProperty("javax.net.ssl.trustStore", this.getTruststore());
            }
            if (!isEmpty(this.getTruststoretype())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0222_Trust_Store_Type", this.getTruststoretype()));
                System.setProperty("javax.net.ssl.trustStoreType", this.getTruststoretype());
            }
            if (!isEmpty(this.getTruststorepassword())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0221_Trust_Store_Pwd", mKeyStoreUtil.encrypt(this.getTruststorepassword())));
                System.setProperty("javax.net.ssl.trustStorePassword", this.getTruststorepassword());
            }
            if (!isEmpty(this.getKeystore())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0223_Key_Store", this.getKeystore()));
                System.setProperty("javax.net.ssl.keyStore", this.getKeystore());
            }
            if (!isEmpty(this.getKeystorepassword())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0224_Key_Store_Pwd", mKeyStoreUtil.encrypt(this.getKeystorepassword())));
                System.setProperty("javax.net.ssl.keyStorePassword", this.getKeystorepassword());
            }
            if (!isEmpty(this.getKeystoreusername())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0225_Key_Store_User", this.getKeystoreusername()));
                System.setProperty("javax.net.ssl.keyStoreUsername", this.getKeystoreusername());
            }
            if (!isEmpty(this.getKeystoretype())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0226_Key_Store_Type", this.getKeystoretype()));
                System.setProperty("javax.net.ssl.keyStoreType", this.getKeystoretype());
            }
            Hashtable<String, String> env = new Hashtable<String, String>();

            if (!isEmpty(this.getPrincipal())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0216_Principal", this.getPrincipal()));
                env.put(Context.SECURITY_PRINCIPAL, this.getPrincipal());
            }
            if (!isEmpty(this.getCredential())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0217_Credential",  mKeyStoreUtil.encrypt(this.getCredential())));
                env.put(Context.SECURITY_CREDENTIALS, this.getCredential());
            }

            env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
            env.put(Context.PROVIDER_URL, this.getLocation());
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0215_URL", this.getLocation()));

            if (!isEmpty(this.getAuthentication())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0218_Authentication", this.getAuthentication()));
                env.put(Context.SECURITY_AUTHENTICATION, this.getAuthentication());
            }

            if (!isEmpty(this.getProtocol())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0219_Protocol", this.getProtocol()));
                env.put(Context.SECURITY_PROTOCOL, this.getProtocol());
            }

            if (!isEmpty(meta.getReferral())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0232_Referral", meta.getReferral()));
                env.put(Context.REFERRAL, meta.getReferral());
            }
            
            if(meta.getAllowConnectionPooling().booleanValue()){
            	mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0238_ALLOW_CONNPOOL", meta.getAllowConnectionPooling().toString()));
            	if(!(!isEmpty(this.getSsltype())&& this.getSsltype().equals(LdapConnection.SSL_TYPE_TLS)) ||
            		this.getTlssecurity().toUpperCase().equals("YES")){
           	
	            	env.put("com.sun.jndi.ldap.connect.pool", "true");
	            	
	                if (!isEmpty(meta.getConnectionPoolPrefSize())){
	    				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0233_CONNPOOL_PREFSIZE", meta.getConnectionPoolPrefSize().toString()));
	                	System.setProperty("com.sun.jndi.ldap.connect.pool.prefsize", meta.getConnectionPoolPrefSize().toString());
	                }
	                
	                if (!isEmpty(meta.getConnectionPoolMaxSize())){
	    				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0234_CONNPOOL_MAXSIZE", meta.getConnectionPoolMaxSize().toString()));
	                	System.setProperty("com.sun.jndi.ldap.connect.pool.maxsize", meta.getConnectionPoolMaxSize().toString());
	                }
	                
	                if (!isEmpty(meta.getConnectionMaxIdleTimeout())){
	    				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0235_CONNPOOL_MAX_IDLETIME", meta.getConnectionMaxIdleTimeout().toString()));
	                	System.setProperty("com.sun.jndi.ldap.connect.pool.timeout", meta.getConnectionMaxIdleTimeout().toString());
	                }                
	                
	                if (!isEmpty(meta.getConnectionProtocol())){
	    				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0236_CONNPOOL_PROTOCOL", meta.getConnectionProtocol()));
	                	System.setProperty("com.sun.jndi.ldap.connect.pool.protocol", meta.getConnectionProtocol());
	                } 
	                
	                if (!isEmpty(meta.getConnectionAuthentication())){
	    				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0237_CONNPOOL_AUTHENTICATION", meta.getConnectionAuthentication()));
	                	System.setProperty("com.sun.jndi.ldap.connect.pool.authentication", meta.getConnectionAuthentication());
	                }  
                } else {
                	mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0239_CONNPOOL_NOT_ALLOWED"));
                }
            } else {
            	mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0238_ALLOW_CONNPOOL", meta.getAllowConnectionPooling().toString()));
            }

            int  retryInterval = meta.getRetryInterval();
            int retryCount = meta.getRetryCount(); 
            for ( int retry = 0; retry <= retryCount ; retry++ ) {
            try{
            	ret = new InitialLdapContext(env, null);
            }catch(javax.naming.CommunicationException conEx){
            		try {
                     if (retry < retryCount) {
                    	 Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE,
                    			 "Failed to establish connection : "+retry);
                    	 Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE,
                    			 "+++ Unable to establish connection ... will retry after: "+ retryInterval + " +++");
                    }
                    Thread.currentThread().sleep( retryInterval );
                }
                catch ( InterruptedException e ) {
                    ;
                }                
                if (retry == retryCount) {
                	Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE,
                			"Cannot establish connection after "+retry+ "times  check the external system for Connection");
                	Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, conEx.getMessage(), conEx);
               	}
            }
            if (null != ret)
            	break;
            }

            if (this.getTlssecurity().toUpperCase().equals("YES") ||
                    this.getSsltype().equals(LdapConnection.SSL_TYPE_TLS)) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0227_Tls_Security", this.getTlssecurity()));
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0217_Ssl_Type", this.getSsltype()));

                StartTlsResponse tls = (StartTlsResponse) ret.extendedOperation(new StartTlsRequest());
                tls.negotiate();
            }

            connection = ret;

        } catch (IllegalArgumentException ex1) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex1);
            throw new Exception(ex1.getMessage());
        } 
        return ret;

    }

    public LdapContext getConnection(KeyStoreUtilClient mKeyStoreUtil) {

        if (null != connection) {
            return connection;
        }

        clearSystemProperties();
        LdapContext ret = null;

        try {

            if (!isEmpty(this.getTruststore())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0220_Trust_Store", this.getTruststore()));
                System.setProperty("javax.net.ssl.trustStore", this.getTruststore());
            }
            if (!isEmpty(this.getTruststoretype())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0222_Trust_Store_Type", this.getTruststoretype()));
                System.setProperty("javax.net.ssl.trustStoreType", this.getTruststoretype());
            }
            if (!isEmpty(this.getTruststorepassword())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0221_Trust_Store_Pwd", mKeyStoreUtil.encrypt(this.getTruststorepassword())));
                System.setProperty("javax.net.ssl.trustStorePassword", this.getTruststorepassword());
            }
            if (!isEmpty(this.getKeystore())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0223_Key_Store", this.getKeystore()));
                System.setProperty("javax.net.ssl.keyStore", this.getKeystore());
            }
            if (!isEmpty(this.getKeystorepassword())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0224_Key_Store_Pwd", mKeyStoreUtil.encrypt(this.getKeystorepassword())));
                System.setProperty("javax.net.ssl.keyStorePassword", this.getKeystorepassword());
            }
            if (!isEmpty(this.getKeystoreusername())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0225_Key_Store_User", this.getKeystoreusername()));
                System.setProperty("javax.net.ssl.keyStoreUsername", this.getKeystoreusername());
            }
            if (!isEmpty(this.getKeystoretype())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0226_Key_Store_Type", this.getKeystoretype()));
                System.setProperty("javax.net.ssl.keyStoreType", this.getKeystoretype());
            }

            Hashtable<String, String> env = new Hashtable<String, String>();
            if (!isEmpty(this.getPrincipal())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0216_Principal", this.getPrincipal()));
                env.put(Context.SECURITY_PRINCIPAL, this.getPrincipal());
            }

            if (!isEmpty(this.getCredential())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0217_Credential", mKeyStoreUtil.encrypt(this.getCredential())));
                env.put(Context.SECURITY_CREDENTIALS, this.getCredential());
            }

            env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
            env.put(Context.PROVIDER_URL, this.getLocation());
			mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0215_URL", this.getLocation()));
			
            if (!isEmpty(this.getAuthentication())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0218_Authentication", this.getAuthentication()));
                env.put(Context.SECURITY_AUTHENTICATION, this.getAuthentication());
            }
            if (!isEmpty(this.getProtocol())) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0219_Protocol", this.getProtocol()));
                env.put(Context.SECURITY_PROTOCOL, this.getProtocol());
            }
            ret = new InitialLdapContext(env, null);

            if (this.getTlssecurity().toUpperCase().equals("YES") || this.getSsltype().equals(LdapConnection.SSL_TYPE_TLS)) {
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0227_Tls_Security", this.getTlssecurity()));
				mLogger.log(Level.INFO, mMessages.getString("LDAPBC_CFG0217_Ssl_Type", this.getSsltype()));
                StartTlsResponse tls = (StartTlsResponse) ret.extendedOperation(new StartTlsRequest());
                tls.negotiate();
            }

            ret = new InitialLdapContext(env, null);
            if (this.getTlssecurity().toUpperCase().equals("YES")) {
                StartTlsResponse tls = (StartTlsResponse) ret.extendedOperation(new StartTlsRequest());
                tls.negotiate();
            }
            connection = ret;

        } catch (Exception ex) {
            Logger.getLogger(LdapConnection.class.getName()).log(Level.SEVERE, null, ex);
        }
        return ret;

    }

    public void clearSystemProperties(OperationMetaData meta) {
        System.clearProperty("javax.net.ssl.trustStore");
        System.clearProperty("javax.net.ssl.trustStoreType");
        System.clearProperty("javax.net.ssl.trustStorePassword");
        System.clearProperty("javax.net.ssl.keyStore");
        System.clearProperty("javax.net.ssl.keyStorePassword");
        System.clearProperty("javax.net.ssl.keyStoreUsername");
        System.clearProperty("javax.net.ssl.keyStoreType");
        if(meta.getAllowConnectionPooling().booleanValue()){
        	System.clearProperty("com.sun.jndi.ldap.connect.pool.prefsize");
        	System.clearProperty("com.sun.jndi.ldap.connect.pool.maxsize");
        	System.clearProperty("com.sun.jndi.ldap.connect.pool.timeout");
        	System.clearProperty("com.sun.jndi.ldap.connect.pool.protocol");
        	System.clearProperty("com.sun.jndi.ldap.connect.pool.authentication");
        }
    }
    
    public void clearSystemProperties() {
        System.clearProperty("javax.net.ssl.trustStore");
        System.clearProperty("javax.net.ssl.trustStoreType");
        System.clearProperty("javax.net.ssl.trustStorePassword");
        System.clearProperty("javax.net.ssl.keyStore");
        System.clearProperty("javax.net.ssl.keyStorePassword");
        System.clearProperty("javax.net.ssl.keyStoreUsername");
        System.clearProperty("javax.net.ssl.keyStoreType");
    }    

//    public String toString() {
//        String ret = "";
//        ret += "localtion: " + this.getLocation() +
//                "\n principal: " + this.getPrincipal() +
//                "\n credential: " + this.getCredential() +
//                "\n ssltype: " + this.getSsltype() +
//                "\n authentication: " + this.getAuthentication() +
//                "\n protocol: " + this.getProtocol() +
//                "\n truststore: " + this.getTruststore() +
//                "\n truststorepassword: " + this.getTruststorepassword() +
//                "\n truststoretype: " + this.getTruststoretype() +
//                "\n keystorepassword: " + this.getKeystorepassword() +
//                "\n keystoreusername: " + this.getKeystoreusername() +
//                "\n keystore: " + this.getKeystore() +
//                "\n keystoretype: " + this.getKeystoretype() +
//                "\n tlssecurity: " + this.getTlssecurity();
//        return ret;
//    }
}

