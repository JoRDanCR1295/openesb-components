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
 * @(#)$Id: LDAPSearchUtil.java,v 1.2 2010/02/15 19:24:14 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.util;

import java.util.Hashtable;

import javax.naming.Context;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.DirContext;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;

/**
 * 
 * The Utility class for LDAP search
 * 
 */
public class LDAPSearchUtil {

    public static final String LDAPURL_PREFIX = "ldap://";

    public static final String LDAPSURL_PREFIX = "ldaps://";

    /**
     * Get a pooled LDAP context
     * 
     * @param loginType
     *            login type, "simple" or "none"
     * @param loginDN
     *            required if loginType=="simple"
     * @param password
     *            required if loginType=="simple"
     * @param isSSL
     *            true if SSL
     * @param ldapHost
     *            host address
     * @param ldapPort
     *            ldap port
     * @param ldapsPort
     *            ldaps port
     * @return A pooled LDAP context
     * @throws NamingException
     */
    public static DirContext getLdapContext(String loginType, String loginDN,
            String password, boolean isSSL, String ldapHost, String ldapPort,
            String ldapsPort) throws NamingException {

        Hashtable<String, String> env = new Hashtable<String, String>();
        env.put(Context.INITIAL_CONTEXT_FACTORY,
                "com.sun.jndi.ldap.LdapCtxFactory");
        env.put(Context.SECURITY_AUTHENTICATION, loginType);
        if (loginDN != null) {
            env.put(Context.SECURITY_PRINCIPAL, loginDN);
        }
        if (password != null) {
            env.put(Context.SECURITY_CREDENTIALS, password);
        }
        env.put("com.sun.jndi.ldap.connect.pool", "true");
        if (isSSL) {
            env.put(Context.PROVIDER_URL, LDAPSURL_PREFIX + ldapHost + ":"
                    + ldapsPort);
            env.put(Context.SECURITY_PROTOCOL, "ssl");
        } else {
            env.put(Context.PROVIDER_URL, LDAPURL_PREFIX + ldapHost + ":"
                    + ldapPort);
        }
        DirContext ctx = null;
        try {
             ctx = new InitialDirContext(env);
        } catch (NamingException ex) {
            if (ctx != null) {
                ctx.close();
            }
            throw ex;
        }
        return ctx;
    }

    /**
     * Get a specific attribute (email, manager) using a filter that has uid as
     * argument.
     * 
     * @param filter
     *            Filter used for search. eg. (uid=%s), %s will be substitued
     *            with uid
     * @param uidValue
     *            The uid value to substitue the %s in filter
     * @param targetAttribute
     *            The target attribute
     * @param ctx
     *            The Ldap search context
     * @param baseDN
     *            The baseDN
     * @param scopeType
     *            The search scope type, default is "sub"
     * @return
     * @throws Exception
     */
    public static String getAttributeByUID(String filter, String uidValue,
            String targetAttribute, DirContext ctx, String baseDN,
            String scopeType) throws Exception {
        String value = null;
        try {
            int scope = SearchControls.SUBTREE_SCOPE;
            if (scopeType != null) {
                if (scopeType.equalsIgnoreCase("one")) {
                    scope = SearchControls.ONELEVEL_SCOPE;
                } else if (scopeType.equalsIgnoreCase("base")) {
                    scope = SearchControls.OBJECT_SCOPE;
                }
            }
            SearchControls controls = new SearchControls();
            controls = new SearchControls();
            controls.setSearchScope(scope);
            controls.setReturningAttributes(new String[] { targetAttribute });
            filter = filter.replaceAll("%s", uidValue);
            NamingEnumeration<SearchResult> names = ctx.search(baseDN, filter,
                    controls);
            while (names.hasMoreElements()) {
                SearchResult result = (SearchResult) names.nextElement();
                Attributes attrs = result.getAttributes();
                NamingEnumeration attren = attrs.getAll();
                while (attren.hasMoreElements()) {
                    Attribute attr = (Attribute) attren.nextElement();
                    value = attr.get().toString();
                    break;
                }
                break;
            }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            throw e;
        } finally {
            try {
                if (ctx != null)
                    ctx.close();
            } catch (NamingException e1) {
                // TODO Auto-generated catch block
                throw e1;
            }
        }

        return value;
    }

    /**
     * Get attribute value of the object using the DN of the object
     * 
     * @param dn
     *            The DN of the object
     * @param targetAttribute
     *            The target attribute name
     * @param ctx
     *            The Ldap search context
     * @return The value of the attribute
     * @throws Exception
     */
    public static String getAttributeByDN(String dn, String targetAttribute,
            DirContext ctx) throws Exception {
        String value = null;
        try {
            int scope = SearchControls.OBJECT_SCOPE;

            SearchControls controls = new SearchControls();
            controls = new SearchControls();
            controls.setSearchScope(scope);
            controls.setReturningAttributes(new String[] { targetAttribute });

            NamingEnumeration<SearchResult> names = ctx.search(dn,
                    "(objectClass=*)", controls);
            while (names.hasMoreElements()) {
                SearchResult result = (SearchResult) names.nextElement();
                Attributes attrs = result.getAttributes();
                NamingEnumeration attren = attrs.getAll();
                while (attren.hasMoreElements()) {
                    Attribute attr = (Attribute) attren.nextElement();
                    value = attr.get().toString();
                    break;
                }
                break;
            }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            throw e;
        } finally {
            try {
                if (ctx != null)
                    ctx.close();
            } catch (NamingException e1) {
                // TODO Auto-generated catch block
                throw e1;
            }
        }

        return value;
    }

    /**
     * Get the DN of the object given uid
     * 
     * @param filter
     *            Filter used for search. eg. (uid=%s), %s will be substitued
     *            with uid
     * @param uidValue
     *            The uid value to substitue the %s in filter
     * @param ctx
     *            The Ldap search context
     * @param baseDN
     *            The baseDN
     * @param scopeType
     *            The search scope type, default is "sub"
     * @return The DN of the object
     * @throws Exception
     */
    public static String getDNByUID(String filter, String uidValue,
            DirContext ctx, String baseDN, String scopeType) throws Exception {
        String value = null;
        try {
            int scope = SearchControls.SUBTREE_SCOPE;
            if (scopeType != null) {
                if (scopeType.equalsIgnoreCase("one")) {
                    scope = SearchControls.ONELEVEL_SCOPE;
                } else if (scopeType.equalsIgnoreCase("base")) {
                    scope = SearchControls.OBJECT_SCOPE;
                }
            }
            SearchControls controls = new SearchControls();
            controls = new SearchControls();
            controls.setSearchScope(scope);
            controls.setReturningAttributes(null);
            filter = filter.replaceAll("%s", uidValue);
            NamingEnumeration<SearchResult> names = ctx.search(baseDN, filter,
                    controls);
            while (names.hasMoreElements()) {
                SearchResult result = (SearchResult) names.nextElement();
                String name = result.getName();
                if (name.trim().length() > 0) {
                    name = name + ",";
                }
                value = name + baseDN;
                break;
            }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            throw e;
        } finally {
            try {
                if (ctx != null)
                    ctx.close();
            } catch (NamingException e1) {
                // TODO Auto-generated catch block
                throw e1;
            }
        }
        return value;
    }

    public static void main(String[] args) {
        
        try {
            //uid=john, email=?
            DirContext ctx = getLdapContext ("none", null, null, false, "localhost", "389", null);
            String value = getAttributeByUID("(uid=%s)", "john", "mail", ctx, "dc=example,dc=com", "sub");
            System.out.println("John's email:" + value);
            //uid=john, manager=?
            ctx = getLdapContext ("none", null, null, false, "localhost", "389", null);
            value = getAttributeByUID("(uid=%s)", "john", "manager", ctx, "dc=example,dc=com", "sub");
            System.out.println("John's manager:" + value);
            
            //uid=john, manager.uid=?
            ctx = getLdapContext ("none", null, null, false, "localhost", "389", null);
            value = getAttributeByUID("(uid=%s)", "john", "manager", ctx, "dc=example,dc=com", "sub");            
            String managerDN = value;
            ctx = getLdapContext ("none", null, null, false, "localhost", "389", null);
            value = getAttributeByDN(managerDN, "uid", ctx);
            System.out.println("John's manager's uid:" + value);
            
            //uid=john, manager.mail=?
            ctx = getLdapContext ("none", null, null, false, "localhost", "389", null);
            value = getAttributeByUID("(uid=%s)", "john", "manager", ctx, "dc=example,dc=com", "sub");            
            managerDN = value;
            ctx = getLdapContext ("none", null, null, false, "localhost", "389", null);
            value = getAttributeByDN(managerDN, "mail", ctx);
            System.out.println("John's manager's mail:" + value);            
            
            //uid=john, john.dn=?
            ctx = getLdapContext ("none", null, null, false, "localhost", "389", null);
            value=getDNByUID ("(uid=%s)", "john", ctx,  "dc=example,dc=com", "sub");
            System.out.println("John's dn:" + value);            
            
        } catch (NamingException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
}
