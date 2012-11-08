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
 * @(#)LDAPTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.ldap;

import java.util.Hashtable;
import javax.naming.Context;
import javax.naming.NameClassPair;
import javax.naming.NamingEnumeration;
import javax.naming.NamingException;
import javax.naming.directory.Attribute;
import javax.naming.directory.Attributes;
import javax.naming.directory.DirContext;
import javax.naming.directory.InitialDirContext;
import javax.naming.directory.SearchControls;
import javax.naming.directory.SearchResult;
import junit.framework.*;

/**
 *
 * @author radval
 */
public class LDAPTest extends TestCase {
    
    public LDAPTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }
    
    public void testLDAPNoAuthentication() {
        //RIT this is disabled till we get back to ldap
//        Hashtable env = new Hashtable();
//        
//        env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
//        env.put(Context.PROVIDER_URL, "ldap://etldev1.stc.com:29219/dc=stc,dc=com");
//
//        DirContext ctx = null;
//        try {
//            ctx = new InitialDirContext(env);
//            String name = "ou=People";
//            NamingEnumeration list = ctx.list("ou=People");
//            assertTrue("has more objects under ou=People" + name, list.hasMore());
//            printNameClass(list);
//            
//            
//            SearchControls controls = new SearchControls();
//            controls.setSearchScope(SearchControls.OBJECT_SCOPE);
//            controls.setReturningAttributes(null);
//        
//            NamingEnumeration names = ctx.search("uid=JSimon,ou=People", "(objectClass=*)", controls);
//     
//            assertTrue("Got some names", names.hasMoreElements());
//            
//            //test first object only
//            while (names.hasMoreElements()) {
//                SearchResult result = (SearchResult) names.nextElement();
//                Attributes attrs = result.getAttributes();
//                assertTrue("Got some attributes", attrs.size() != 0);
//                printAttributes(attrs);
//                break;
//            }
//        } catch (Exception e) {
//            e.printStackTrace();
//            fail(e.getMessage());
//        }
//        finally {
//            try {
//                ctx.close();
//            } catch (NamingException e1) {
//                e1.printStackTrace();
//                fail(e1.getMessage());
//            }            
//        }
    }

    
    private void printNameClass(NamingEnumeration list) throws Exception {
        while (list.hasMore()) {
            NameClassPair nc = (NameClassPair)list.next();
            System.out.println(nc);
        }
    }
    
    
    private void printAttributes(Attributes attrs) throws Exception {
        NamingEnumeration attren = attrs.getAll();
        while (attren.hasMoreElements()) {
            Attribute attr = (Attribute) attren.nextElement();
            String value = (String) attr.get();   
            System.out.println("Attr Name:"+ attr.getID() + " value:" + value);
        }
    }
}
