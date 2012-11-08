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
 * @(#)NameUtilTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.util;
import junit.framework.*;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.rmi.server.UID;
import java.util.HashSet;
import com.sun.jbi.engine.iep.core.runtime.util.NameUtil;
/*
 * NameUtilTest.java
 * JUnit based test
 *
 * Created on February 15, 2007, 5:10 PM
 */

/**
 *
 * @author rdwivedi
 */
public class NameUtilTest extends TestCase {
    
    public NameUtilTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(NameUtilTest.class);
        
        return suite;
    }

    /**
     * Test of isLegalName method, of class com.sun.jbi.engine.iep.core.runtime.util.NameUtil.
     */
    public void testIsLegalName() {
        
        assertTrue(NameUtil.isLegalName("The_Name"));
        
        assertFalse(NameUtil.isLegalName(""));
        assertFalse(NameUtil.isLegalName("boolean"));
        assertFalse(NameUtil.isLegalName(" THe Name"));
        
    }

    /**
     * Test of isKeyword method, of class com.sun.jbi.engine.iep.core.runtime.util.NameUtil.
     */
    public void testIsKeyword() {
        assertTrue(NameUtil.isKeyword("int"));
        
        assertFalse(NameUtil.isKeyword("KeyWords"));
        assertFalse(NameUtil.isKeyword("Name Word"));
        
        assertTrue(NameUtil.isKeyword("boolean"));
    }

    /**
     * Test of makeJavaId method, of class com.sun.jbi.engine.iep.core.runtime.util.NameUtil.
     */
    public void testMakeJavaId() {
        
        assertEquals("$A12", NameUtil.makeJavaId("$A12"));
        assertEquals("Space_Space", NameUtil.makeJavaId("Space Space"));
        assertEquals("_boolean", NameUtil.makeJavaId("boolean"));
        
        
    }

    /**
     * Test of isJavaKeyword method, of class com.sun.jbi.engine.iep.core.runtime.util.NameUtil.
     */
    public void testIsJavaKeyword() {
        assertFalse(NameUtil.isJavaKeyword("KeyWord"));
        assertFalse(NameUtil.isJavaKeyword("Name Word"));
        assertTrue(NameUtil.isJavaKeyword("int"));
        assertTrue(NameUtil.isJavaKeyword("boolean"));
    }

    

    /**
     * Test of makeAlphaNumId method, of class com.sun.jbi.engine.iep.core.runtime.util.NameUtil.
     */
    public void testMakeAlphaNumId() {
        
        assertEquals("123", NameUtil.makeAlphaNumId("123"));
        
         
    }

   
    
}
