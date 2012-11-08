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
 * @(#)JDBCAddressTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.extensions;

import junit.framework.*;

import javax.xml.namespace.QName;


/**
 *
 *
 */
public class JDBCAddressTest extends TestCase {
    JDBCAddress instance = null;

    public JDBCAddressTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
        instance = new JDBCAddress();
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(JDBCAddressTest.class);

        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.jdbcbc.extensions.JdbcAddress.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");

        // 1. testing the default value of element type
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/jdbc/",
                "address");
        QName result = instance.getElementType();
        Assert.assertEquals(expResult, result);

        // 2. testing setElementType
        final QName val = new QName("http://my-jdbc-address-test", "address");
        expResult = new QName("http://my-jdbc-address-test", "address");
        instance.setElementType(val);
        result = instance.getElementType();
        Assert.assertEquals(expResult, result);

        System.out.println(
            "Successfully tested setElementType and getElementType");
    }

    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.jdbcbc.extensions.JdbcAddress.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");

        final Boolean val = Boolean.TRUE;
        final Boolean expResult = Boolean.TRUE;
        instance.setRequired(val);

        final Boolean result = instance.getRequired();
        Assert.assertEquals(expResult, result);

        System.out.println("Successfully tested setRequired and getRequired");
    }

    /**
    * Test of setJndiName and getJndiName method, of class com.sun.jbi.jdbcbc.extensions.JdbcAddress.
    */
    public void testSetGetJndiName() {
        System.out.println("Testing setJndiName and getJndiName");

        final String val = "jdbc/conpool";
        ;

        final String expResult = "jdbc/conpool";
        instance.setJndiName(val);

        final String result = instance.getJndiName();
        Assert.assertEquals(expResult, result);
        System.out.println("Successfully tested setJndiName and getJndiName");
    }
}
