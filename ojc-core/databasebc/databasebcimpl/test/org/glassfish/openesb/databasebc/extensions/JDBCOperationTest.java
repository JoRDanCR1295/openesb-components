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
 * @(#)JDBCOperationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.extensions;

import com.ibm.wsdl.Constants;

import junit.framework.*;

import javax.xml.namespace.QName;


/**
 *
 *
 */
public class JDBCOperationTest extends TestCase {
    JDBCOperation instance = null;

    public JDBCOperationTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
        instance = new JDBCOperation();
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(JDBCOperationTest.class);

        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class org.glassfish.openesb.databasebc.extensions.JdbcOperation.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");

        // 1. testing default element type value
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/jdbc/",
                Constants.ELEM_OPERATION);
        QName result = instance.getElementType();
        Assert.assertEquals(expResult, result);

        // 2. testing setElementType
        final QName val = new QName("http://jdbc-operation-test/",
                Constants.ELEM_OPERATION);
        expResult = new QName("http://jdbc-operation-test/",
                Constants.ELEM_OPERATION);
        instance.setElementType(val);
        result = instance.getElementType();
        Assert.assertEquals(expResult, result);

        System.out.println(
            "Successfully tested setElementType and getElementType");
    }

    /**
     * Test of setRequired and setgetRequired method, of class org.glassfish.openesb.databasebc.extensions.JdbcOperation.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");

        final Boolean expResult = Boolean.TRUE;
        instance.setRequired(Boolean.TRUE);

        final Boolean result = instance.getRequired();
        Assert.assertEquals(expResult, result);

        System.out.println("Successfully tested setRequired and getRequired");
    }
}
