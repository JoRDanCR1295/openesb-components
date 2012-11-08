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
 * @(#)JDBCSqlTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.extensions;

import com.ibm.wsdl.Constants;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import javax.xml.namespace.QName;


public class JDBCSqlTest extends TestCase {
    JDBCSql instance = null;

    //@Override
	public void setUp() throws Exception {
        instance = new JDBCSql();
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(JDBCSqlTest.class);

        return suite;
    }

    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");

        // 1. testing default element type value
        QName expResult = new QName("http://schemas.stc.com/jbi/wsdl-extensions/jdbc/",
                Constants.ELEM_OPERATION);
        QName result = instance.getElementType();

        //assertEquals(expResult, result);

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

    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");

        final Boolean expResult = Boolean.TRUE;
        instance.setRequired(Boolean.TRUE);

        final Boolean result = instance.getRequired();
        Assert.assertEquals(expResult, result);
        System.out.println("Successfully tested setRequired and getRequired");
    }

    public void testSetGetParamOrder() {
        System.out.println("Testing GetParamOrder and setParamOrder");

        final String paramOrder = "col1,col2";
        instance.setParamOrder(paramOrder);
        Assert.assertEquals(paramOrder, instance.getParamOrder());
        System.out.println(
            "Successfully tested GetParamOrder and setGetParamOrder");
    }

    public void testSetGetSql() {
        System.out.println("Testing setJDBCSql and getJDBCSql");

        final String sql = "insert into emp values(10,'hi','hello',20,500,'mgr',20)";
        instance.setSql(sql);
        Assert.assertEquals(sql, instance.getSql());
        System.out.println("Successfully tested setJDBCSql and getJDBCSql");
    }

    public void testSetGetTransaction() {
        System.out.println("Testing Transaction");

        final String transaction = "XATransaction";
        instance.setTransaction(transaction);
        Assert.assertEquals(transaction, instance.getTransaction());
        System.out.println("Successfully tested transaction");
    }

    public void testSetGetPollingPostProcessing() {
        System.out.println("Testing PollingPostProcessing");

        final String pollingPostProcess = "delete";
        instance.setMoveRowToTableName(pollingPostProcess);
        Assert.assertEquals(pollingPostProcess, instance.getMoveRowToTableName());
        System.out.println("Successfully tested pollingPostProcessing");
    }

    public void testSetGetMoveRowToTableName() {
        System.out.println("Testing MoveRowTableName");

        final String movetableName = "empDept";
        instance.setMoveRowToTableName(movetableName);
        Assert.assertEquals(movetableName, instance.getMoveRowToTableName());
        System.out.println("Successfully tested MoveRowTableName");
    }

    public void testSetGetTableName() {
        System.out.println("Testing TableName");

        final String tableName = "emp";
        instance.setTableName(tableName);
        Assert.assertEquals(tableName, instance.getTableName());
        System.out.println("Successfully tested TableName");
    }

    public void testSetGetMarkColumnValue() {
        System.out.println("Testing setMarkColumnvalue");

        final String columnValue = "True";
        instance.setMarkColumnValue(columnValue);
        Assert.assertEquals(columnValue, instance.getMarkColumnValue());
        System.out.println("Successfully tested MarkColumnValue");
    }

    public void testSetGetMarkColumnName() {
        System.out.println("Testing MarkColumnName");

        final String flagName = "flag";
        instance.setMarkColumnName(flagName);
        Assert.assertEquals(flagName, instance.getMarkColumnName());
        System.out.println("Successfully tested setMarkColumnName");
    }

    public void testSetGetPKName() {
        System.out.println("Testing setPKName and GetPKName");

        final String pkName = "col1";
        instance.setPKName(pkName);
        Assert.assertEquals(pkName, instance.getPKName());
        System.out.println("Successfully tested setPKName and GetPKName");
    }
}
