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
 * @(#)TaskXmlWriterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.mgmt.task;

import junit.framework.Test;
import junit.framework.TestSuite;

import com.sun.jbi.component.mgmt.task.ExceptionInfo;
import com.sun.jbi.component.mgmt.task.MsgLocInfo;
import com.sun.jbi.component.mgmt.task.TaskXmlWriter;
import com.sun.jbi.crl.util.Util;

/**
 *
 * @author Kevan Simpson
 */
public class TaskXmlWriterTest extends TaskMessageTestCase {
    /**
     * Constructs a test case for {@link TaskXmlWriter}.
     * @param testName The name of the test.
     */
    public TaskXmlWriterTest(String testName) {
        super(testName);
    }

    public Test suite() {
    	return new TestSuite(TaskXmlWriterTest.class);
    }
    
    protected void setUp() throws Exception {
        super.setUp();  // REQUIRED!
    }

    protected void tearDown() throws Exception {
    }

    /** Tests {@link TaskXmlWriter#visit(ExceptionInfo)}. */
    public void testWriteExceptionInfo() throws Exception {
        System.out.println("testWriteExceptionInfo()");
        MsgLocInfo msgLocInfo = new MsgLocInfo("test-token", "foo {0} msg {1}", 
                                               new Object[] { "test", "end" });
        Exception error = new Exception("testWriteExceptionInfo()");
        ExceptionInfo info = new ExceptionInfo(1, msgLocInfo, error);
        String expectedXml = getProperty("exception.info.1");
        // insert stack-trace content into xml read from properties file
        expectedXml = insertParam(expectedXml, "{error}", Util.escape(Util.toString(error)));
        // insert msg-loc-info content
        expectedXml = insertParam(expectedXml, "msg.loc.info.1", null);
        
        String actualXml = TaskXmlWriter.toXml(info);
        assertTrue("testWriteExceptionInfo()", compareXml(expectedXml, actualXml));
        System.out.println("PASSED!");
    }
    
    /** Tests {@link TaskXmlWriter#visit(ExceptionInfo)}. */
    public void testWriteMsgLocInfo() throws Exception {
        System.out.println("testWriteMsgLocInfo()");
        MsgLocInfo msgLocInfo = new MsgLocInfo("test-token", "foo '{0} msg '{1}", 
                                               new Object[] {});
        String expectedXml = getProperty("msg.loc.info.2");
        String actualXml = TaskXmlWriter.toXml(msgLocInfo);
        assertTrue("testWriteMsgLocInfo() - empty args", 
                   compareXml(expectedXml, actualXml));
        
        msgLocInfo = new MsgLocInfo("test-token", "foo '{0} msg '{1}", (Object[]) null);
        actualXml = TaskXmlWriter.toXml(msgLocInfo);
        assertTrue("testWriteMsgLocInfo() - null args", 
                   compareXml(expectedXml, actualXml));
        System.out.println("PASSED!");
    }
}
