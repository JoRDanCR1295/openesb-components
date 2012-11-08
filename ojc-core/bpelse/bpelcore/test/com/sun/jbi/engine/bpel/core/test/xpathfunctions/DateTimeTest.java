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
 * @(#)DateTimeTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.xpathfunctions;

import java.util.Date;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathContextFactory;

import com.sun.jbi.engine.bpel.core.bpel.xpath.functions.DateParserImpl;
import com.sun.jbi.engine.bpel.core.bpel.xpath.functions.XPath2Functions;

/*
 * Unit test for testing the date time functions for BPEL engine. The logic for this
 * test is to get the value from the engine and then compare that against the current
 * date/time instance.
 * 
 */
public class DateTimeTest extends TestCase {

    static final JXPathContextFactory JXPATH_FACTORY = JXPathContextFactory.newInstance();

    JXPathContext context = null;

    public static Test suite() {
        TestSuite suite = new TestSuite(DateTimeTest.class);

        return suite;
    }

    /**
     * @param testName
     */
    public DateTimeTest(String testName) {
        super(testName);
        context = JXPATH_FACTORY.newContext(null, null);
        context.setLenient(false);
        context.setFunctions(new XPath2Functions());
    }

    /* (non-Javadoc)
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        super.setUp();
    }

    /* (non-Javadoc)
     * @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    /**
     * 
     */
    public void testCurrentDateTime() {
        String value = (String) context.getValue("current-dateTime()");
        //System.out.println(value);
        verifyDate(value);
        value = value.substring(value.indexOf('T')+1);
        verifyTime(value);
    }

    /**
     * 
     */
    public void testCurrentDate() {
        String value = (String) context.getValue("current-date()");
        
        verifyDate(value);
    }

    private void verifyDate(String value) {
        assertTrue(value != null && !value.equals(""));
        Date date = DateParserImpl.parse(value);
        assertTrue(date != null);
        
        assertTrue(date.getDate() == getDate(value));
        // need to offset the month value by 1
        assertTrue((date.getMonth() + 1) == getMonth(value));
    }
    
    /**
     * 
     */
    public void testCurrentTime() {
        String value = (String) context.getValue("current-time()");
        verifyTime(value);
    }
    
    private void verifyTime(String value) {
        assertTrue(value != null && !value.equals(""));
        Date date = DateParserImpl.parseTime(value);
        assertTrue(date != null);
        
        int hour = Integer.parseInt(value.substring(0, 2));
        int min = Integer.parseInt(value.substring(3, 5));
        int secs = Integer.parseInt(value.substring(6, 8));
        
//        assertTrue(secs == calendar.get(Calendar.SECOND));
//        assertTrue(min == calendar.get(Calendar.MINUTE));
//        
        assertTrue(secs == date.getSeconds());
        assertTrue(min == date.getMinutes());
              
    }

    /**
     * @param value
     * @return
     */
    private int getYear(String value) {
        return Integer.parseInt(value.substring(0, 4));
    }

    /**
     * @param value
     * @return
     */
    private int getMonth(String value) {
        return Integer.parseInt(value.substring(5, 7));
    }

    /**
     * @param value
     * @return
     */
    private int getDate(String value) {
        return Integer.parseInt(value.substring(8, 10));
    }

    /**
     * @param value
     * @return
     */
    private int getHour(String value) {
        return Integer.parseInt(value.substring(11, 13));
    }

    /**
     * @param value
     * @return
     */
    private int getMin(String value) {
        return Integer.parseInt(value.substring(14, 16));
    }

    /**
     * @param value
     * @return
     */
    private int getSec(String value) {
        return Integer.parseInt(value.substring(17, 19));
    }

    /**
     * @param value
     * @return
     */
    private int getMilliSec(String value) {
        return Integer.parseInt(value.substring(20, 22));
    }
}
