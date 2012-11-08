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
 * @(#)TriggerDetailsTest.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain;

import junit.framework.TestCase;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.TriggerEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.impl.TriggerExImpl;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.impl.TriggerExImplTest;

/**
 * Unit test for TriggerDetails.
 * 
 * @author sunsoabi_edwong
 */
public class TriggerDetailsTest extends TestCase {

    private static final String QUARTZ_GROUP_STR = "QuartzGrouop";      //NOI18N
    private static final String GROUP_STR = "Group";                    //NOI18N
    private static final String DATEFORMAT_STR = "yyyy-MM-dd";          //NOI18N
    private static final String MODE_STR = "Mode";                      //NOI18N
    private static final String STARTING_STR = "Starting";              //NOI18N
    private static final String ENDING_STR = "Ending";                  //NOI18N
    private static final String TIMEZONE_STR = "America/Los_Angeles";   //NOI18N
    
    public TriggerDetailsTest(String testName) {
        super(testName);
    }

//    public void testGetGroup() {
//    }
//
//    public void testGetDateFormat() {
//    }
//
//    public void testGetDateFormatAsString() {
//    }
//
//    public void testGetMode() {
//    }
//
//    public void testGetStarting() {
//    }
//
//    public void testGetEnding() {
//    }
//
//    public void testGetTimezone() {
//    }
//
//    public void testGetTimezoneAsString() {
//    }
//
//    public void testGetTriggerEx() {
//    }
    
    private void setField(StringBuilder sb, String field) {
        sb.append((field != null) ? field.length() : 0).append(Stringable.DELIM)
                .append((field != null) ? field : "");                  //NOI18N
    }

    public void testReadObject() {
        TriggerDetails td = new TriggerDetails();
        StringBuilder data = new StringBuilder();
        data.append(TriggerDetails.CLASSID);
        setField(data, QUARTZ_GROUP_STR);
        setField(data, GROUP_STR);
        setField(data, DATEFORMAT_STR);
        setField(data, MODE_STR);
        setField(data, STARTING_STR);
        setField(data, ENDING_STR);
        setField(data, TIMEZONE_STR);
        TriggerEx trig = new TriggerExImpl();
        TriggerExImplTest.setTestWriteObject(trig);
        setField(data, trig.valueAsString());
        td.readObject(data.toString());
        assertEquals(QUARTZ_GROUP_STR, td.getQuartzGroup());
        assertEquals(GROUP_STR, td.getGroup());
        assertEquals(DATEFORMAT_STR, td.getDateFormatAsString());
        assertEquals(MODE_STR, td.getMode());
        assertEquals(STARTING_STR, td.getStarting());
        assertEquals(ENDING_STR, td.getEnding());
        assertNotNull(td.getTriggerEx());
        assertEquals(trig.valueAsString(), td.getTriggerEx().valueAsString());

        data = new StringBuilder();
        data.append(TriggerDetails.CLASSID);
        setField(data, null);
        setField(data, null);
        setField(data, null);
        setField(data, null);
        setField(data, null);
        setField(data, null);
        setField(data, null);
        setField(data, null);
        td.readObject(data.toString());
        assertEquals(null, td.getQuartzGroup());
        assertEquals(null, td.getGroup());
        assertEquals(null, td.getDateFormatAsString());
        assertEquals(null, td.getMode());
        assertEquals(null, td.getStarting());
        assertEquals(null, td.getEnding());
        assertEquals(null, td.getTriggerEx());
    }

    public void testWriteObject() {
        TriggerDetails td = new TriggerDetails();
        StringBuilder expected = new StringBuilder();
        expected.append(TriggerDetails.CLASSID);
        setField(expected, null);
        setField(expected, null);
        setField(expected, null);
        setField(expected, null);
        setField(expected, null);
        setField(expected, null);
        setField(expected, null);
        setField(expected, null);
        StringBuilder data = new StringBuilder();
        td.writeObject(data);
        assertEquals(expected.toString(), data.toString());

        expected = new StringBuilder();
        expected.append(TriggerDetails.CLASSID);
        setField(expected, QUARTZ_GROUP_STR);
        setField(expected, GROUP_STR);
        setField(expected, DATEFORMAT_STR);
        setField(expected, MODE_STR);
        setField(expected, STARTING_STR);
        setField(expected, ENDING_STR);
        setField(expected, TIMEZONE_STR);
        TriggerEx trig = new TriggerExImpl();
        TriggerExImplTest.setTestWriteObject(trig);
        setField(expected, trig.valueAsString());
        data = new StringBuilder();
        td.setQuartzGroup(QUARTZ_GROUP_STR);
        td.setGroup(GROUP_STR);
        td.setDateFormat(DATEFORMAT_STR);
        td.setMode(MODE_STR);
        td.setStarting(STARTING_STR);
        td.setEnding(ENDING_STR);
        td.setTimezone(TIMEZONE_STR);
        td.setTriggerEx(trig);
        td.writeObject(data);
        assertEquals(expected.toString(), data.toString());
    }

//    public void testValueOf() {
//    }
//
//    public void testValueAsString() {
//    }

}
