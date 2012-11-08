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
 * @(#)TriggerExImplTest.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain.wsdl4jext.impl;

import junit.framework.TestCase;
import org.glassfish.openesb.schedulerbc.domain.Stringable;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.TriggerEx;

/**
 * Unit test for TriggerExImpl.
 *
 * @author sunsoabi_edwong
 */
public class TriggerExImplTest extends TestCase {
    public static final String NAME_STR =  "Name";                      //NOI18N
    public static final String TYPE_STR =  "Type";                      //NOI18N
    public static final String DESCRIPTION_STR =  "Description";        //NOI18N
    public static final String REPEAT_STR =  "Repeat";                  //NOI18N
    public static final String INTERVAL_STR =  "Interval";              //NOI18N
    public static final String CRONEXPR_STR =  "CronExpr";              //NOI18N
    public static final String DURATION_STR =  "Duration";              //NOI18N
    public static final String MESSAGE_STR =  "Message";                //NOI18N
    
    public TriggerExImplTest(String testName) {
        super(testName);
    }
//
//    public void testGetName() {
//    }
//
//    public void testSetName() {
//    }
//
//    public void testGetType() {
//    }
//
//    public void testSetType() {
//    }
//
//    public void testIsEnabled() {
//    }
//
//    public void testSetEnabled() {
//    }
//
//    public void testGetDescription() {
//    }
//
//    public void testSetDescription() {
//    }
//
//    public void testGetRepeat() {
//    }
//
//    public void testSetRepeat() {
//    }
//
//    public void testGetInterval() {
//    }
//
//    public void testSetInterval() {
//    }
//
//    public void testGetCronExpr() {
//    }
//
//    public void testSetCronExpr() {
//    }
//
//    public void testGetDuration() {
//    }
//
//    public void testSetDuration() {
//    }
//
//    public void testSetElementType() {
//    }
//
//    public void testGetElementType() {
//    }
//
//    public void testSetRequired() {
//    }
//
//    public void testGetRequired() {
//    }
//
//    public void testGetMessage() {
//    }
//
//    public void testSetMessage() {
//    }

    private void setField(StringBuilder sb, String field) {
        sb.append((field != null) ? field.length() : 0).append(Stringable.DELIM)
                .append((field != null) ? field : "");                  //NOI18N
    }

    private void setField(StringBuilder sb, boolean field) {
        sb.append(Boolean.toString(field).length()).append(Stringable.DELIM)
                .append(field);
    }

    public void testReadObject() {
        TriggerExImpl trig = new TriggerExImpl();
        StringBuilder data = new StringBuilder();
        data.append(TriggerExImpl.CLASSID);
        setField(data, NAME_STR);
        setField(data, TYPE_STR);
        setField(data, true);
        setField(data, DESCRIPTION_STR);
        setField(data, REPEAT_STR);
        setField(data, INTERVAL_STR);
        setField(data, CRONEXPR_STR);
        setField(data, DURATION_STR);
        setField(data, MESSAGE_STR);
        trig.readObject(data.toString());
        assertEquals(NAME_STR, trig.getName());
        assertEquals(TYPE_STR, trig.getType());
        assertTrue(trig.isEnabled());
        assertEquals(DESCRIPTION_STR, trig.getDescription());
        assertEquals(REPEAT_STR, trig.getRepeat());
        assertEquals(INTERVAL_STR, trig.getInterval());
        assertEquals(CRONEXPR_STR, trig.getCronExpr());
        assertEquals(DURATION_STR, trig.getDuration());
        assertEquals(MESSAGE_STR, trig.getMessage());
        
        data = new StringBuilder();
        data.append(TriggerExImpl.CLASSID);
        setField(data, null);
        setField(data, null);
        setField(data, false);
        setField(data, null);
        setField(data, null);
        setField(data, null);
        setField(data, null);
        setField(data, null);
        setField(data, null);
        trig.readObject(data.toString());
        assertEquals(null, trig.getName());
        assertEquals(null, trig.getType());
        assertTrue(!trig.isEnabled());
        assertEquals(null, trig.getDescription());
        assertEquals(null, trig.getRepeat());
        assertEquals(null, trig.getInterval());
        assertEquals(null, trig.getCronExpr());
        assertEquals(null, trig.getDuration());
        assertEquals(null, trig.getMessage());
    }

    public static void setTestWriteObject(TriggerEx trig) {
        trig.setName(NAME_STR);
        trig.setType(TYPE_STR);
        trig.setEnabled("true");                                        //NOI18N
        trig.setDescription(DESCRIPTION_STR);
        trig.setRepeat(REPEAT_STR);
        trig.setInterval(INTERVAL_STR);
        trig.setCronExpr(CRONEXPR_STR);
        trig.setDuration(DURATION_STR);
        trig.setMessage(MESSAGE_STR);
    }

    public void testWriteObject() {
        TriggerExImpl trig = new TriggerExImpl();
        StringBuilder expected = new StringBuilder();
        expected.append(TriggerExImpl.CLASSID);
        setField(expected, null);
        setField(expected, null);
        setField(expected, false);
        setField(expected, null);
        setField(expected, null);
        setField(expected, null);
        setField(expected, null);
        setField(expected, null);
        setField(expected, null);
        StringBuilder data = new StringBuilder();
        trig.writeObject(data);
        assertEquals(expected.toString(), data.toString());

        expected = new StringBuilder();
        expected.append(TriggerExImpl.CLASSID);
        setField(expected, NAME_STR);
        setField(expected, TYPE_STR);
        setField(expected, true);
        setField(expected, DESCRIPTION_STR);
        setField(expected, REPEAT_STR);
        setField(expected, INTERVAL_STR);
        setField(expected, CRONEXPR_STR);
        setField(expected, DURATION_STR);
        setField(expected, MESSAGE_STR);
        data = new StringBuilder();
        setTestWriteObject(trig);
        trig.writeObject(data);
        assertEquals(expected.toString(), data.toString());
    }

}
