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
 */

/*
 * @(#)$Id: XmlUtilTest.java,v 1.1 2008/11/12 23:00:20 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.util;

import java.util.Calendar;
import java.util.TimeZone;

import junit.framework.TestCase;

public class XmlUtilTest extends TestCase {
    public void testConvertToDateTime() throws Exception {
        // NPE check
        try {
            XmlUtil.convertToDateTime(null);
            fail("NullPointerException expected");
        } catch (NullPointerException e) {
            // pass
        }
        
        // Get the current calendar value;
        // just for kicks, get it as if environment is in GMT.
        // Convert to dateTime
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT+00:00"));
        String result = XmlUtil.convertToDateTime(cal);
        StringBuffer resultBuffer = new StringBuffer(result);
        
        // Reparse dateTime back to a calendar value
        Calendar recal = Calendar.getInstance();
        recal.clear();
        {
            int yearDelimPos = resultBuffer.indexOf("-");
            String year = resultBuffer.substring(0, yearDelimPos);
        
            int monthDelimPos = resultBuffer.indexOf("-", yearDelimPos + 1);
            String month = resultBuffer.substring(yearDelimPos + 1, monthDelimPos);
        
            int monthDayDelimPos = resultBuffer.indexOf("T", monthDelimPos + 1);
            String monthDay = resultBuffer.substring(monthDelimPos + 1, monthDayDelimPos);
        
            int hourPos = resultBuffer.indexOf(":", monthDayDelimPos + 1);
            String hour = resultBuffer.substring(monthDayDelimPos + 1, hourPos);
        
            int minutePos = resultBuffer.indexOf(":", hourPos + 1);
            String minute = resultBuffer.substring(hourPos + 1, minutePos);
        
            int secondPos = resultBuffer.indexOf(".", minutePos + 1);
            String second = resultBuffer.substring(minutePos + 1, secondPos);
            String fractions = resultBuffer.substring(secondPos + 1, secondPos + 4);
            
            recal.set(Calendar.YEAR, Integer.parseInt(year));
            recal.set(Calendar.MONTH, Integer.parseInt(month) - 1);
            recal.set(Calendar.DAY_OF_MONTH, Integer.parseInt(monthDay));
            recal.set(Calendar.HOUR_OF_DAY, Integer.parseInt(hour));
            recal.set(Calendar.MINUTE, Integer.parseInt(minute));
            recal.set(Calendar.SECOND, Integer.parseInt(second));
            recal.set(Calendar.MILLISECOND, Integer.parseInt(fractions));
            // I don't care about parsing and setting timezone offset,
            // since computations will factor in local timezone.
            recal.getTimeInMillis();
        }

        // Original sampled time (GMT) should be equal to the reparsed one (local)
        assertTrue("input=" + cal.getTime().toString() + "\noutput="
                + recal.getTime().toString(), recal.compareTo(cal) == 0);
    }

    public void testConvertToCalendar() throws Exception {
        try {
            XmlUtil.convertToDateTime(null);
            fail("NullPointerException expected");
        } catch (NullPointerException e) {
            //pass
        }
        
        // Sample the current date-time.
        // For kicks, use GMT.
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT+00:00"));
        
        // Then serialize it.
        String dateTime = XmlUtil.convertToDateTime(cal);
        
        // Then deserialize it.
        Calendar recal = XmlUtil.convertToCalendar(dateTime);
        
        // Original sample should be equal to the result.
        assertTrue("input=" + cal.getTime().toString() + "\noutput="
                + recal.getTime().toString(), recal.compareTo(cal) == 0);
    }
}
