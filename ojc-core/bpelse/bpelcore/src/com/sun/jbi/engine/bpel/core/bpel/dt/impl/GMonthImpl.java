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
 * @(#)GMonthImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dt.impl;

import java.text.DecimalFormat;
import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;

import com.sun.jbi.engine.bpel.core.bpel.dt.GMonth;


/**
 * GMonth implementation
 *
 * @author Sun Microsystems
 * @version 
 */
public class GMonthImpl implements GMonth {
    /**
     * decimal format
     */
    static final DecimalFormat FMT = new DecimalFormat("00");

    /**
     * simple date format
     */
    static final SimpleDateFormat DATEFORMAT = new SimpleDateFormat("MM");

    /**
     * month
     */
    int month;

    /**
     * timezone
     */
    TimeZone tz;

    /**
     * Creates a new GMonthImpl object.
     */
    public GMonthImpl() {
    }

    /**
     * Creates a new GMonthImpl object.
     *
     * @param date date
     * @param tz timezone
     */
    public GMonthImpl(int date, TimeZone tz) {
        month = date;
        this.tz = tz;
    }

    /**
     * get month
     *
     * @return int month
     */
    public int getMonth() {
        return month;
    }

    /**
     * set month
     *
     * @param value month
     */
    public void setMonth(int value) { // 1 == January
        month = value;
    }

    /**
     * get timezone
     *
     * @return TimeZone timezone
     */
    public TimeZone getTimeZone() {
        return tz;
    }

    /**
     * set timezone
     *
     * @param tz timezone
     */
    public void setTimeZone(TimeZone tz) {
        this.tz = tz;
    }

    /**
     * toString
     *
     * @return String String
     */
    public String toString() {
        String result = FMT.format(month);

        if (tz != null) {
            result += TimeZoneParserImpl.format(tz);
        }

        return result;
    }

    /**
     * parse
     *
     * @param value month
     *
     * @return GMonth month
     */
    public static GMonth parse(String value) {
        ParsePosition pos = new ParsePosition(0);
        java.util.Date date = DATEFORMAT.parse(value, pos);
        TimeZone tz = null;

        if (pos.getIndex() < value.length()) {
            tz = TimeZoneParserImpl.parse(value.substring(pos.getIndex()));
        }

        Calendar cal = new GregorianCalendar(tz);
        cal.setTime(date);

        return new GMonthImpl(cal.get(cal.MONTH) + 1, tz);
    }
}
