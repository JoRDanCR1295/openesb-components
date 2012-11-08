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
 * @(#)GYearImpl.java 
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

import com.sun.jbi.engine.bpel.core.bpel.dt.GYear;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 * @version 
 */
public class GYearImpl implements GYear {
    /**
     * DOCUMENT ME!
     */
    static final DecimalFormat FMT = new DecimalFormat("0000");

    /**
     * DOCUMENT ME!
     */
    static final SimpleDateFormat DATEFORMAT = new SimpleDateFormat("yyyy");

    /**
     * DOCUMENT ME!
     */
    int year;

    /**
     * DOCUMENT ME!
     */
    TimeZone tz;

    /**
     * Creates a new GYearImpl object.
     */
    public GYearImpl() {
    }

    /**
     * Creates a new GYearImpl object.
     *
     * @param date DOCUMENT ME!
     * @param tz DOCUMENT ME!
     */
    public GYearImpl(int date, TimeZone tz) {
        year = date;
        this.tz = tz;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getYear() {
        return year;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value DOCUMENT ME!
     */
    public void setYear(int value) {
        year = value;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public TimeZone getTimeZone() {
        return tz;
    }

    /**
     * DOCUMENT ME!
     *
     * @param tz DOCUMENT ME!
     */
    public void setTimeZone(TimeZone tz) {
        this.tz = tz;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        String result = FMT.format(year);

        if (tz != null) {
            result += TimeZoneParserImpl.format(tz);
        }

        return result;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static GYear parse(String value) {
        ParsePosition pos = new ParsePosition(0);
        java.util.Date date = DATEFORMAT.parse(value, pos);
        TimeZone tz = null;

        if (pos.getIndex() < value.length()) {
            tz = TimeZoneParserImpl.parse(value.substring(pos.getIndex()));
        }

        Calendar cal = new GregorianCalendar(tz);
        cal.setTime(date);

        return new GYearImpl(cal.get(cal.YEAR), tz);
    }
}
