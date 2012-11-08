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
 * @(#)DateImpl.java 
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

import com.sun.jbi.engine.bpel.core.bpel.dt.Date;
import com.sun.jbi.engine.bpel.core.bpel.util.I18n;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 * @version 
 */
public class DateImpl implements Date {

    /**
     * DOCUMENT ME!
     */
    static final DecimalFormat YEARFMT = new DecimalFormat("00");

    /**
     * DOCUMENT ME!
     */
    static final DecimalFormat FMT = new DecimalFormat("00");

    /**
     * DOCUMENT ME!
     */
    static final SimpleDateFormat DATEFMT = new SimpleDateFormat("yyyy-MM-dd");

    /**
     * DOCUMENT ME!
     */
    int year;

    /**
     * DOCUMENT ME!
     */
    int month;

    /**
     * DOCUMENT ME!
     */
    int day;

    /**
     * DOCUMENT ME!
     */
    TimeZone tz;

    /**
     * Creates a new DateImpl object.
     */
    public DateImpl() {
    }

    /**
     * Creates a new DateImpl object.
     *
     * @param year DOCUMENT ME!
     * @param month DOCUMENT ME!
     * @param day DOCUMENT ME!
     * @param tz DOCUMENT ME!
     */
    public DateImpl(int year, int month, int day, TimeZone tz) {
        this.year = year;
        this.month = month;
        this.day = day;
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
        this.year = value;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getMonth() { // January == 1

        return month;
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
     * @param tzz DOCUMENT ME!
     */
    public void setTimeZone(TimeZone tzz) {
        tz = tzz;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value DOCUMENT ME!
     */
    public void setMonth(int value) {
        this.month = value;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getDay() {
        return day;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value DOCUMENT ME!
     */
    public void setDay(int value) {
        this.day = value;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        return YEARFMT.format(getYear()) + "-" + FMT.format(getMonth()) + "-" +
        FMT.format(getDay());
    }

    /**
     * DOCUMENT ME!
     *
     * @param value DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static Date parse(String value) {
        ParsePosition pos = new ParsePosition(0);
        DATEFMT.setLenient(false);

        java.util.Date date = DATEFMT.parse(value, pos);

        if (date == null) {
            if ((value == null) || (value.equals(""))) {
                return null;
            }

            throw new RuntimeException(I18n.loc("BPCOR-6002: DateImpl_Invalid_Date_found"));
        }

        TimeZone tz = null;

        if (pos.getIndex() < value.length()) {
            tz = TimeZoneParserImpl.parse(value.substring(pos.getIndex()));
        }

        Calendar cal = new GregorianCalendar(tz);
        cal.setTime(date);

        return new DateImpl(cal.get(cal.YEAR), cal.get(cal.MONTH) + 1,
            cal.get(cal.DAY_OF_MONTH), tz);
    }
}
