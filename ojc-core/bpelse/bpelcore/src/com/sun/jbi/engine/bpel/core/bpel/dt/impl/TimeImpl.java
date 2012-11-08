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
 * @(#)TimeImpl.java 
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

import com.sun.jbi.engine.bpel.core.bpel.dt.Time;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 * @version 
 */
public class TimeImpl implements Time {
    /**
     * DOCUMENT ME!
     */
    static final DecimalFormat FMT = new DecimalFormat("00");

    /**
     * DOCUMENT ME!
     */
    static final DecimalFormat MILLIFMT = new DecimalFormat(".000");

    /**
     * DOCUMENT ME!
     */
    static final SimpleDateFormat DATEFORMAT = new SimpleDateFormat("hh:mm:ss");

    /**
     * DOCUMENT ME!
     */
    static final DecimalFormat DECIMALFMT = new DecimalFormat(".#");

    /**
     * DOCUMENT ME!
     */
    int hours;

    /**
     * DOCUMENT ME!
     */
    int minutes;

    /**
     * DOCUMENT ME!
     */
    int seconds;

    /**
     * DOCUMENT ME!
     */
    int milliseconds;

    /**
     * DOCUMENT ME!
     */
    TimeZone tz;

    /**
     * Creates a new TimeImpl object.
     */
    public TimeImpl() {
    }

    /**
     * Creates a new TimeImpl object.
     *
     * @param hours DOCUMENT ME!
     * @param minutes DOCUMENT ME!
     * @param seconds DOCUMENT ME!
     * @param milliseconds DOCUMENT ME!
     * @param tz DOCUMENT ME!
     */
    public TimeImpl(int hours, int minutes, int seconds, int milliseconds,
        TimeZone tz) {
        this.hours = hours;
        this.minutes = minutes;
        this.seconds = seconds;
        this.milliseconds = milliseconds;
        this.tz = tz;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getHours() {
        return hours;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getMinutes() {
        return minutes;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getSeconds() {
        return seconds;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getMilliseconds() {
        return milliseconds;
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
     * @param hours DOCUMENT ME!
     */
    public void setHours(int hours) {
        this.hours = hours;
    }

    /**
     * DOCUMENT ME!
     *
     * @param minutes DOCUMENT ME!
     */
    public void setMinutes(int minutes) {
        this.minutes = minutes;
    }

    /**
     * DOCUMENT ME!
     *
     * @param seconds DOCUMENT ME!
     */
    public void setSeconds(int seconds) {
        this.seconds = seconds;
    }

    /**
     * DOCUMENT ME!
     *
     * @param millis DOCUMENT ME!
     */
    public void setMilliseconds(int millis) {
        this.milliseconds = millis;
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
        String result = FMT.format(getHours());
        result += ":";
        result += FMT.format(getMinutes());
        result += ":";
        result += FMT.format(getSeconds());

        if (milliseconds > 0) {
            result += ".";
            result += MILLIFMT.format(milliseconds);
        }

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
    public static Time parse(String value) {
        ParsePosition pos = new ParsePosition(0);
        java.util.Date date = DATEFORMAT.parse(value, pos);
        TimeZone tz = null;
        int milliseconds = 0;

        if (pos.getIndex() < value.length()) {
            if (value.charAt(pos.getIndex()) == '.') {
                double fractionalSeconds = (DECIMALFMT.parse(value, pos)).doubleValue();
                milliseconds = (int) (fractionalSeconds * 1000);
            }

            if (pos.getIndex() < value.length()) {
                tz = TimeZoneParserImpl.parse(value.substring(pos.getIndex()));
            }
        }

        Calendar cal = new GregorianCalendar(tz);
        cal.setTime(date);

        return new TimeImpl(cal.get(cal.HOUR), cal.get(cal.MINUTE),
            cal.get(cal.SECOND), milliseconds, tz);
    }
}
