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
 * @(#)DataTypeFactoryImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dt.impl;

import java.util.GregorianCalendar;
import java.util.TimeZone;

import com.sun.jbi.engine.bpel.core.bpel.dt.Date;
import com.sun.jbi.engine.bpel.core.bpel.dt.DateTime;
import com.sun.jbi.engine.bpel.core.bpel.dt.Duration;
import com.sun.jbi.engine.bpel.core.bpel.dt.GDay;
import com.sun.jbi.engine.bpel.core.bpel.dt.GMonth;
import com.sun.jbi.engine.bpel.core.bpel.dt.GMonthDay;
import com.sun.jbi.engine.bpel.core.bpel.dt.GYear;
import com.sun.jbi.engine.bpel.core.bpel.dt.GYearMonth;
import com.sun.jbi.engine.bpel.core.bpel.dt.Time;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 *         Preferences - Java - Code Generation - Code and Comments
 */
public class DataTypeFactoryImpl {
    /**
     * Creates a new DataTypeFactoryImpl object.
     */
    public DataTypeFactoryImpl() {
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#parseDate(java.lang.String)
     */
    public Date parseDate(String val) {
        return DateImpl.parse(val);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#newDateInstance(int, int, int, java.util.TimeZone)
     */
    public Date newDateInstance(int year, int month, int day, TimeZone tz) {
        return new DateImpl(year, month, day, tz);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#parseDateTime(java.lang.String)
     */
    public DateTime parseDateTime(String val) {
        return DateTimeImpl.parse(val);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#newDateTimeInstance(int, int, int, java.util.TimeZone)
     */
    public DateTime newDateTimeInstance(int year, int month, int day) {
        return new DateTimeImpl(year, month, day);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#newDateTimeInstance(java.util.GregorianCalendar)
     */
    public DateTime newDateTimeInstance(GregorianCalendar cal) {
        return new DateTimeImpl(cal);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#getCalendar(java.lang.String)
     */
    public GregorianCalendar getCalendar(String isodate) {
        return DateParserImpl.getCalendar(isodate);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#getIsoDate(java.util.Date)
     */
    public String getIsoDate(java.util.Date date) {
        return DateParserImpl.getIsoDate(date);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#parseDuration(java.lang.String)
     */
    public Duration parseDuration(String val) {
        return DurationImpl.parse(val);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#parseGDay(java.lang.String)
     */
    public GDay parseGDay(String val) {
        return GDayImpl.parse(val);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#newGDayInstance(int, java.util.TimeZone)
     */
    public GDay newGDayInstance(int day, TimeZone tz) {
        return new GDayImpl(day, tz);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#parseGMonthDay(java.lang.String)
     */
    public GMonthDay parseGMonthDay(String val) {
        return GMonthDayImpl.parse(val);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#newGMonthDayInstance(int, int, java.util.TimeZone)
     */
    public GMonthDay newGMonthDayInstance(int month, int day, TimeZone tz) {
        return new GMonthDayImpl(month, day, tz);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#parseGMonth(java.lang.String)
     */
    public GMonth parseGMonth(String val) {
        return GMonthImpl.parse(val);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#newGMonthInstance(int, java.util.TimeZone)
     */
    public GMonth newGMonthInstance(int month, TimeZone tz) {
        return new GMonthImpl(month, tz);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#parseGYear(java.lang.String)
     */
    public GYear parseGYear(String val) {
        return GYearImpl.parse(val);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#newGYearInstance(int, java.util.TimeZone)
     */
    public GYear newGYearInstance(int year, TimeZone tz) {
        return new GYearImpl(year, tz);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#parseGYearMonth(java.lang.String)
     */
    public GYearMonth parseGYearMonth(String val) {
        return GYearMonthImpl.parse(val);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#newGYearMonthInstance(int, int, java.util.TimeZone)
     */
    public GYearMonth newGYearMonthInstance(int year, int month, TimeZone tz) {
        return new GYearMonthImpl(year, month, tz);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#parseTime(java.lang.String)
     */
    public Time parseTime(String val) {
        return TimeImpl.parse(val);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#newTimeInstance(int, int, int, int, java.util.TimeZone)
     */
    public Time newTimeInstance(int hours, int minutes, int seconds,
        int milliseconds, TimeZone tz) {
        return new TimeImpl(hours, minutes, seconds, milliseconds, tz);
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#decodeFromBase64Binary(String)
     */
    public byte[] decodeFromBase64Binary(String val) {
        if (val != null) {
            try {
                Base64Utils.base64DecodeToByte(val);
            } catch (java.io.IOException e) {
                e.printStackTrace();
            }
        }

        return null;
    }

    /**
     * @see com.sun.jbi.engine.bpel.core.bpms.dt.DataTypeFactory#decodeFromBase64BinaryToString(String)
     */
    public String decodeFromBase64BinaryToString(String val) {
        if (val != null) {
            try {
                Base64Utils.base64Decode(val);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return null;
    }

    /**
     * encode to base64 binary
     * @param val bytes
     * @return String string
     */
    public String encodeToBase64Binary(byte[] val) {
        if (val != null) {
            try {
                Base64Utils.byteToBase64String(val);
            } catch (java.io.IOException e) {
                e.printStackTrace();
            }
        }

        return null;
    }

    /**
     * encode to base64 binary
     * @param val string
     * @return String string
     */
    public String encodeToBase64Binary(String val) {
        if (val != null) {
            try {
                Base64Utils.string2Base64(val);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return null;
    }

    /**
     * encode to hex binary
     * @param val string
     * @return String string
     */
    public String encodeToHexBinary(String val) {
        return null;
    }

    /**
     * decode from hex binary
     * @param val string
     * @return byte[] bytes
     */
    public byte[] decodeFromHexBinary(String val) {
        return null;
    }

    /**
     * decode from hex binary to string
     * @param val string
     * @return String string
     */
    public String decodeFromHexBinaryToString(String val) {
        return null;
    }

    /**
     * encode to hex binary
     * @param val bytes
     * @return String encoded string
     */
    public String encodeToHexBinary(byte[] val) {
        return null;
    }
}
