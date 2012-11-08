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
 * @(#)DateUtils.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.eways.util;

import java.util.Date;
import java.util.TimeZone;

import java.text.SimpleDateFormat;
import java.text.ParseException;

import com.sun.stc.jcsre.FormatterFactory;

/**
 * class of handy date manipulation methods
 */
public class DateUtils {

    /** number of millis in a second */
    public static final long SECOND = 1000;

    /** number of millis in a minute */
    public static final long MINUTE = 60*SECOND;

    /** number of millis in an hour */
    public static final long HOUR = 60*MINUTE;

    /** number of millis in a day */
    public static final long DAY = 24*HOUR;

    /** number of millis in a week */
    public static final long WEEK = 7*DAY;

    /** default timestamp format */
    public static final String TIMESTAMP_FORMAT = "hh:mm:ss.SSS";

    /** default datestamp format */
    public static final String DATESTAMP_FORMAT = "yyyy-MM-dd";

    /** default datetimestamp format */
    public static final String DATETIMESTAMP_FORMAT = DATESTAMP_FORMAT
                                                    + " "
                                                    + TIMESTAMP_FORMAT;

    /** formatter for RFC2822 datetimes */
    private static SimpleDateFormat RFC2822_FORMAT = new SimpleDateFormat("E, dd MMM yyyy hh:mm:ss ");

    /**
     * private constructor to prevent instantiation
     */
    private DateUtils (){}

    /**
     * generate a timestamp in the default timestamp format
     *
     * @return a string containing the time in the default format
     *
     * @see #TIMESTAMP_FORMAT
     */
    public static String timeStamp () {
        return timeStamp(DateUtils.TIMESTAMP_FORMAT);
    }

    /**
     * generate a datestamp in the default datestamp format
     *
     * @return a string containing the date in the default format
     *
     * @see #DATESTAMP_FORMAT
     */
    public static String dateStamp () {
        return timeStamp(DateUtils.DATESTAMP_FORMAT);
    }

    /**
     * generate a datetimestamp in the default datetimestamp format
     *
     * @return a string containing the date and time in the default format
     *
     * @see #DATETIMESTAMP_FORMAT
     */
    public static String dateTimeStamp () {
        return timeStamp(DateUtils.DATETIMESTAMP_FORMAT);
    }

    /**
     * generate a timestamp in the specified format.
     *
     * if _pattern is null, the default format is used.
     *
     * @param _format format
     */
    public static String timeStamp (String _pattern) {
        return DateUtils.format(new Date(), _pattern);
    }

    /**
     * generated a formatted string representing the specified date
     *
     * if pattern is null the default format is used.
     *
     * @param _date the date
     * @param _pattern the pattern
     * 
     * @return string containing the formatted date
     */
    public static String format (Date _date, String _pattern) {
        return FormatterFactory.getDateFormat(_pattern).format(_date);
    }

    /**
     * generated a formatted string representing the specified date
     *
     * if pattern is null the default format is used.
     *
     * @param _pattern the pattern
     * @param _date the date
     * 
     * @return string containing the formatted date
     *
     * @see #format(Date,String)
     */
    public static String format (String _pattern, Date _date) {
        return format(_date, _pattern);
    }

    /**
     * parse a date given the specified pattern
     *
     * @param _date the date
     * @param _pattern the pattern
     *
     * @return the parsed date
     *
     * @throws ParseException unable to parse date using pattern
     */
    public static Date parse (String _date, String _pattern) throws ParseException {
        return FormatterFactory.getDateFormat(_pattern).parse(_date);
    }

    /**
     * transform a date from one representation to another
     *
     * @param _date date to transform
     * @param _from source pattern 
     * @param _to destination pattern
     *
     * @return _date formatted in _toPattern
     *
     * @throws ParseException if _date is not in format implied by _from
     */
    public static String transformDate (String _date, String _from, String _to) throws ParseException {
        return FormatterFactory.getDateFormat(_to).format(FormatterFactory.getDateFormat(_from).parse(_date));
    }

    /**
     * convert the specified date and time into RFC2822 format
     *
     * @param _date the date in the default format (returned from 
     * <code>DateUtils.dateStamp()</code>)
     * @param _time the time in the default format (returned from
     * <code>DateUtils.timeStamp()</code>)
     *
     * @returns the date and time in RFC2822 format
     *
     * @throws ParseException _date or _time is invalid
     *
     * @see #dateStamp
     * @see #timeStamp
     */
    public static String toRFC2822 (String _date, String _time) throws ParseException {
        return toRFC2822(_date+" "+_time);
    }

    /**
     * convert the specified datetime into RFC2822 format
     *
     * @param _dateTime the datetime in the default format (returned from 
     * <code>DateUtils.dateTimeStamp()</code>)
     *
     * @returns the date and time in RFC2822 format
     *
     * @throws ParseException _dateTime is invalid.
     *
     * @see #dateTimeStamp
     */
    public static String toRFC2822 (String _dateTime) throws ParseException {
        return toRFC2822(parse(_dateTime, DATETIMESTAMP_FORMAT));
    }

    /**
     * convert the specified date and time to RFC2822 format
     *
     * @param _date the date and time
     *
     * @return the date and time in RFC2822 format
     */
    public static String toRFC2822 (Date _date) {
        
        long milliSeconds = TimeZone.getDefault().getRawOffset();
        // -(5*60*60*1000);
        long minutes = (milliSeconds / DateUtils.MINUTE);
        long hours = minutes / 60;
        minutes %= 60;
        
        String strHour = FormatterFactory.getNumberFormat("00").format(hours);
        String strMinutes = FormatterFactory.getNumberFormat("00").format(minutes);
        
        if(milliSeconds > -1)
            strHour = "+" + strHour;
        
        String result = null;
        synchronized(DateUtils.RFC2822_FORMAT) {
            result = DateUtils.RFC2822_FORMAT.format(_date);
        }
        
        return result + strHour + strMinutes;
    }

}
