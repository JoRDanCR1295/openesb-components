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
 * @(#)DateTime.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dt;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.TimeZone;


/**
 * DateTime interface
 *
 * @author Sun Microsystems
 *         Preferences - Java - Code Generation - Code and Comments
 */
public interface DateTime {
    /**
     * gets calendar
     *
     * @return Calendar calendar
     */
    public Calendar getCal();

    /**
     * check if datetime is after given datetime
     *
     * @param other datetime
     *
     * @return boolean: if datetime is after given datetime, returns true; otherwise, returns false
     */
    public boolean after(DateTime other);

    /**
     * check if datetime is before given datatime
     *
     * @param other datetime
     *
     * @return boolean: if datetime is before given datetime, returns true; otherwise, returns
     *         false
     */
    public boolean before(DateTime other);

    /**
     * sets calendar
     *
     * @param cal gregorian calendar
     */
    public void setCalendar(GregorianCalendar cal);

    /**
     * roll
     *
     * @param dur duration
     */
    public void roll(Duration dur);

    /**
     * add
     *
     * @param dur duration
     */
    public void add(Duration dur);

    /**
     * substract
     *
     * @param dur duration
     */
    public void subtract(Duration dur);

    /**
     * convert to gregorian calendar
     *
     * @return GreogorianCalendar gregorian calendar
     */
    public GregorianCalendar toCalendar();

    /**
     * gets year
     *
     * @return int year
     */
    public int getYear();

    /**
     * gets month
     *
     * @return int month
     */
    public int getMonth();

    /**
     * gets day
     *
     * @return int day
     */
    public int getDay();

    /**
     * gets hours
     *
     * @return int hours
     */
    public int getHours();

    /**
     * gets minutes
     *
     * @return int minutes
     */
    public int getMinutes();

    /**
     * gets seconds
     *
     * @return int seconds
     */
    public int getSeconds();

    /**
     * gets timezone
     *
     * @return TimeZone timezone
     */
    public TimeZone getTimeZone();

    /**
     * gets milliseconds
     *
     * @return int milliseconds
     */
    public int getMilliseconds();

    /**
     * sets year
     *
     * @param year year
     */
    public void setYear(int year);

    /**
     * sets month
     *
     * @param month month
     */
    public void setMonth(int month);

    /**
     * sets day
     *
     * @param day day
     */
    public void setDay(int day);

    /**
     * sets hours
     *
     * @param hours hours
     */
    public void setHours(int hours);

    /**
     * sets minutes
     *
     * @param minutes minutes
     */
    public void setMinutes(int minutes);

    /**
     * sets seconds
     *
     * @param seconds seconds
     */
    public void setSeconds(int seconds);

    /**
     * sets timezone
     *
     * @param tz timezone
     */
    public void setTimeZone(TimeZone tz);

    /**
     * sts milliseconds
     *
     * @param millis milliseconds
     */
    public void setMilliseconds(int millis);

    /**
     * roll year
     *
     * @param years number of years
     */
    public void rollYear(int years);

    /**
     * roll month
     *
     * @param months number of months
     */
    public void rollMonth(int months);

    /**
     * roll days
     *
     * @param days number of days
     */
    public void rollDay(int days);

    /**
     * roll hours
     *
     * @param hours number of hours
     */
    public void rollHours(int hours);

    /**
     * roll minutes
     *
     * @param minutes number of minutes
     */
    public void rollMinutes(int minutes);

    /**
     * roll seconds
     *
     * @param seconds number of seconds
     */
    public void rollSeconds(int seconds);

    /**
     * roll milliseconds
     *
     * @param millis number of milliseconds
     */
    public void rollMilliseconds(int millis);

    /**
     * adds year
     *
     * @param years number of years
     */
    public void addYear(int years);

    /**
     * adds months
     *
     * @param months number of months
     */
    public void addMonth(int months);

    /**
     * add days
     *
     * @param days number of days
     */
    public void addDay(int days);

    /**
     * adds minutes
     *
     * @param minutes number of minutes
     */
    public void addMinutes(int minutes);

    /**
     * adds seconds
     *
     * @param seconds number of seconds
     */
    public void addSeconds(int seconds);

    /**
     * adds milliseconds
     *
     * @param millis number of milliseconds
     */
    public void addMilliseconds(int millis);

    /**
     * toString
     *
     * @return String String
     */
    public String toString();

    /**
     * adds hours
     *
     * @param hours number of hours
     */
    public void addHours(int hours);
}
