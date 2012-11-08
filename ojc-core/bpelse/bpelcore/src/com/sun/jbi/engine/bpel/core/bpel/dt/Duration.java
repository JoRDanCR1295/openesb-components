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
 * @(#)Duration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dt;


/**
 * Duration interface
 *
 * @author Sun Microsystems
 *         Preferences - Java - Code Generation - Code and Comments
 */
public interface Duration {
    /**
     * sets years
     *
     * @param years number of years
     */
    public void setYears(int years);

    /**
     * sets months
     *
     * @param months number of months
     */
    public void setMonths(int months);

    /**
     * sets days
     *
     * @param days number of days
     */
    public void setDays(int days);

    /**
     * sets hours
     *
     * @param hours number of hours
     */
    public void setHours(int hours);

    /**
     * sets minutes
     *
     * @param minutes number of minutes
     */
    public void setMinutes(int minutes);

    /**
     * sets seconds
     *
     * @param seconds number of seconds
     */
    public void setSeconds(int seconds);

    /**
     * sets milliseconds
     *
     * @param millis number of milliseconds
     */
    public void setMilliseconds(int millis);

    /**
     * get years
     *
     * @return int number of years
     */
    public int getYears();

    /**
     * gets months
     *
     * @return int number of months
     */
    public int getMonths();

    /**
     * get days
     *
     * @return int number of days
     */
    public int getDays();

    /**
     * get hours
     *
     * @return int number of hours
     */
    public int getHours();

    /**
     * get minutes
     *
     * @return int number of minutes
     */
    public int getMinutes();

    /**
     * get seconds
     *
     * @return int number of seconds
     */
    public int getSeconds();

    /**
     * get milliseconds
     *
     * @return int number of milliseconds
     */
    public int getMilliseconds();

    /**
     * check if it's negative
     *
     * @return boolean: if it's negative, returns true; otherwise, returns false
     */
    public boolean isNegative();

    /**
     * sets negavtive flag
     *
     * @param value negative flag
     */
    public void setNegative(boolean value);
}
