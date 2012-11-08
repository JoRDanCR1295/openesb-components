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
 * @(#)RFC977Format.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.util.dateformat;

import java.text.ParseException;

import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;


public class RFC977Format implements MailDateFormat {
    /**
    * Internal date formatter for long date formats
    */
    private final BaseDateFormat longFormat;

    /**
     * Internal date formatter for short date formats
     */
    private final BaseDateFormat shortFormat;

    /**
     * Constructor for RFC977DateFormat
     */
    public RFC977Format() {
        longFormat = new BaseDateFormat("yyyyMMdd HHmmss", Locale.ENGLISH);
        shortFormat = new BaseDateFormat("yyMMdd HHmmss", Locale.ENGLISH);
    }

    /**
     * This method returns the long form of the RFC977 Date
     *
     * @return java.lang.String
     * @param d Date
     */
    public String format(Date d) {
        return longFormat.format(d);
    }

    /**
     * Parses text from the beginning of the given string to produce a date.
     * The method may not use the entire text of the given string.
     * <p>
     * This method is designed to be thread safe, so we wrap our delegated
     * parse method in an appropriate synchronized block.
     *
     * @param source A <code>String</code> whose beginning should be parsed.
     * @return A <code>Date</code> parsed from the string.
     * @throws ParseException if the beginning of the specified string
     *         cannot be parsed.
     */
    public Date parse(String source) throws ParseException {
        source = source.trim();

        if (source.indexOf(' ') == 6) {
            return shortFormat.parse(source);
        } else {
            return longFormat.parse(source);
        }
    }

    /**
     * Sets the time zone of this SynchronizedDateFormat object.
     * @param zone the given new time zone.
     */
    public void setTimeZone(TimeZone zone) {
        synchronized (this) {
            shortFormat.setTimeZone(zone);
            longFormat.setTimeZone(zone);
        }
    }

    /**
     * Gets the time zone.
     * @return the time zone associated with this SynchronizedDateFormat.
     */
    public TimeZone getTimeZone() {
        synchronized (this) {
            return shortFormat.getTimeZone();
        }
    }

    /**
     * Specify whether or not date/time parsing is to be lenient.  With
     * lenient parsing, the parser may use heuristics to interpret inputs that
     * do not precisely match this object's format.  With strict parsing,
     * inputs must match this object's format.
     * @param lenient when true, parsing is lenient
     * @see java.util.Calendar#setLenient
     */
    public void setLenient(boolean lenient) {
        synchronized (this) {
            shortFormat.setLenient(lenient);
            longFormat.setLenient(lenient);
        }
    }

    /**
     * Tell whether date/time parsing is to be lenient.
     * @return whether this SynchronizedDateFormat is lenient.
     */
    public boolean isLenient() {
        synchronized (this) {
            return shortFormat.isLenient();
        }
    }

    /**
     * Overrides equals
     */
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (!(obj instanceof RFC977Format)) {
            return false;
        }

        RFC977Format other = (RFC977Format) obj;

        synchronized (this) {
            return ((shortFormat.equals(other.shortFormat)) &&
            (longFormat.equals(other.longFormat)));
        }
    }

    /**
     * Overrides hashCode
     */
    public int hashCode() {
        return (int) (longFormat.hashCode() & shortFormat.hashCode());
    }
}
