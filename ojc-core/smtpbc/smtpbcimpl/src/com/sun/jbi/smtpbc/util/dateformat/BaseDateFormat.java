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
 * @(#)BaseDateFormat.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc.util.dateformat;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;


public class BaseDateFormat implements MailDateFormat {
    private final DateFormat dateFormat;
    final Lock lock = new ReentrantLock();

    /**
     * Public constructor that mimics that of SimpleDateFormat.  See
     * java.text.SimpleDateFormat for more details.
     *
     * @param pattern the pattern that defines this DateFormat
     * @param locale the locale
     */
    public BaseDateFormat(String pattern, Locale locale) {
        dateFormat = new SimpleDateFormat(pattern, locale);
    }

    /**
     * <p>Wrapper method to allow child classes to synchronize a preexisting
     * DateFormat.</p>
     *
     * <p>TODO: Investigate replacing this with a factory method.</p>
     *
     * @param the DateFormat to synchronize
     */
    protected BaseDateFormat(DateFormat theDateFormat) {
        dateFormat = theDateFormat;
    }

    /* (non-Javadoc)
         * @see com.sun.jbi.smtpbc.util.dateformat.MailDateFormat#format(java.util.Date)
         */
    public String format(Date d) {
        lock.lock();

        try {
            return dateFormat.format(d);
        } finally {
            lock.unlock();
        }
    }

    /* (non-Javadoc)
         * @see com.sun.jbi.smtpbc.util.dateformat.MailDateFormat#parse(java.lang.String)
         */
    public Date parse(String source) throws ParseException {
        lock.lock();

        try {
            return dateFormat.parse(source);
        } finally {
            lock.unlock();
        }
    }

    /* (non-Javadoc)
         * @see com.sun.jbi.smtpbc.util.dateformat.MailDateFormat#setTimeZone(java.util.TimeZone)
         */
    public void setTimeZone(TimeZone zone) {
        lock.lock();

        try {
            dateFormat.setTimeZone(zone);
        } finally {
            lock.unlock();
        }
    }

    /* (non-Javadoc)
         * @see com.sun.jbi.smtpbc.util.dateformat.MailDateFormat#getTimeZone()
         */
    public TimeZone getTimeZone() {
        lock.lock();

        try {
            return dateFormat.getTimeZone();
        } finally {
            lock.unlock();
        }
    }

    /* (non-Javadoc)
         * @see com.sun.jbi.smtpbc.util.dateformat.MailDateFormat#setLenient(boolean)
         */
    public void setLenient(boolean lenient) {
        lock.lock();

        try {
            dateFormat.setLenient(lenient);
        } finally {
            lock.unlock();
        }
    }

    /* (non-Javadoc)
         * @see com.sun.jbi.smtpbc.util.dateformat.MailDateFormat#isLenient()
         */
    public boolean isLenient() {
        lock.lock();

        try {
            return dateFormat.isLenient();
        } finally {
            lock.unlock();
        }
    }

    /**
     * Overrides hashCode
     */
    public int hashCode() {
        lock.lock();

        try {
            return dateFormat.hashCode();
        } finally {
            lock.unlock();
        }
    }

    /**
     * Overrides equals
     */
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if ((obj == null) || (getClass() != obj.getClass())) {
            return false;
        }

        lock.lock();

        try {
            return dateFormat.equals(obj);
        } finally {
            lock.unlock();
        }
    }
}
