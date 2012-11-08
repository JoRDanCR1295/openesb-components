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

package com.sun.jbi.engine.bpel.core.bpel.util;

import java.text.DecimalFormat;


/**
 * Implementation of xsd:dateTime and xsd:duration
 *
 * @author Sun Microsystems
 * @version 
 */
public class Duration {
    
    /** DOCUMENT ME! */
    static final DecimalFormat FMT = 
            new DecimalFormat(".###"); // NO I18N

    /** DOCUMENT ME! */
    boolean negative;

    /** DOCUMENT ME! */
    int years;

    /** DOCUMENT ME! */
    int months;

    /** DOCUMENT ME! */
    int days;

    /** DOCUMENT ME! */
    int hours;

    /** DOCUMENT ME! */
    int minutes;

    /** DOCUMENT ME! */
    int seconds;

    /** DOCUMENT ME! */
    int milliseconds;

    //TimeZone tz; fix me !!

    /**
     * DOCUMENT ME!
     */
    public Duration() {
    }

    /**
     * DOCUMENT ME!
     *
     * @param copy DOCUMENT ME!
     */
    public Duration(Duration copy) {
        negative = copy.negative;
        years = copy.years;
        months = copy.months;
        days = copy.days;
        hours = copy.hours;
        minutes = copy.minutes;
        seconds = copy.seconds;
        milliseconds = copy.milliseconds;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value DOCUMENT ME!
     *
     * @throws IllegalArgumentException DOCUMENT ME!
     */
    private void check(int value) {
        if (value < 0) {
            throw new IllegalArgumentException(I18n.loc("BPCOR-3058: value cannot be negative"));
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param years DOCUMENT ME!
     */
    public void setYears(int years) {
        check(years);
        this.years = years;
    }

    /**
     * DOCUMENT ME!
     *
     * @param months DOCUMENT ME!
     */
    public void setMonths(int months) {
        check(months);
        this.months = months;
    }

    /**
     * DOCUMENT ME!
     *
     * @param days DOCUMENT ME!
     */
    public void setDays(int days) {
        check(days);
        this.days = days;
    }

    /**
     * DOCUMENT ME!
     *
     * @param hours DOCUMENT ME!
     */
    public void setHours(int hours) {
        check(hours);
        this.hours = hours;
    }

    /**
     * DOCUMENT ME!
     *
     * @param minutes DOCUMENT ME!
     */
    public void setMinutes(int minutes) {
        check(minutes);
        this.minutes = minutes;
    }

    /**
     * DOCUMENT ME!
     *
     * @param seconds DOCUMENT ME!
     */
    public void setSeconds(int seconds) {
        check(seconds);
        this.seconds = seconds;
    }

    /**
     * DOCUMENT ME!
     *
     * @param millis DOCUMENT ME!
     */
    public void setMilliseconds(int millis) {
        check(millis);
        this.milliseconds = millis;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getYears() {
        return years;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getMonths() {
        return months;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public int getDays() {
        return days;
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
    public boolean isNegative() {
        return negative;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value DOCUMENT ME!
     */
    public void setNegative(boolean value) {
        negative = value;
    }

    /**
     * DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public String toString() {
        String buffer = ""; // NO I18N

        if (negative) {
            buffer += "-"; // NO I18N
        }

        buffer += "P"; // NO I18N

        if (years > 0) {
            buffer += years;
            buffer += "Y"; // NO I18N
        }

        //if (months > 0) {
        buffer += months;
        buffer += "M"; // NO I18N

        //}
        if (days > 0) {
            buffer += days;
            buffer += "D"; // NO I18N
        }

        if ((hours > 0) || (minutes > 0) || (seconds > 0) ||
                (milliseconds > 0)) {
            buffer += "T"; // NO I18N

            if (hours > 0) {
                buffer += hours;
                buffer += "H"; // NO I18N
            }

            if (minutes > 0) {
                buffer += minutes;
                buffer += "M"; // NO I18N
            }

            if ((seconds > 0) || (milliseconds > 0)) {
                buffer += seconds;

                if (milliseconds > 0) {
                    synchronized (FMT) {
                        buffer += FMT.format((double) milliseconds / 1000);
                    }
                }

                buffer += "S"; // NO I18N
            }
        }

        return buffer;
    }

    /**
     * DOCUMENT ME!
     *
     * @param value DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static Duration parse(String value) {
        DurationParser parser = new DurationParser(value);

        return parser.parseDuration();
    }

    //P(nY)? (nM)? (nD)?T(nH)? (nM)? (nS("."n)?)?
    static class DurationParser {
        
        /** DOCUMENT ME! */
        Duration result;

        /** DOCUMENT ME! */
        char[] buf;

        /** DOCUMENT ME! */
        int pos;

        /**
         * Creates a new DurationParser object.
         *
         * @param value DOCUMENT ME!
         */
        DurationParser(String value) {
            buf = value.toCharArray();
            pos = buf.length - 1;
            result = new Duration();
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         *
         * @throws RuntimeException DOCUMENT ME!
         */
        int peek() {
            if (pos < 0) {
                throw new RuntimeException(I18n.loc("BPCOR-3059: malformed duration"));
            }

            return buf[pos];
        }

        /**
         * DOCUMENT ME!
         */
        void lex() {
            pos--;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        int parseInt() {
            int mark = pos;

            while ((pos > 0) && Character.isDigit(buf[pos - 1])) {
                pos--;
            }

            int res = Integer.parseInt(new String(buf, pos, (mark + 1) - pos));
            pos--;

            return res;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        boolean parseSeconds() {
            if (peek() == 'S') { // NO I18N
                lex();

                int value = parseInt();

                if (peek() == '.') { // NO I18N
                    result.setMilliseconds((int) (Double.parseDouble("0." + // NO I18N
                            value) * 1000));
                    lex();
                    value = parseInt();
                }

                result.setSeconds(value);

                return true;
            }

            return false;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        boolean parseMinutes() {
            if (peek() == 'M') {
                lex();
                result.setMinutes(parseInt());

                return true;
            }

            return false;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        boolean parseHours() {
            if (peek() == 'H') {
                lex();
                result.setHours(parseInt());

                return true;
            }

            return false;
        }

        /**
         * DOCUMENT ME!
         *
         * @throws RuntimeException DOCUMENT ME!
         */
        void parseT() {
            if (peek() == 'T') {
                lex();
            } else {
                throw new RuntimeException(I18n.loc("BPCOR-3060: Expected 'T' instead of '{0}'", new Integer(peek())));
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @throws RuntimeException DOCUMENT ME!
         */
        void parseP() {
            if (peek() == 'P') {
                lex();
            } else {
                throw new RuntimeException(I18n.loc("BPCOR-3061: Expected 'P' instead of '{0}'", new Integer(peek()))); 
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        boolean parseDay() {
            if (peek() == 'D') {
                lex();
                result.setDays(parseInt());

                return true;
            }

            return false;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        boolean parseMonth() {
            if (peek() == 'M') {
                lex();
                result.setMonths(parseInt());

                return true;
            }

            return false;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        boolean parseYear() {
            if (peek() == 'Y') {
                lex();
                result.setYears(parseInt());

                return true;
            }

            return false;
        }

        /**
         * DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        Duration parseDuration() {
            boolean some = false;

            if (parseSeconds()) {
                some = true;
            }

            if (parseMinutes()) {
                some = true;
            }

            if (parseHours()) {
                some = true;
            }

            if (some) {
                parseT();
            }

            if (parseDay()) {
                some = true;
            }

            if (parseMonth()) {
                some = true;
            }

            if (parseYear()) {
                some = true;
            }

            parseP();
            parseEnd(some);

            return result;
        }

        /**
         * DOCUMENT ME!
         *
         * @param some DOCUMENT ME!
         *
         * @throws RuntimeException DOCUMENT ME!
         */
        void parseEnd(boolean some) {
            if (!some) {
                throw new RuntimeException(I18n.loc("BPCOR-3062: No fields specified"));
            }

            if (pos > 0) {
                throw new RuntimeException(I18n.loc("BPCOR-3063: unexpected text: {0}", new String(buf, 0, pos)));
            }
        }
    }
}
