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
 * @(#)DateParser.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.util;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.TimeZone;

    /**
     * DOCUMENT ME!
     *
     * @author Sun Microsystems
     * @version 
     * @deprecated This class was written as a util for the BPEL editor and would 
     * removed from the eInsight_Integration module
     */

    public class DateParser {
        /**
         * DOCUMENT ME!
         *
         * @param st DOCUMENT ME!
         * @param token DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         *
         * @throws RuntimeException DOCUMENT ME!
         */
        private static boolean check(StringTokenizer st, String token)
            throws RuntimeException {
            try {
                if (st.nextToken().equals(token)) {
                    return true;
                } else {
                    throw new RuntimeException("Missing [" + token + "]");
                }
            } catch (NoSuchElementException ex) {
                return false;
            }
        }

        /**
         * DOCUMENT ME!
         *
         * @param isodate DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         *
         * @throws RuntimeException DOCUMENT ME!
         */
        public static GregorianCalendar getCalendar(String isodate)
            throws RuntimeException {
            // YYYY-MM-DDThh:mm:ss.sTZD
            StringTokenizer st = new StringTokenizer(isodate, "-T:.+Z", true);

            GregorianCalendar calendar = new GregorianCalendar(TimeZone
                                                               .getTimeZone("UTC"));
            calendar.clear();

            try {
                // Year
                if (st.hasMoreTokens()) {
                    int year = Integer.parseInt(st.nextToken());
                    System.out.println("@@@### DateParser year: " + year);
                    calendar.set(Calendar.YEAR, year);
                } else {
                    return calendar;
                }

                // Month
                if (check(st, "-") && (st.hasMoreTokens())) {
                    int month = Integer.parseInt(st.nextToken()) - 1;
                    System.out.println("@@@### DateParser month: " + month);
                    calendar.set(Calendar.MONTH, month);
                } else {
                    System.out.println("@@@### DateParser NO MONTH ");
                    return calendar;
                }

                // Day
                if (check(st, "-") && (st.hasMoreTokens())) {
                    int day = Integer.parseInt(st.nextToken());
                    System.out.println("@@@### DateParser day: " + day);
                    calendar.set(Calendar.DAY_OF_MONTH, day);
                } else {
                    System.out.println("@@@### NO DAY: ");
                    return calendar;
                }

                // Hour
                if (check(st, "T") && (st.hasMoreTokens())) {
                    int hour = Integer.parseInt(st.nextToken());
                    calendar.set(Calendar.HOUR_OF_DAY, hour);
                } else {
                    calendar.set(Calendar.HOUR_OF_DAY, 0);
                    calendar.set(Calendar.MINUTE, 0);
                    calendar.set(Calendar.SECOND, 0);
                    calendar.set(Calendar.MILLISECOND, 0);

                    return calendar;
                }

                // Minutes
                if (check(st, ":") && (st.hasMoreTokens())) {
                    int minutes = Integer.parseInt(st.nextToken());
                    calendar.set(Calendar.MINUTE, minutes);
                } else {
                    calendar.set(Calendar.MINUTE, 0);
                    calendar.set(Calendar.SECOND, 0);
                    calendar.set(Calendar.MILLISECOND, 0);

                    return calendar;
                }

                //
                // Not mandatory now
                //
                // Secondes
                if (!st.hasMoreTokens()) {
                    return calendar;
                }

                String tok = st.nextToken();

                if (tok.equals(":")) { // seconds

                    if (st.hasMoreTokens()) {
                        int secondes = Integer.parseInt(st.nextToken());
                        calendar.set(Calendar.SECOND, secondes);

                        if (!st.hasMoreTokens()) {
                            return calendar;
                        }

                        // frac sec
                        tok = st.nextToken();

                        if (tok.equals(".")) {
                            // bug fixed, thx to Martin Bottcher
                            String nt = st.nextToken();
                            StringBuffer ntbuffer = new StringBuffer(nt);
                            while (ntbuffer.length() < 3) {
                                ntbuffer.append("0");
                            }
                            nt = ntbuffer.toString();
                            nt = nt.substring(0, 3); //Cut trailing chars..

                            int millisec = Integer.parseInt(nt);

                            //int millisec = Integer.parseInt(st.nextToken()) * 10;
                            calendar.set(Calendar.MILLISECOND, millisec);

                            if (!st.hasMoreTokens()) {
                                return calendar;
                            }

                            tok = st.nextToken();
                        } else {
                            calendar.set(Calendar.MILLISECOND, 0);
                        }
                    } else {
                        throw new RuntimeException("No secondes specified");
                    }
                } else {
                    calendar.set(Calendar.SECOND, 0);
                    calendar.set(Calendar.MILLISECOND, 0);
                }

                // Timezone
                if (!tok.equals("Z")) { // UTC

                    if (!(tok.equals("+") ||  tok.equals("-"))) {
                        throw new RuntimeException("only Z, + or - allowed");
                    }

                    boolean plus = tok.equals("+");

                    if (!st.hasMoreTokens()) {
                        throw new RuntimeException("Missing hour field");
                    }

                    int tzhour = Integer.parseInt(st.nextToken());
                    int tzmin = 0;

                    if (check(st, ":") && (st.hasMoreTokens())) {
                        tzmin = Integer.parseInt(st.nextToken());
                    } else {
                        throw new RuntimeException("Missing minute field");
                    }

                    // Since the time is represented at UTC (tz 0) format
                    // we need to convert the local time to UTC timezone
                    // for example if PST (-8) is 1.00 PM then UTC is 9.00 PM
                    int rawOffset = (tzhour * 60 + tzmin) * 60 * 1000;
                    if (!plus) {
                        calendar.getTimeZone().setRawOffset(-rawOffset);
                        //calendar.add(Calendar.HOUR, tzhour);
                        //calendar.add(Calendar.MINUTE, tzmin);
                    } else {
                        calendar.getTimeZone().setRawOffset(rawOffset);
                        //calendar.add(Calendar.HOUR, -tzhour);
                        //calendar.add(Calendar.MINUTE, -tzmin);
                    }
                }
            } catch (NumberFormatException ex) {
                throw new RuntimeException("[" + ex.getMessage()
                                           + "] is not an integer");
            }

            return calendar;
        }

        /**
         * Parse the given string in ISO 8601 format and build a Date object.
         *
         * @param isodate the date in ISO 8601 format
         *
         * @return a Date instance
         *
         * @exception RuntimeException if the date is not valid
         */
        public static java.util.Date parse(String isodate)
            throws RuntimeException {
            Calendar calendar = getCalendar(isodate);

            return calendar.getTime();
        }

        /**
         * DOCUMENT ME!
         *
         * @param i DOCUMENT ME!
         *
         * @return DOCUMENT ME!
         */
        private static String twoDigit(int i) {
            if ((i >= 0) && (i < 10)) {
                return "0" + String.valueOf(i);
            }

            return String.valueOf(i);
        }

        /**
         * Generate a ISO 8601 date
         *
         * @param date a Date instance
         *
         * @return a string representing the date in the ISO 8601 format
         */
        public static String getIsoDate(java.util.Date date) {
            Calendar calendar = new GregorianCalendar(TimeZone.getTimeZone(
                                                                           "UTC"));
            calendar.setTime(date);

            StringBuffer buffer = new StringBuffer();
            buffer.append(calendar.get(Calendar.YEAR));
            buffer.append("-");
            buffer.append(twoDigit(calendar.get(Calendar.MONTH) + 1));
            buffer.append("-");
            buffer.append(twoDigit(calendar.get(Calendar.DAY_OF_MONTH)));
            buffer.append("T");
            buffer.append(twoDigit(calendar.get(Calendar.HOUR_OF_DAY)));
            buffer.append(":");
            buffer.append(twoDigit(calendar.get(Calendar.MINUTE)));
            buffer.append(":");
            buffer.append(twoDigit(calendar.get(Calendar.SECOND)));
            buffer.append(".");
            buffer.append(twoDigit(calendar.get(Calendar.MILLISECOND) / 10));
            buffer.append("Z");

            return buffer.toString();
        }
    }
