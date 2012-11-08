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
 * @(#)DateParserImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.xpath;

import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.TimeZone;

import com.sun.jbi.engine.workflow.util.I18n;

/**
 * Implementation of Date Parser
 */
public class DateParserImpl {

	static final TimeZone UTC = TimeZone.getTimeZone("UTC");
	
    

    //static ResourceBundle rb = ResourceBundle.getBundle("com.sun.jbi.engine.bpel.core.bpel.xpath.functions.Bundle");

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

        GregorianCalendar calendar = new GregorianCalendar(UTC);
        calendar.clear();

        try {
            // Year
            if (st.hasMoreTokens()) {
                int year = Integer.parseInt(st.nextToken());
                calendar.set(Calendar.YEAR, year);
            } else {
                return calendar;
            }

            // Month
            if (check(st, "-", isodate) && (st.hasMoreTokens())) {
                int month = Integer.parseInt(st.nextToken()) - 1;
                calendar.set(Calendar.MONTH, month);
            } else {
                return calendar;
            }

            // Day
            if (check(st, "-", isodate) && (st.hasMoreTokens())) {
                int day = Integer.parseInt(st.nextToken());
                calendar.set(Calendar.DAY_OF_MONTH, day);
            } else {
                return calendar;
            }
          
          
            if( !st.hasMoreTokens() ){
            	calendar.set(Calendar.HOUR_OF_DAY, 0);
                calendar.set(Calendar.MINUTE, 0);
                calendar.set(Calendar.SECOND, 0);
                calendar.set(Calendar.MILLISECOND, 0);
                
                return calendar;
            }
            
            String tok = st.nextToken();
            if ( !"T".equals(tok) ){
                calendar.set(Calendar.HOUR_OF_DAY, 0);
                calendar.set(Calendar.MINUTE, 0);
                calendar.set(Calendar.SECOND, 0);
                calendar.set(Calendar.MILLISECOND, 0);

            	parseTimeZone(tok, st, calendar, isodate);
            	
            	return calendar;
            	
            } else if ( !st.hasMoreTokens() ){
            	
            	throw new RuntimeException(I18n.loc("WLM-6050: Missing Time in {0}", isodate));
            }

            //Hour
            int hour = Integer.parseInt(st.nextToken());
            calendar.set(Calendar.HOUR_OF_DAY, hour);

            //Minutes
            tok = st.nextToken();
            if (":".equals(tok) && (st.hasMoreTokens())) {
                int minutes = Integer.parseInt(st.nextToken());
                calendar.set(Calendar.MINUTE, minutes);
            } else {
                calendar.set(Calendar.MINUTE, 0);
                calendar.set(Calendar.SECOND, 0);
                calendar.set(Calendar.MILLISECOND, 0);
            }     
            
            // Secondes
            if (!st.hasMoreTokens()) {
                return calendar;
            }

            tok = st.nextToken();
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

                        while (nt.length() < 3) {
                            nt += "0";
                        }

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
                    throw new RuntimeException(I18n.loc("WLM-6051: Missing second in {0}", st));
                }
            } else {
                calendar.set(Calendar.SECOND, 0);
                calendar.set(Calendar.MILLISECOND, 0);
            }

            // Timezone
            parseTimeZone(tok, st, calendar, isodate);
            
        } catch (NumberFormatException ex) {
            throw new RuntimeException("[" + ex.getMessage()
                                       + "] " + "Not Integer");
        }

        return calendar;
    }


    /**
     * Generate a Local date time
     * 
     * @param date a Date instance
     * @return a string representing the date in the ISO 8601 format
     */
    public static String getLocalDateTime(Calendar calendar) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(fourDigit(calendar.get(Calendar.YEAR)));
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
        
        int UTCZoneOffsetInMillis = calendar.get(Calendar.ZONE_OFFSET);
        int offSetHours = getUTCZoneOffsetHours(UTCZoneOffsetInMillis);
        int offSetMinutes = getUTCZoneOffsetMinutes(UTCZoneOffsetInMillis);
        
        buffer.append(getTimeZoneOffset(offSetHours, offSetMinutes)); // in hours        
        
        return buffer.toString();
    }

    /**
     * Generate a Local date
     * 
     * @param date a Date instance
     * @return a string representing the date in the ISO 8601 format
     */
    public static String getLocalDate(Calendar calendar) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(fourDigit(calendar.get(Calendar.YEAR)));
        buffer.append("-");
        buffer.append(twoDigit(calendar.get(Calendar.MONTH) + 1));
        buffer.append("-");
        buffer.append(twoDigit(calendar.get(Calendar.DAY_OF_MONTH)));
        int UTCZoneOffsetInMillis = calendar.get(Calendar.ZONE_OFFSET);
        int offSetHours = getUTCZoneOffsetHours(UTCZoneOffsetInMillis);
        int offSetMinutes = getUTCZoneOffsetMinutes(UTCZoneOffsetInMillis);
        
        buffer.append(getTimeZoneOffset(offSetHours, offSetMinutes)); // in hours          
        return buffer.toString();
    }

    /**
     * Generate a Local date
     * 
     * @param date a Date instance
     * @return a string representing the date in the ISO 8601 format
     */
    public static String getLocalTime(Calendar calendar) {
        StringBuffer buffer = new StringBuffer();
        buffer.append(twoDigit(calendar.get(Calendar.HOUR_OF_DAY)));
        buffer.append(":");
        buffer.append(twoDigit(calendar.get(Calendar.MINUTE)));
        buffer.append(":");
        buffer.append(twoDigit(calendar.get(Calendar.SECOND)));
        buffer.append(".");
        buffer.append(twoDigit(calendar.get(Calendar.MILLISECOND) / 10));
        int UTCZoneOffsetInMillis = calendar.get(Calendar.ZONE_OFFSET);
        int offSetHours = getUTCZoneOffsetHours(UTCZoneOffsetInMillis);
        int offSetMinutes = getUTCZoneOffsetMinutes(UTCZoneOffsetInMillis);
        
        buffer.append(getTimeZoneOffset(offSetHours, offSetMinutes)); // in hours
        return buffer.toString();
    }
    
    /**
     * @param UTCZoneOffsetInMillis
     * @return
     */
    private static int getUTCZoneOffsetHours(int UTCZoneOffsetInMillis) {
        return UTCZoneOffsetInMillis/(60*60*1000);
    }
    
    /**
     * @param UTCZoneOffsetInMillis
     * @return
     */
    private static int getUTCZoneOffsetMinutes(int UTCZoneOffsetInMillis) {
        return (UTCZoneOffsetInMillis % (60*60*1000))/(1000*60);
    }
    
    /**
     * @param hours
     * @param mins
     * @return
     */
    private static String getTimeZoneOffset(int hours, int mins) {
        StringBuffer offset = new StringBuffer();
        
        if (hours >= 0 && hours < 10) {
            offset.append("0").append(String.valueOf(hours));
        } else if (hours >= -10 && hours < 0) {
            String value = String.valueOf(hours);
            offset.append("-0").append(value.substring(1));
        } else {
            offset.append(String.valueOf(hours));
        }
        
        offset.append(":");
        
        if (mins >= 0 && mins < 10) {
            offset.append("0").append(String.valueOf(mins));
        } else if (mins >= -10 && mins < 0) {
            String value = String.valueOf(mins);
            offset.append("0").append(value.substring(1));    
        } else {
            offset.append(String.valueOf(mins));
        }
        return offset.toString();
    }
    
    private static String twoDigit(int i) {
        if (i >= 0 && i < 10) {
            return "0" + String.valueOf(i);
        }
        return String.valueOf(i);
    }

    private static String fourDigit(int i) {
        StringBuffer prepend = new StringBuffer();
        if (i >= 0 && i < 1000) {
            prepend.append("0");
            if (i < 100) {
                prepend.append("0");
                if (i < 10)
                    prepend.append("0");
            }
        }
        return prepend.append(String.valueOf(i)).toString();
    }
    
    private static void parseTimeZone(String firstTok, StringTokenizer st, Calendar calendar, String toParse){
    	String tok = firstTok;
        if (!tok.equals("Z")) { // UTC

            if (!(tok.equals("+") ||  tok.equals("-"))) {
                throw new RuntimeException("only Z, + or - allowed");
            }

            boolean plus = tok.equals("+");

            if (!st.hasMoreTokens()) {
                throw new RuntimeException(I18n.loc("WLM-6052: Missing hour in {0}", toParse));
            }

            int tzhour = Integer.parseInt(st.nextToken());
            int tzmin = 0;

            if (check(st, ":", toParse) && (st.hasMoreTokens())) {
                tzmin = Integer.parseInt(st.nextToken());
            } else {
                throw new RuntimeException(I18n.loc("WLM-6053: Missing minute in {0}", toParse));
            }

            // Since the time is represented at UTC (tz 0) format
            // we need to convert the local time to UTC timezone
            // for example if PST (-8) is 1.00 PM then UTC is 9.00 PM
            if (!plus) {
                calendar.add(Calendar.HOUR, tzhour);
                calendar.add(Calendar.MINUTE, tzmin);
            } else {
                calendar.add(Calendar.HOUR, -tzhour);
                calendar.add(Calendar.MINUTE, -tzmin);
            }
        }
    	
    }
    
    
    private static boolean check(StringTokenizer st, String token, String toParse)
    throws RuntimeException {
    try {
        if (st.nextToken().equals(token)) {
            return true;
        } else {
            throw new RuntimeException(I18n.loc("WLM-6054: Missing  {0} in {1} ", token, toParse));
        }
    } catch (NoSuchElementException ex) {
        return false;
    }
}


}
