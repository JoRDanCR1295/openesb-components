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
 * @(#)SQLUtils.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.sql.framework.jdbc;

import com.sun.sql.framework.exception.BaseException;
import com.sun.sql.framework.utils.RuntimeAttribute;
import com.sun.sql.framework.utils.StringUtil;
import java.sql.PreparedStatement;
import java.sql.Types;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.logging.Logger;
import com.sun.jbi.internationalization.Messages;

/**
 * Utility class supplying lookup and conversion methods for SQL-related tasks.
 * 
 * @author Srinivasan Rengarajan
 * @author Girish Patil
 *
 * @version 1.0
 */
public class SQLUtils {
    
    private static final Messages mMessages = Messages.getMessages(SQLUtils.class);
    private static final String dbLinkSql = "CREATE DATABASE LINK \"{0}\" ( DS_JNDI_NAME=";
    private static final String connParams = " DRIVER=\'default\' URL=\'default\' USERNAME=\'default\' PASSWORD=\'default\' ";

    private static String quoteString(String strToWrap) {
        return "'" + strToWrap + "'";
    }

    public static String createDBLinkSQL(String linkName, String jndiName) {
        MessageFormat form = new MessageFormat(dbLinkSql);
        Object[] args = new Object[]{linkName,quoteString(""),"","",""};
        String dbLinkSql = form.format(args) + quoteString(jndiName) + connParams + ")";
        return dbLinkSql;
    }

    /**
     * Create a SQL String to be used with java.sql.PreparedStatement by substituting
     * symbols beginning "$" with "?". Binding variable order is preserved.
     * 
     * @pre rawSql does not contain any "?"
     * @post processedSql has all the "$attributeName" in the rawSql is replaced with "?"
     *       where "attributeName" is name/key in attrMap
     * @param rawSql
     * @param attrMap map of RuntimeAttribute attribute name and RuntimeAttribute.
     * @param paramList
     * @return preparedStatement string
     */
    public static String createPreparedStatement(String rawSql, Map attrMap, List paramList) {
        Iterator iter = attrMap.values().iterator();
        if (!iter.hasNext()) {
            return rawSql;
        }

        if (paramList != null) {
            List orderedSymbolList = SQLUtils.getOrderedSymbolList(rawSql, attrMap);
            paramList.clear();
            paramList.addAll(orderedSymbolList);
        }
        String processedSql = rawSql;

        do {
            RuntimeAttribute attr = (RuntimeAttribute) iter.next();
            boolean flag = false;
            do {
                processedSql = StringUtil.replaceFirst(processedSql, "?", "\\$" + attr.getAttributeName());
                if (!rawSql.equals(processedSql)) {
                    flag = true;
                } else {
                    flag = false;
                }

                rawSql = processedSql;
            } while (flag);
        } while (iter.hasNext());

        Logger.getLogger(SQLUtils.class.getName()).info(mMessages.getString("EDMSE-I0443.Generated_PreparedStatement") + processedSql);
        return processedSql;
    }
    
        /**
     * Returns the list of the Sumbol Names in "attrMap" available in "rawSql" in the
     * order of appearance.
     *
     * @param rawSql
     * @param attrMap
     * @return
     */
    private static List getOrderedSymbolList(String rawSql, Map attrMap) {
        Map map = new TreeMap();

        if ((rawSql != null) && (attrMap != null)) {
            Iterator iter = attrMap.values().iterator();

            RuntimeAttribute attr = null;

            int pos = -1;
            int indexFrom = 0;

            while (iter.hasNext()) {
                attr = (RuntimeAttribute) iter.next();
                indexFrom = 0;
                boolean morePresent = true;
                while (morePresent) {
                    pos = rawSql.indexOf("$" + attr.getAttributeName(), indexFrom);
                    if (pos >= 0) {
                        map.put(new Integer(pos), attr.getAttributeName());
                        indexFrom = pos + attr.getAttributeName().length();
                    } else {
                        morePresent = false;
                    }
                }
            }
        }

        return new ArrayList(map.values());
    }
    
    public static void populatePreparedStatement(PreparedStatement ps, Map attrMap, List paramList, HashMap dynamicMap) throws BaseException {
        ListIterator iter = paramList.listIterator();
        try {
            while (iter.hasNext()) {
                String attrName = (String) iter.next();
                RuntimeAttribute attr = (RuntimeAttribute) attrMap.get(attrName);
                int index = iter.nextIndex();
                int jdbcType = attr.getJdbcType();
                //Object valueObj = attr.getAttributeObject();
                Object valueObj = dynamicMap.get(attrName);
                if(valueObj.equals("EMPTYNODE") || valueObj == null) {
                    // If the user does not pass the argument value (empty element)
                    // for the runtime argument, we should use default
                    //attrMap contains default (designtime) values for runtime arguments.
                    valueObj = attr.getAttributeObject();
                }
                Number numberObj = null;

                switch (jdbcType) {

                    case Types.DOUBLE:
                        numberObj = (valueObj instanceof Number) ? (Number) valueObj : Double.valueOf(valueObj.toString());
                        ps.setDouble(index, numberObj.doubleValue());
                        break;

                    case Types.FLOAT:
                        numberObj = (valueObj instanceof Number) ? (Number) valueObj : Float.valueOf(valueObj.toString());
                        ps.setFloat(index, numberObj.floatValue());
                        break;

                    case Types.INTEGER:
                        numberObj = (valueObj instanceof Number) ? (Number) valueObj : Integer.valueOf(valueObj.toString());
                        ps.setInt(index, numberObj.intValue());
                        break;

                    case Types.TIMESTAMP:
                        long ts = com.sun.sql.framework.jdbc.SQLUtils.convertFromIso8601(valueObj.toString());
                        Logger.getLogger(SQLUtils.class.getName()).info("**** timestamp **** " + ts);
                        try {
                            ps.setTimestamp(index, new java.sql.Timestamp(ts));
                        } catch (java.sql.SQLException e) {
                            ps.setDate(index, new java.sql.Date(ts));
                        }
                        break;

                    case Types.CHAR:
                    case Types.VARCHAR:
                    default:
                        ps.setString(index, valueObj.toString());
                        break;
                }
            }
        } catch (Exception e) {
            String details = e.getMessage();
            if (StringUtil.isNullString(details)) {
                details = e.toString();
            }
            Logger.getLogger(SQLUtils.class.getName()).severe(e.getMessage());
            throw new BaseException(details, e);
        }
    }

        /**
     * returns a Gregorian Calendar of given iso date
     * 
     * @param isodate date in YYYY-MM-DDThh:mm:ss.sTZD format
     * @return GregorianCalendar
     */
    public static GregorianCalendar getCalendar(String isodate) {
        // YYYY-MM-DDThh:mm:ss.sTZD
        StringTokenizer st = new StringTokenizer(isodate, "-T:.+Z", true);

        GregorianCalendar calendar = new GregorianCalendar(TimeZone.getTimeZone("UTC"));
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
            if (check(st, "-") && (st.hasMoreTokens())) {
                int month = Integer.parseInt(st.nextToken()) - 1;
                calendar.set(Calendar.MONTH, month);
            } else {
                return calendar;
            }

            // Day
            if (check(st, "-") && (st.hasMoreTokens())) {
                int day = Integer.parseInt(st.nextToken());
                calendar.set(Calendar.DAY_OF_MONTH, day);
            } else {
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

                        while (nt.length() < 3) {
                            nt += "0";
                        }

                        nt = nt.substring(0, 3); // Cut trailing chars..

                        int millisec = Integer.parseInt(nt);

                        // int millisec = Integer.parseInt(st.nextToken()) * 10;
                        calendar.set(Calendar.MILLISECOND, millisec);

                        if (!st.hasMoreTokens()) {
                            return calendar;
                        }

                        tok = st.nextToken();
                    } else {
                        calendar.set(Calendar.MILLISECOND, 0);
                    }
                } else {
                    throw new RuntimeException(mMessages.getString("EDMSE-E0126.No_secondes_specified"));
                }
            } else {
                calendar.set(Calendar.SECOND, 0);
                calendar.set(Calendar.MILLISECOND, 0);
            }

            // Timezone
            if (!tok.equals("Z")) { // UTC

                if (!(tok.equals("+") || tok.equals("-"))) {
                    throw new RuntimeException(mMessages.getString("EDMSE-E0127.alloed_Chars"));
                }

                boolean plus = tok.equals("+");

                if (!st.hasMoreTokens()) {
                    throw new RuntimeException(mMessages.getString("EDMSE-E0128.Missing_hour_field"));
                }

                int tzhour = Integer.parseInt(st.nextToken());
                int tzmin;

                if (check(st, ":") && (st.hasMoreTokens())) {
                    tzmin = Integer.parseInt(st.nextToken());
                } else {
                    throw new RuntimeException(mMessages.getString("EDMSE-E0129.Missing_minute_field"));
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
        } catch (NumberFormatException ex) {
            throw new RuntimeException("[" + ex.getMessage() + "]"+mMessages.getString("EDMSE-E0130.Not_an_Integer"));
        }

        return calendar;
    }
    
    private static boolean check(StringTokenizer st, String token) throws RuntimeException {
        try {
            if (st.nextToken().equals(token)) {
                return true;
            }
            throw new RuntimeException("Missing [" + token + "]");
        } catch (NoSuchElementException ex) {
            return false;
        }
    }
        
    /**
     * convertFromIso8601
     * 
     * @param isoDateTime - ISO datetime
     * @return a long value
     */
    public static long convertFromIso8601(String isoDateTime) {
        return getCalendar(isoDateTime).getTimeInMillis();
    }
    
    /* Private no-arg constructor; this class should not be instantiable. */
    private SQLUtils() {
    }
}
