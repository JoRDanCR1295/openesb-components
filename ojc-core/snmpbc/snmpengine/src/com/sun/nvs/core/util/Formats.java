/*
 * Copyright 2005 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.nvs.core.util;

import com.sun.nvs.core.north.base.IOContext;

import java.io.*;

import java.text.SimpleDateFormat;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.1 $
  */
public class Formats {
    /**
     * DOCUMENT ME!
     */
    public static final String SPACES = "                                                  " +
        "                                                   " +
        "                                                   " +
        "                                                   " +
        "                                                   ";

    /**
     * DOCUMENT ME!
     */
    public static final int MILLIS_IN_DAY = 24 * 60 * 60 * 1000;

    /**
     * DOCUMENT ME!
     */
    public static final int MILLIS_IN_HOUR = 60 * 60 * 1000;

    /**
     * DOCUMENT ME!
     */
    public static final int MILLIS_IN_MIN = 60 * 1000;

    /**
     * DOCUMENT ME!
     */
    public static final int MILLIS_IN_SEC = 1000;

    /**
     * DOCUMENT ME!
     */
    public static final int SECS_IN_DAY = 24 * 60 * 60;

    /**
     * DOCUMENT ME!
     */
    public static final int SECS_IN_HOUR = 60 * 60;

    /**
     * DOCUMENT ME!
     */
    public static final int SECS_IN_MIN = 60;

    /**
     * DOCUMENT ME!
     */
    public static final String[] monthNames = {
            "january", "february", "march", "april", "may", "june", "july", "august", "september",
            "october", "november", "december",
        };

    /**
     * DOCUMENT ME!
     */
    public static final SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy HH:mm");

    /**
     * DOCUMENT ME!
     */
    public static final SimpleDateFormat durationFormat = new SimpleDateFormat("HH:mm:ss");

    /**
     * DOCUMENT ME!
     */
    static final String[][] ifNameReplacements = {
            {"fastethernet", "Fa",},
            {"ethernet", "Eth",},
            {"dot11radio", "Do",},
            {"port-channel", "Po",},
            {"gigabitethernet", "Gi"},
            {"processor", "Proc"},
            {"pix system memory", "PIXSysMem"},
            {"general", "Gene"},
        };

    /**
     * DOCUMENT ME!
     */
    static final String[][] xmlEscapes = {
            {"<", "&lt;",},
            {">", "&gt;",},
            {"&", "&amp;",},
            {"\'", "&apos;",},
            {"\"", "&quot;",},
        };

    /**
     * DOCUMENT ME!
     *
     * @param t DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String formatTime(long t) {
        return dateFormat.format(new Date(t));
    }

    /**
     * DOCUMENT ME!
     *
     * @param dateTime DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final long parseTime(String dateTime) {
        try {
            return dateFormat.parse(dateTime).getTime();
        } catch (Exception ex) {
        }

        return -1;
    }

    /**
     * DOCUMENT ME!
     *
     * @param valueInSecs DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getDurationInDHM(long valueInSecs) {
        StringBuffer strB = new StringBuffer();

        int days = (int) Math.floor(valueInSecs / SECS_IN_DAY);
        long remAfterDays = (valueInSecs - (days * SECS_IN_DAY));

        int hrs = (int) Math.floor(remAfterDays / SECS_IN_HOUR);
        long remAfterHrs = (remAfterDays - (hrs * SECS_IN_HOUR));

        int mins = (int) Math.floor(remAfterHrs / SECS_IN_MIN);
        long remAfterMin = (remAfterHrs - (mins * SECS_IN_MIN));

        strB.append(TableFormat.fixLength("" + days, 3, false) + " d ");
        strB.append(TableFormat.fixLengthLeftAlign("" + hrs, "0", 2) + ":");
        strB.append(TableFormat.fixLengthLeftAlign("" + mins, "0", 2));

        return strB.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param valueInSecs DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getDurationInDHMS(long valueInSecs) {
        StringBuffer strB = new StringBuffer();

        int days = (int) Math.floor(valueInSecs / SECS_IN_DAY);
        long remAfterDays = (valueInSecs - (days * SECS_IN_DAY));

        int hrs = (int) Math.floor(remAfterDays / SECS_IN_HOUR);
        long remAfterHrs = (remAfterDays - (hrs * SECS_IN_HOUR));

        int mins = (int) Math.floor(remAfterHrs / SECS_IN_MIN);
        long remAfterMin = (remAfterHrs - (mins * SECS_IN_MIN));

        long secs = remAfterMin;

        strB.append(TableFormat.fixLength("" + days, 3, false) + " d ");
        strB.append(TableFormat.fixLengthLeftAlign("" + hrs, "0", 2) + ":");
        strB.append(TableFormat.fixLengthLeftAlign("" + mins, "0", 2) + ":");
        strB.append(TableFormat.fixLengthLeftAlign("" + secs, "0", 2));

        return strB.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param gc DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String calendarToString(GregorianCalendar gc) {
        if (gc == null) {
            return null;
        }

        int year = gc.get(Calendar.YEAR);
        int month = gc.get(Calendar.MONTH);
        int day = gc.get(Calendar.DATE);
        int hour = gc.get(Calendar.HOUR_OF_DAY);
        int mins = gc.get(Calendar.MINUTE);

        return monthNames[month] + " " + day + " " + year + " " + hour + " " + mins;
    }

    /**
     * DOCUMENT ME!
     *
     * @param gc DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String calendarToXMLString(GregorianCalendar gc) {
        if (gc == null) {
            return null;
        }

        int year = gc.get(Calendar.YEAR);
        int month = gc.get(Calendar.MONTH);
        int day = gc.get(Calendar.DATE);
        int hour = gc.get(Calendar.HOUR_OF_DAY);
        int mins = gc.get(Calendar.MINUTE);

        StringBuffer strB = new StringBuffer();

        strB.append("<Year>" + year + "</Year>");
        strB.append("<Month>" + monthNames[month] + "</Month>");
        strB.append("<Day>" + day + "</Day>");
        strB.append("<Hour>" + hour + "</Hour>");
        strB.append("<Minutes>" + mins + "</Minutes>");

        return strB.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param length DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String getSpaces(int length) {
        if (length <= 0) {
            return "";
        } else {
            return SPACES.substring(0, length);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String quoteIfNeeded(String s) {
        boolean needed = false;

        if (s.indexOf(" ") >= 0) {
            needed = true;
        } else if (s.indexOf("\t") >= 0) {
            needed = true;
        }

        if (needed) {
            return "\"" + s + "\"";
        } else {
            return s;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param longIfName DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String toShortInterfaceName(String longIfName) {
        String s = longIfName.toLowerCase();

        for (int i = 0; i < ifNameReplacements.length; i++) {
            String pattern = ifNameReplacements[i][0];
            String repl = ifNameReplacements[i][1];

            if (s.startsWith(pattern)) {
                String str = s.substring(pattern.length());

                return repl + str;
            }
        }

        return longIfName;
    }

    /**
     * DOCUMENT ME!
     *
     * @param io DOCUMENT ME!
     * @param s DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String conditionalEscape(IOContext io, String s) {
        String formatType = io.getFormatType();

        if ((formatType != null) && formatType.equals(FormatTypes.XML)) {
            return escape(s);
        } else {
            return s;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static final String escape(String s) {
        if (s == null) {
            return "";
        }

        StringBuffer result = new StringBuffer();

        while (s.length() > 0) {
            boolean found = false;

            for (int i = 0; i < xmlEscapes.length; i++) {
                int idx = s.indexOf(xmlEscapes[i][0]);

                if (idx >= 0) {
                    result.append(s.substring(0, idx));
                    result.append(xmlEscapes[i][1]);
                    s = s.substring(idx + xmlEscapes[i][0].length());
                    found = true;

                    break;
                }
            }

            if (!found) {
                result.append(s);

                break;
            }
        }

        return result.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     * @param prefix DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String insertPrefixForEachLine(String s, String prefix) {
        String[] lines = ParseUtils.split(s, "\n\r");
        StringBuffer sb = new StringBuffer();

        for (int j = 0; j < lines.length; j++) {
            sb.append(prefix + lines[j] + "\n");
        }

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param fileName DOCUMENT ME!
     * @param insertPrefix DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public static String readTextFile(String fileName, String insertPrefix)
        throws Exception {
        BufferedReader r = new BufferedReader(new FileReader(fileName));

        String line = null;
        StringBuffer sb = new StringBuffer();

        while ((line = r.readLine()) != null) {
            sb.append(insertPrefix + line + "\n");
        }

        r.close();

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param fileName DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public static String readTextFile(String fileName)
        throws Exception {
        return readTextFile(fileName, "");
    }

    /**
     * DOCUMENT ME!
     *
     * @param s DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String capitalizeFirstLetter(String s) {
        if (s.length() == 0) {
            return s;
        }

        StringBuffer sb = new StringBuffer();
        int len = s.length();

        for (int i = 0; i < len; i++) {
            char c = s.charAt(i);

            if (i == 0) {
                c = Character.toUpperCase(c);
            }

            sb.append(c);
        }

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param tokens DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String capitalizeFirstLetterAndAppend(String[] tokens) {
        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < tokens.length; i++) {
            sb.append(capitalizeFirstLetter(tokens[i]) + " ");
        }

        return sb.toString().trim();
    }

    /**
     * DOCUMENT ME!
     *
     * @param taskId DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String formatTaskName(String taskId) {
        String[] tokens = ParseUtils.split(taskId, ".");

        return capitalizeFirstLetterAndAppend(tokens);
    }

    /**
     * DOCUMENT ME!
     *
     * @param msg DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String formatHeader(String msg) {
        return "<color:header>" + msg + "<color:none>";
    }

    /**
     * DOCUMENT ME!
     *
     * @param msg DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String formatError(String msg) {
        return "<color:error>" + msg + "<color:none>";
    }

    /**
     * DOCUMENT ME!
     *
     * @param tokens DOCUMENT ME!
     * @param seperator DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String concat(String[] tokens, String seperator) {
        StringBuffer sb = new StringBuffer();

        for (int i = 0; i < tokens.length; i++) {
            if (i != 0) {
                sb.append(seperator);
            }

            sb.append(tokens[i]);
        }

        return sb.toString();
    }

    /**
     * DOCUMENT ME!
     *
     * @param str DOCUMENT ME!
     * @param length DOCUMENT ME!
     * @param delims DOCUMENT ME!
     *
     * @throws Exception DOCUMENT ME!
     */
    public static void isValidWord(String str, int length, String delims)
        throws Exception {
        String[] strA = ParseUtils.split(str, delims);

        if (strA.length > 1) {
            throw new Exception("Invalid characters in input");
        }

        if (str.length() > length) {
            throw new Exception("Length of the input should be less than or equal to " + length);
        }
    }
}
