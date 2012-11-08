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
 * @(#)TimeZoneParserImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dt.impl;

import java.text.DecimalFormat;
import java.util.TimeZone;

import com.sun.jbi.engine.bpel.core.bpel.dt.TimeZoneParser;


/**
 * DOCUMENT ME!
 *
 * @author Sun Microsystems
 * @version 
 */
class TimeZoneParserImpl implements TimeZoneParser {
    /**
     * DOCUMENT ME!
     */
    static final DecimalFormat FMT = new DecimalFormat("00");

    /**
     * DOCUMENT ME!
     *
     * @param value DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static TimeZone parse(String value) {
        if ((value == null) || (value.length() == 0)) {
            return null;
        }

        if (value.equals("Z")) {
            return TimeZone.getTimeZone("GMT");
        }

        return TimeZone.getTimeZone("GMT" + value);
    }

    /**
     * DOCUMENT ME!
     *
     * @param tz DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static String format(TimeZone tz) {
        // [+-]hh:mm
        // Z
        if (tz == null) {
            return "";
        }

        int offset = tz.getRawOffset(); // milliseconds

        if (offset == 0) {
            return "Z";
        }

        int seconds = offset / 1000;
        int minutes = seconds / 60;
        int hours = minutes / 60;
        minutes %= 60;

        // For negatives, FMT.format will add the - sign
        String result = (tz.getRawOffset() < 0) ? "" : "+";
        result += FMT.format(hours);
        result += ":";
        result += FMT.format(minutes);

        return result;
    }
}
