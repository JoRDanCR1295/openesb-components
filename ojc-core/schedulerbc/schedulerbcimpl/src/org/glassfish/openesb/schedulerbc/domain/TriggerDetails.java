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
 * @(#)TriggerDetails.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain;

import com.sun.jbi.common.util.Util;
import java.text.NumberFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.TimeZone;
import java.util.logging.Logger;
import org.glassfish.openesb.schedulerbc.I18n;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.TriggerEx;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.impl.TriggerExImpl;

/**
 * Scheduler trigger model.
 *
 * @author sunsoabi_edwong
 */
public class TriggerDetails implements Stringable {
    private String quartzGroup;
    private String group;
    private SimpleDateFormat dateFormat;
    private String mode;
    private String starting;
    private String ending;
    private TimeZone timezone;
    private TriggerEx triggerEx;

    private Logger logger;

    static final String CLASSID = TriggerDetails.class.getName() + DELIM;
    private static final SimpleDateFormat DEFAULT_SDT = new SimpleDateFormat();
    private static final TimeZone DEFAULT_TZ = TimeZone.getDefault();

    public TriggerDetails() {
        super();
    }
    
    public TriggerDetails(String quartzGroup, String group, String dateFormat,
            String mode, String starting, String ending, String timezone,
            TriggerEx triggerEx, Logger logger) {
        super();

        this.quartzGroup = quartzGroup;
        this.group = group;
        setDateFormat(dateFormat);
        this.mode = mode;
        this.starting = starting;
        this.ending = ending;
        setTimezone(timezone);
        this.triggerEx = triggerEx;
        this.logger = logger;
    }

    void setQuartzGroup(String quartzGroup) {
        this.quartzGroup = quartzGroup;
    }

    public String getQuartzGroup() {
        return quartzGroup;
    }

    void setGroup(String group) {
        this.group = group;
    }

    public String getGroup() {
        return group;
    }

    void setDateFormat(String dateFormat) {
        this.dateFormat = (!Util.isEmpty(dateFormat)
                ? new SimpleDateFormat(dateFormat)
                : null);
    }

    public SimpleDateFormat getDateFormat() {
        return (dateFormat != null) ? dateFormat : DEFAULT_SDT;
    }

    public String getDateFormatAsString() {
        return (dateFormat != null) ? dateFormat.toPattern() : null;
    }

    void setMode(String mode) {
        this.mode = mode;
    }

    public String getMode() {
        return mode;
    }

    void setStarting(String starting) {
        this.starting = starting;
    }

    public String getStarting() {
        return starting;
    }

    void setEnding(String ending) {
        this.ending = ending;
    }

    public String getEnding() {
        return ending;
    }

    void setTimezone(String timezone) {
        if (!Util.isEmpty(timezone) && !isTimeZoneValid(timezone)) {
            I18n.warning(log(), "SCHEDBC-6009: Invalid timezone "       //NOI18N
                    + "ID ({0}) specified; using default instead",      //NOI18N
                    timezone);
            timezone = null;
        }
        this.timezone = (Util.isEmpty(timezone)
                ? null : TimeZone.getTimeZone(timezone));

        getDateFormat().setTimeZone(getTimezone());
    }

    public TimeZone getTimezone() {
        return (timezone != null) ? timezone : DEFAULT_TZ;
    }

    public String getTimezoneAsString() {
        return (timezone != null) ? timezone.getID() : null;
    }

    void setTriggerEx(TriggerEx triggerEx) {
        this.triggerEx = triggerEx;
    }

    public TriggerEx getTriggerEx() {
        return triggerEx;
    }

    private boolean isTimeZoneValid(String timezone) {
        boolean found = false;
        for (String tz : TimeZone.getAvailableIDs()) {
            if (tz.equals(timezone)) {
                found = true;
                break;
            }
        }
        return found;
    }

    public void readObject(String data) throws StringableException {
        if ((null == data) || (data.trim().length() == 0)
                || !data.startsWith(CLASSID)) {
            throw new StringableException("Invalid data!");             //NOI18N
        }
        String[] fields = readFields(data);
        int i = 0;
        setQuartzGroup(fields[i++]);
        setGroup(fields[i++]);
        setDateFormat(fields[i++]);
        setMode(fields[i++]);
        setStarting(fields[i++]);
        setEnding(fields[i++]);
        setTimezone(fields[i++]);
        setTriggerEx(TriggerExImpl.valueOf(fields[i++]));
    }

    public void writeObject(StringBuilder data) throws StringableException {
        data.append(CLASSID);
        writeField(data, getQuartzGroup());
        writeField(data, getGroup());
        writeField(data, getDateFormatAsString());
        writeField(data, getMode());
        writeField(data, getStarting());
        writeField(data, getEnding());
        writeField(data, getTimezoneAsString());
        writeField(data, getTriggerEx());
    }

    public static TriggerDetails valueOf(String data)
            throws StringableException {
        if (null == data) {
            return null;
        }
        TriggerDetails value = new TriggerDetails();
        value.readObject(data);
        return value;
    }

    public String valueAsString() throws StringableException {
        StringBuilder sb = new StringBuilder();
        writeObject(sb);
        return sb.toString();
    }

    private Logger log() {
        if (null == logger) {
            logger = Logger.getLogger(getClass().getName());
        }
        return logger;
    }

    private String[] readFields(String data) {
        int offset = CLASSID.length();
        String[] fields = new String[8];
        int delim = -1;
        int i = 0;
        NumberFormat nf = NumberFormat.getIntegerInstance();
        do {
            delim = data.indexOf(DELIM, offset);
            if (delim != -1) {
                try {
                    int length =
                            nf.parse(data.substring(offset, delim)).intValue();
                    if (length < 0) {
                        throw new StringableException(
                                "Invalid negative length!");            //NOI18N
                    } else {
                        int start = delim + 1;
                        offset = start + length;
                        if (length > 0) {
                            fields[i++] = data.substring(start, offset);
                        } else {
                            fields[i++] = null;
                        }
                    }

                } catch (ParseException pe) {
                    throw new StringableException(pe);
                }
            }
        } while ((delim != -1) && (offset < data.length())
                && (i < fields.length));
        return fields;
    }

    private StringBuilder writeField(StringBuilder data, String field) {
        return data.append((field != null) ? field.length() : 0).append(DELIM)
                .append((field != null) ? field : "");                  //NOI18N
    }

    private StringBuilder writeField(StringBuilder data, TriggerEx field) {
        String fieldStr = (field != null) ? field.valueAsString() : null;
        return data.append((fieldStr != null) ? fieldStr.length() : 0)
                .append(DELIM)
                .append((fieldStr != null) ? fieldStr : "");            //NOI18N
    }
}
