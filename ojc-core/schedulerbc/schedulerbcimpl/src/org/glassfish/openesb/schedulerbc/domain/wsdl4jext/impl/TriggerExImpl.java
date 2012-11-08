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
 * @(#)ActivePeriodExImpl.java
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain.wsdl4jext.impl;

import java.text.NumberFormat;
import java.text.ParseException;
import javax.xml.namespace.QName;
import org.glassfish.openesb.schedulerbc.domain.StringableException;
import org.glassfish.openesb.schedulerbc.domain.wsdl4jext.TriggerEx;

/**
 * Implements the WSDL Input extension.
 * 
 * @author sunsoabi_edwong
 */
public class TriggerExImpl implements TriggerEx {

    private String name;
    private String type;
    private boolean enabled;
    private String description;
    private String repeat;
    private String interval;
    private String cronExpr;
    private String duration;
    private String message;

    static final String CLASSID = TriggerExImpl.class.getName() + DELIM;
    
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
    
    public String getType() {
        return type;
    }
    
    public void setType(String type) {
        this.type = type;
    }
    
    public boolean isEnabled() {
        return enabled;
    }
    
    public void setEnabled(String enabled) {
        this.enabled = Boolean.parseBoolean(enabled);
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getRepeat() {
        return repeat;
    }

    public void setRepeat(String repeat) {
        this.repeat = repeat;
    }

    public String getInterval() {
        return interval;
    }

    public void setInterval(String interval) {
        this.interval = interval;
    }
    
    public String getCronExpr() {
        return cronExpr;
    }
    
    public void setCronExpr(String cronExpr) {
        this.cronExpr = cronExpr;
    }
    
    public String getDuration() {
        return duration;
    }
    
    public void setDuration(String duration) {
        this.duration = duration;
    }

    public void setElementType(QName arg0) {}

    public QName getElementType() {
        return ELEM_TYPE;
    }

    public void setRequired(Boolean arg0) {}

    public Boolean getRequired() {
        return Boolean.TRUE;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    private String[] readFields(String data) {
        int offset = CLASSID.length();
        String[] fields = new String[9];
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

    public void readObject(String data) throws StringableException {
        if ((null == data) || (data.trim().length() == 0)
                || !data.startsWith(CLASSID)) {
            throw new StringableException("Invalid data!");             //NOI18N
        }
        String[] fields = readFields(data);
        setName(fields[0]);
        setType(fields[1]);
        setEnabled(fields[2]);
        setDescription(fields[3]);
        setRepeat(fields[4]);
        setInterval(fields[5]);
        setCronExpr(fields[6]);
        setDuration(fields[7]);
        setMessage(fields[8]);
    }

    public void writeObject(StringBuilder data) throws StringableException {
        data.append(CLASSID);
        writeField(data, getName());
        writeField(data, getType());
        writeField(data, isEnabled());
        writeField(data, getDescription());
        writeField(data, getRepeat());
        writeField(data, getInterval());
        writeField(data, getCronExpr());
        writeField(data, getDuration());
        writeField(data, getMessage());
    }

    private StringBuilder writeField(StringBuilder data, String field) {
        return data.append((field != null) ? field.length() : 0).append(DELIM)
                .append((field != null) ? field : "");                  //NOI18N
    }

    private StringBuilder writeField(StringBuilder data, boolean field) {
        String bStr = Boolean.toString(field);
        return data.append(bStr.length()).append(DELIM).append(bStr);
    }

    public static TriggerEx valueOf(String data) throws StringableException {
        if (null == data) {
            return null;
        }
        TriggerEx value = new TriggerExImpl();
        value.readObject(data);
        return value;
    }

    public String valueAsString() throws StringableException {
        StringBuilder sb = new StringBuilder();
        writeObject(sb);
        return sb.toString();
    }

}
