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
 * @(#)SchedulerEndPointInfo.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain;

import com.sun.jbi.common.descriptor.EndpointInfo;
import java.text.NumberFormat;
import java.text.ParseException;
import javax.xml.namespace.QName;

/**
 * Wrapper around EndPointInfo to implement Stringable.
 * @author sunsoabi_edwong
 */
public class SchedulerEndPointInfo extends EndpointInfo
        implements Stringable {

    static final String CLASSID = SchedulerEndPointInfo.class.getName()
            + DELIM;

    private boolean provides;
    private String endPtName;
    private QName interfaceName;
    private QName serviceName;
    private LinkType linkType;

    public SchedulerEndPointInfo() {
        super(false, null, null, null, null);
    }

    public SchedulerEndPointInfo(EndpointInfo info) {
        this();
        provides = info.isProvides();
        endPtName = info.getEndpointName();
        interfaceName = info.getInterfaceName();
        serviceName = info.getServiceName();
        linkType = info.getLinkType();
    }

    void setProvides(boolean provides) {
        this.provides = provides;
    }

    @Override
    public boolean isProvides() {
        return provides;
    }

    void setEndpointName(String endPtName) {
        this.endPtName = endPtName;
    }

    @Override
    public String getEndpointName() {
        return endPtName;
    }

    void setInterfaceName(QName interfaceName) {
        this.interfaceName = interfaceName;
    }

    @Override
    public QName getInterfaceName() {
        return interfaceName;
    }

    void setServiceName(QName serviceName) {
        this.serviceName = serviceName;
    }

    @Override
    public QName getServiceName() {
        return serviceName;
    }

    void setLinkType(LinkType linkType) {
        this.linkType = linkType;
    }

    @Override
    public LinkType getLinkType() {
        return linkType;
    }

    public void readObject(String data) throws StringableException {
        if ((null == data) || (data.trim().length() == 0)
                || !data.startsWith(CLASSID)) {
            throw new StringableException("Invalid data!");             //NOI18N
        }
        String[] fields = readFields(data);
        setProvides(Boolean.parseBoolean(fields[0]));
        setEndpointName(fields[1]);
        setInterfaceName((fields[2] != null) ? QName.valueOf(fields[2]) : null);
        setServiceName((fields[3] != null) ? QName.valueOf(fields[3]) : null);
        setLinkType((fields[4] != null) ? LinkType.valueOf(fields[4]) : null);
    }

    public void writeObject(StringBuilder data) throws StringableException {
        data.append(CLASSID);
        writeField(data, isProvides());
        writeField(data, getEndpointName());
        writeField(data, (QName) getInterfaceName());
        writeField(data, (QName) getServiceName());
        writeField(data, (LinkType) getLinkType());
    }

    private String[] readFields(String data) {
        int offset = CLASSID.length();
        String[] fields = new String[5];
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

    private StringBuilder writeField(StringBuilder data, QName field) {
        String fieldStr = (field != null) ? field.toString() : null;
        return data.append((fieldStr != null) ? fieldStr.length() : 0)
                .append(DELIM)
                .append((fieldStr != null) ? fieldStr : "");            //NOI18N
    }

    private StringBuilder writeField(StringBuilder data, boolean field) {
        String bStr = Boolean.toString(field);
        return data.append(bStr.length()).append(DELIM).append(bStr);
    }

    private StringBuilder writeField(StringBuilder data, LinkType field) {
        String fieldStr = (field != null) ? field.name() : null;
        return data.append((fieldStr != null) ? fieldStr.length() : 0)
                .append(DELIM)
                .append((fieldStr != null) ? fieldStr : "");            //NOI18N
    }

    public static SchedulerEndPointInfo valueOf(String data)
            throws StringableException {
        if (null == data) {
            return null;
        }
        SchedulerEndPointInfo value = new SchedulerEndPointInfo();
        value.readObject(data);
        return value;
    }

    public String valueAsString() throws StringableException {
        StringBuilder sb = new StringBuilder();
        writeObject(sb);
        return sb.toString();
    }
}
