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
 * @(#)SchedulerEndPointInfoTest.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.schedulerbc.domain;

import com.sun.jbi.common.descriptor.EndpointInfo.LinkType;
import javax.xml.namespace.QName;
import junit.framework.TestCase;

/**
 * Unit test for SchedulerEndPointInfo.
 * 
 * @author sunsoabi_edwong
 */
public class SchedulerEndPointInfoTest extends TestCase {

    private static final String ENDPOINTNAME_STR = "EndpointName";      //NOI18N
    private static final QName INTERFACENAME_QNAME =
            new QName("Interface", "Name");                             //NOI18N
    private static final QName SERVICENAME_QNAME =
            new QName("Service", "Name");                               //NOI18N
    
    public SchedulerEndPointInfoTest(String testName) {
        super(testName);
    }

//    public void testIsProvides() {
//    }
//
//    public void testGetEndpointName() {
//    }
//
//    public void testGetInterfaceName() {
//    }
//
//    public void testGetServiceName() {
//    }
//
//    public void testGetLinkType() {
//    }

    private void setField(StringBuilder sb, String field) {
        sb.append((field != null) ? field.length() : 0).append(Stringable.DELIM)
                .append((field != null) ? field : "");                  //NOI18N
    }

    private void setField(StringBuilder sb, QName field) {
        String fieldStr = (field != null) ? field.toString() : null;
        sb.append((fieldStr != null) ? fieldStr.length() : 0)
                .append(Stringable.DELIM)
                .append((fieldStr != null) ? fieldStr : "");            //NOI18N
    }

    private void setField(StringBuilder sb, LinkType field) {
        String fieldStr = (field != null) ? field.name() : null;
        sb.append((fieldStr != null) ? fieldStr.length() : 0)
                .append(Stringable.DELIM)
                .append((fieldStr != null) ? fieldStr : "");            //NOI18N
    }

    private void setField(StringBuilder sb, boolean field) {
        sb.append(Boolean.toString(field).length()).append(Stringable.DELIM)
                .append(field);
    }

    public void testReadObject() {
        SchedulerEndPointInfo info = new SchedulerEndPointInfo();
        StringBuilder data = new StringBuilder();
        data.append(SchedulerEndPointInfo.CLASSID);
        setField(data, true);
        setField(data, ENDPOINTNAME_STR);
        setField(data, INTERFACENAME_QNAME);
        setField(data, SERVICENAME_QNAME);
        setField(data, LinkType.hard);
        info.readObject(data.toString());
        assertTrue(info.isProvides());
        assertEquals(ENDPOINTNAME_STR, info.getEndpointName());
        assertEquals(INTERFACENAME_QNAME, info.getInterfaceName());
        assertEquals(SERVICENAME_QNAME, info.getServiceName());
        assertEquals(LinkType.hard, info.getLinkType());
        
        data = new StringBuilder();
        data.append(SchedulerEndPointInfo.CLASSID);
        setField(data, false);
        setField(data, (String) null);
        setField(data, (QName) null);
        setField(data, (QName) null);
        setField(data, (LinkType) null);
        info.readObject(data.toString());
        assertTrue(!info.isProvides());
        assertEquals(null, info.getEndpointName());
        assertEquals(null, info.getInterfaceName());
        assertEquals(null, info.getServiceName());
        assertEquals(null, info.getLinkType());
    }

    public void testWriteObject() {
        SchedulerEndPointInfo info = new SchedulerEndPointInfo();
        StringBuilder expected = new StringBuilder();
        expected.append(SchedulerEndPointInfo.CLASSID);
        setField(expected, false);
        setField(expected, (String) null);
        setField(expected, (QName) null);
        setField(expected, (QName) null);
        setField(expected, (LinkType) null);
        StringBuilder data = new StringBuilder();
        info.writeObject(data);
        assertEquals(expected.toString(), data.toString());

        expected = new StringBuilder();
        expected.append(SchedulerEndPointInfo.CLASSID);
        setField(expected, true);
        setField(expected, ENDPOINTNAME_STR);
        setField(expected, INTERFACENAME_QNAME);
        setField(expected, SERVICENAME_QNAME);
        setField(expected, LinkType.hard);
        data = new StringBuilder();
        info.setProvides(true);
        info.setEndpointName(ENDPOINTNAME_STR);
        info.setInterfaceName(INTERFACENAME_QNAME);
        info.setServiceName(SERVICENAME_QNAME);
        info.setLinkType(LinkType.hard);
        info.writeObject(data);
        assertEquals(expected.toString(), data.toString());
    }

}
