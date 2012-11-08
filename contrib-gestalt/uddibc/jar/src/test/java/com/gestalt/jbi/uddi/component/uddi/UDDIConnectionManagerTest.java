/**
 *   uddi-binding-component - UDDI Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
 *
 *   This library is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU Lesser General Public
 *   License version 2.1 as published by the Free Software Foundation.
 *
 *   This library is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *   Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
 */
package com.gestalt.jbi.uddi.component.uddi;

import com.gestalt.jbi.uddi.component.UDDIEndpoint;

import junit.framework.TestCase;

import org.jmock.Mockery;

import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.jmock.lib.legacy.ClassImposteriser;

import org.junit.Test;

import org.junit.runner.RunWith;

import java.util.concurrent.ConcurrentHashMap;

import javax.xml.namespace.QName;


/**
 * Created by IntelliJ IDEA.
 * User: panderson
 * Date: May 29, 2007
 * Time: 2:35:50 PM
 * To change this template use File | Settings | File Templates.
 */
@RunWith(JMock.class)
public class UDDIConnectionManagerTest extends TestCase {
    Mockery context = new JUnit4Mockery();
    private UDDIConnectionManager connMan = new UDDIConnectionManager();

    protected void setUp() throws Exception {
        super.setUp();
        context.setImposteriser(ClassImposteriser.INSTANCE);
    }

    @Test
    public void testCreateInquiryProxy() throws Exception {
        String inquiryURI = "http://juddi.com:8080/juddi/inquiry";
        UDDIEndpoint uddiEndpoint = new UDDIEndpoint(new QName("Test"), null,
                "TestEndpoint", null, null, null, null);

        ConcurrentHashMap<?, ?> map = connMan.getInquiryProxies();
        assertTrue("UDDI Proxy already exists and should NOT!",
            null == map.get(inquiryURI));

        connMan.createInquiryProxy(inquiryURI, uddiEndpoint);

        map = connMan.getInquiryProxies();
        assertTrue("UDDI Proxy was NOT created!", null != map.get(inquiryURI));
    }

    @Test
    public void testDestroyProxy() throws Exception {
        String inquiryURI = "http://juddi.com:8080/juddi/inquiry";
        UDDIEndpoint uddiEndpoint = new UDDIEndpoint(new QName("Test"), null,
                "TestEndpoint", null, null, null, null);

        connMan.createInquiryProxy(inquiryURI, uddiEndpoint);

        ConcurrentHashMap<?, ?> map = connMan.getInquiryProxies();
        assertTrue("UDDI Proxy does NOT exist prior to test!",
            null != map.get(inquiryURI));

        connMan.destroyProxy(inquiryURI, uddiEndpoint);

        map = connMan.getInquiryProxies();
        assertTrue("UDDI Proxy was NOT destroyed!", null == map.get(inquiryURI));
    }
}
