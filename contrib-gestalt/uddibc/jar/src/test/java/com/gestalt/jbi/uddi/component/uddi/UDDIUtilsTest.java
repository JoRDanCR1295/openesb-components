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

import junit.framework.TestCase;

import org.jmock.Mockery;

import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.jmock.lib.legacy.ClassImposteriser;

import org.junit.Test;

import org.junit.runner.RunWith;

import org.uddi4j.client.UDDIProxy;

import java.util.List;


/**
 * Created by IntelliJ IDEA.
 * User: panderson
 * Date: May 24, 2007
 * Time: 4:59:50 PM
 * To change this template use File | Settings | File Templates.
 */
@RunWith(JMock.class)
public class UDDIUtilsTest extends TestCase {
    Mockery context = new JUnit4Mockery();
    private UDDIUtils utils = new UDDIUtils();

    protected void setUp() throws Exception {
        super.setUp();
        context.setImposteriser(ClassImposteriser.INSTANCE);
    }

    @Test
    public void testGetBusinessKeys() throws Exception {
        UDDIProxy uddi = new MockUDDIProxy();
        List<String> businessKeys = utils.getBusinessKeys("Any Business", uddi);
        assertTrue("Business Keys return was NULL!", null != businessKeys);
        assertTrue("Size of returned Business Keys was ZERO!",
            0 < businessKeys.size());
    }

    @Test
    public void testLookupService() throws Exception {
        UDDIProxy uddi = new MockUDDIProxy();
        List<String> services = utils.lookupService("TestService",
                "TestBusiness", uddi);
        assertTrue("List of service was NULL!", null != services);
        assertTrue("Size of returned Services was ZERO!", 0 < services.size());
    }
}
