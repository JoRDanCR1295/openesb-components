/**
 *   uddi-binding-component-integration - UDDI Binding Component Integration
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
package com.gestalt.jbi.uddi;

import junit.framework.TestCase;
import junit.framework.Test;
import junit.framework.TestSuite;

import java.util.List;
import java.util.logging.Logger;

import com.gestalt.jbi.uddi.component.uddi.UDDIUtils;
import com.gestalt.jbi.uddi.component.uddi.UDDIConnectionManager;
import org.uddi4j.client.UDDIProxy;

/**
 * Author: cgallemore
 * Date: Apr 23, 2007
 */
public class UDDILookupTest extends TestCase {
	private Logger log = Logger.getLogger(UDDILookupTest.class.getName());
    private static final String UDDI_URL = "http://test.uddi.microsoft.com/inquire";
    private static final String JUDDI_URL = "http://10.9.5.171:8080/juddi/inquiry";
    private UDDIUtils uddiUtils = new UDDIUtils();

    public UDDILookupTest(String whichTest) {
        super(whichTest);
    }

    static public Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new UDDILookupTest("testServiceSearchMicrosoft"));
        suite.addTest(new UDDILookupTest("testServiceSearchJuddi"));
        return suite;
    }


    /**
     * Test the ability to return the endpoint URL given a service name
     *
     * online uddi browser:
     *  http://soapclient.com/uddi/uddi.sri?requestname=get_serviceDetail&serviceKey=6166f8b2-436d-4001-9f68-f37ff8b47ea3&operator=http://test.uddi.microsoft.com/inquire
     *
     * @throws Exception
     */
    public void testServiceSearchMicrosoft() throws Exception {
        UDDIConnectionManager manager = new UDDIConnectionManager();
        UDDIProxy proxy = manager.createInquiryProxy(UDDI_URL, null);

        List<String> urls = uddiUtils.lookupService("Certification", "Microsoft DRMS Dev", proxy);
        assertTrue("There should be endpoint url(s) returned", urls.size() > 0);
        assertEquals("Service url doesnt match", urls.toArray()[0], "https://wtest33.redmond.corp.microsoft.com/certification/certification.asmx");

        for (String url : urls) {
            log.fine("The service url is " + url);
        }
    }

    public void testServiceSearchJuddi() throws Exception {
        UDDIConnectionManager manager = new UDDIConnectionManager();
        UDDIProxy proxy = manager.createInquiryProxy(JUDDI_URL, null);

        List<String> urls = uddiUtils.lookupService("Test_Bob_Service", "Gestalt", proxy);
        assertTrue("There should be endpoint url(s) returned", urls.size() > 0);
        assertEquals("Service url doesnt match", urls.toArray()[0], "http://foo.com/test/BobService");

        for (String url : urls) {
            log.fine("The service url is " + url);
        }
    }
}
