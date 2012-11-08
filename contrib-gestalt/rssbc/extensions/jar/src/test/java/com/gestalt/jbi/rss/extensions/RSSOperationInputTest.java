/**
 *   rss-binding-component-extensions - Extensions for the RSS Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com
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
package com.gestalt.jbi.rss.extensions;

import com.gestalt.jbi.rss.extensions.RSSOperationInput.FilterTypes;

import com.ibm.wsdl.Constants;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import javax.xml.namespace.QName;


/**
 * Author: cgallemore
 * Date: May 9, 2007
 */
public class RSSOperationInputTest extends TestCase {
    private static final int DEFAULT_POLLING_INTERVAL = 10;
    public static final String RSS_NS_URI = "http://schemas.sun.com/jbi/wsdl-extensions/rss/";
    private Boolean isRequired = false;
    private RSSOperationInput rssOperationInput;

    public RSSOperationInputTest(String whichTest) {
        super(whichTest);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new RSSOperationInputTest("testGetterAndSetter"));

        return suite;
    }

    public void setUp() {
        rssOperationInput = new RSSOperationInput();
    }

    /**
     * Test the Getter and Setter for Element Type
     */
    public void testGetterAndSetter() {
        String description = "This is a new feed";
        String title = "New Feed";
        String link = "http://www.cnn.com";
        int pollingInterval = DEFAULT_POLLING_INTERVAL;
        String filterByValue = "now";
        String longitude = "longitude";
        String latitude = "latitude";
        String destinationUrl = "http://localhost:8000/feed1";

        //Test Get and Set ElementType
        QName qName = new QName(RSS_NS_URI, Constants.ELEM_INPUT);
        assertNotNull("getElementType was null",
            rssOperationInput.getElementType());
        assertTrue("getElementType was incorrect",
            rssOperationInput.getElementType().equals(qName));

        //Test Get and Set Required
        rssOperationInput.setRequired(isRequired);
        assertFalse("getRequired returned true", rssOperationInput.getRequired());
        isRequired = true;
        rssOperationInput.setRequired(isRequired);
        assertTrue("getRequired returned false", rssOperationInput.getRequired());

        rssOperationInput.setEntryDescription(description);
        assertEquals("getDescription not correct", description,
            rssOperationInput.getEntryDescription());

        rssOperationInput.setFilterByType(FilterTypes.publishDate);
        assertEquals("getFilterType was incorrect", FilterTypes.publishDate,
            rssOperationInput.getFilterByType());

        rssOperationInput.setFilterByValue(filterByValue);
        assertEquals("getFilterValue was incorrect", filterByValue,
            rssOperationInput.getFilterByValue());

        rssOperationInput.setEntryLink(link);
        assertEquals("getLink was incorrect", link,
            rssOperationInput.getEntryLink().toString());

        rssOperationInput.setPollingInterval(pollingInterval);
        assertEquals("getPollingInterval was incorrect", pollingInterval,
            rssOperationInput.getPollingInterval());

        rssOperationInput.setEntryTitle(title);
        assertEquals("getTitle was incorrect", title,
            rssOperationInput.getEntryTitle());

        rssOperationInput.setLongitude(longitude);
        assertEquals("setLongitude was incorrect", longitude,
            rssOperationInput.getLongitude());

        rssOperationInput.setLatitude(latitude);
        assertEquals("setLatitude was incorrect", latitude,
            rssOperationInput.getLatitude());

        rssOperationInput.setDestinationUrl(destinationUrl);
        assertEquals("destinationUrl was incorrect", destinationUrl,
                rssOperationInput.getDestinationUrl());
    }
}
