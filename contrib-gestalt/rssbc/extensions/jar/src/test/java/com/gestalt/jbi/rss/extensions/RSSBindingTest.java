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

import com.ibm.wsdl.Constants;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import javax.xml.namespace.QName;


/**
 * Author: cgallemore
 * Date: May 9, 2007
 */
public class RSSBindingTest extends TestCase {
    public static final String RSS_NS_URI = "http://schemas.sun.com/jbi/wsdl-extensions/rss/";
    private Boolean isRequired = false;
    private RSSBinding rssBinding;

    public RSSBindingTest(String whichTest) {
        super(whichTest);
    }

    public static Test suite() {
        TestSuite suite = new TestSuite();
        suite.addTest(new RSSBindingTest("testGetterAndSetter"));

        return suite;
    }

    public void setUp() {
        rssBinding = new RSSBinding();
    }

    /**
     * Test the Getter and Setter for Element Type
     */
    public void testGetterAndSetter() {
        //Test Get and Set ElementType
        QName qName = new QName(RSS_NS_URI, Constants.ELEM_BINDING);
        assertNotNull("getElementType was null", rssBinding.getElementType());
        assertTrue("getElementType was incorrect",
            rssBinding.getElementType().equals(qName));

        //Test Get and Set Required
        rssBinding.setRequired(isRequired);
        assertFalse("getRequired returned true", rssBinding.getRequired());
        isRequired = true;
        rssBinding.setRequired(isRequired);
        assertTrue("getRequired returned false", rssBinding.getRequired());
    }
}
