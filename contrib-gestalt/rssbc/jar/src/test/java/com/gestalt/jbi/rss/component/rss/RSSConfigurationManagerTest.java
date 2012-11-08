/**
 *   rss-binding-component - RSS Binding Component
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
package com.gestalt.jbi.rss.component.rss;

import com.gestalt.jbi.rss.component.rss.persistence.ComponentType;
import com.gestalt.jbi.rss.component.rss.persistence.ConfigurationManager;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;

import org.xml.sax.InputSource;

import java.io.StringReader;

import java.util.HashMap;
import java.util.Map;

import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;


public class RSSConfigurationManagerTest extends TestCase {
    private final String PERSIST = "Persist";

    public void testParseConfigExtensions() throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        Document doc = builder.parse(new InputSource(
                    new StringReader(
                        "<?xml version=\"1.0\" encoding=\"UTF-8\"?><config:Configuration>" +
                        "            <config:Persist>true</config:Persist>" +
                        "        </config:Configuration>")));

        DocumentFragment frag = doc.createDocumentFragment();
        frag.appendChild(doc.getFirstChild());

        Map<String, String> map = new HashMap<String, String>();
        ConfigurationManager.parseConfigExtensions(map, frag);

        assertNotNull(map.get(PERSIST));
        assertTrue(Boolean.valueOf(map.get(PERSIST)));
    }

    public void testConstructObjectName() {
        try {
            String result = "com.sun.ebi:ServiceType=Configuration,InstallationType=bindingComponents,IdentificationName=componentName";
            ObjectName on = ConfigurationManager.constructObjectName(ComponentType.BC,
                    "componentName");
            assertEquals(on.toString(), result);
        } catch (MalformedObjectNameException e) {
            e.printStackTrace();
            assertTrue("Exception thrown: " + e.getMessage(), false);
        }
    }
}
