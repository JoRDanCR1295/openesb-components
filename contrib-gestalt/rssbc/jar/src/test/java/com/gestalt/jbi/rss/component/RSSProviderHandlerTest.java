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
package com.gestalt.jbi.rss.component;

import junit.framework.TestCase;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import java.net.URL;

import java.util.List;


/**
 * Created by IntelliJ IDEA.
 * User: csturtz
 * Date: Jun 25, 2007
 * Time: 11:57:25 AM
 * To change this template use File | Settings | File Templates.
 */
public class RSSProviderHandlerTest extends TestCase {
    private static final String JBI_RESOURCE_NO_NAMESPACE = "/jbipart.xml";
    private static final String JBI_RESOURCE_WITH_NAMESPACE = "/jbipartsWithNameSpace.xml";
    RSSProviderHandler providerHandler = new RSSProviderHandler(null);

    public void testParseFeedList() throws Exception {
        String feedListXML1 = "<EndpointReferenceList>" +
            "<EndpointReference>" + "<Address>" +
            "http://chadthedeveloper.blogspot.com/feeds/posts/default" +
            "</Address>" + "</EndpointReference>" + "<EndpointReference>" +
            "<Address>" + "http://gallemore.blogspot.com/feeds/posts/default" +
            "</Address>" + "</EndpointReference>" + "</EndpointReferenceList>";

        String feedListXML2 = "<EndpointReferenceList></EndpointReferenceList>";

        List<URL> urlList1 = providerHandler.parseFeedList(feedListXML1);
        assertEquals("List should have 2 URLs", urlList1.size(), 2);

        List<URL> urlList2 = providerHandler.parseFeedList(feedListXML2);
        assertEquals("List should have 0 URLs", urlList2.size(), 0);
    }

    public void testAddPartsXMLNoNameSpace() throws IOException {
        File jbiFile = getFile(JBI_RESOURCE_NO_NAMESPACE);
        BufferedReader reader = new BufferedReader(new FileReader(jbiFile));

        StringBuilder buffer = new StringBuilder();
        String temp = null;

        while ((temp = reader.readLine()) != null)
            buffer.append(temp);

        System.out.println("buffer = " + buffer.toString());

        List<String> list = providerHandler.addPartsXML(buffer.toString());
        assertTrue(list.size() > 0);

        for (String part : list) {
            System.out.println("Part is: " + part);
        }
    }

    public void testAddPartsXMLWithNameSpace() throws IOException {
        File jbiFile = getFile(JBI_RESOURCE_WITH_NAMESPACE);
        BufferedReader reader = new BufferedReader(new FileReader(jbiFile));

        StringBuilder buffer = new StringBuilder();
        String temp = null;

        while ((temp = reader.readLine()) != null)
            buffer.append(temp);

        System.out.println("buffer = " + buffer.toString());

        List<String> list = providerHandler.addPartsXML(buffer.toString());
        assertTrue(list.size() > 0);

        for (String part : list) {
            System.out.println("Part is: " + part);
        }
    }

    private File getFile(String fileName) {
        File jbiFile;
        URL url = this.getClass().getResource(fileName);
        assertNotNull(url);
        jbiFile = new File(url.getFile());
        assertNotNull(jbiFile);

        return jbiFile;
    }
}
