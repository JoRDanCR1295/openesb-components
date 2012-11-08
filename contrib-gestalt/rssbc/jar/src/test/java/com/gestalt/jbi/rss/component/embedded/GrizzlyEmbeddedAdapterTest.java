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
package com.gestalt.jbi.rss.component.embedded;

import com.gestalt.jbi.rss.component.rss.RSSManager;

import com.sun.grizzly.tcp.OutputBuffer;
import com.sun.grizzly.tcp.Request;
import com.sun.grizzly.tcp.Response;
import com.sun.grizzly.util.buf.ByteChunk;

import com.sun.syndication.feed.module.georss.GeoRSSModule;
import com.sun.syndication.feed.module.georss.W3CGeoModuleImpl;
import com.sun.syndication.feed.module.georss.geometries.Position;
import com.sun.syndication.feed.synd.*;
import com.sun.syndication.io.SyndFeedOutput;

import junit.framework.TestCase;

import org.jmock.Expectations;
import org.jmock.Mockery;

import org.jmock.integration.junit4.JUnit4Mockery;

import org.jmock.lib.legacy.ClassImposteriser;

import java.net.HttpURLConnection;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;


public class GrizzlyEmbeddedAdapterTest extends TestCase {
    private Mockery context = new JUnit4Mockery();

    public void testBadMethod() throws Exception {
        Request req = new Request();
        req.requestURI().setString("/jlorenzen/feed");
        req.method().setString("POST");

        Response resp = new Response();

        GrizzlyEmbeddedAdapter adapter = new GrizzlyEmbeddedAdapter(null);
        adapter.service(req, resp);

        assertEquals(HttpURLConnection.HTTP_BAD_METHOD, resp.getStatus());
    }

    public void testNotFound() throws Exception {
        Request req = new Request();
        req.requestURI().setString("/jlorenzen/feed");
        req.method().setString("GET");

        Response resp = new Response();

        GrizzlyEmbeddedAdapter adapter = new GrizzlyEmbeddedAdapter(new RSSManager());
        adapter.service(req, resp);

        assertEquals(HttpURLConnection.HTTP_NOT_FOUND, resp.getStatus());
    }

    public void testGetLocalFeed() throws Exception {
        context.setImposteriser(ClassImposteriser.INSTANCE);

        final RSSManager rssManager = context.mock(RSSManager.class);
        final SyndFeed feed = createFeed();

        context.checking(new Expectations() {

                {
                    allowing(rssManager).getLocalFeed(with(any(String.class)));
                    will(returnValue(feed));
                }
            });

        Request req = new Request();
        req.requestURI().setString("/jlorenzen/feed");
        req.method().setString("GET");

        Response resp = new Response();
        BasicOutputBuffer outputBuffer = new BasicOutputBuffer();
        resp.setOutputBuffer(outputBuffer);

        GrizzlyEmbeddedAdapter adapter = new GrizzlyEmbeddedAdapter(rssManager);
        adapter.service(req, resp);

        assertEquals(HttpURLConnection.HTTP_OK, resp.getStatus());
        assertEquals("text/xml", resp.getContentType());

        SyndFeedOutput output = new SyndFeedOutput();
        byte[] bytes = output.outputString(feed).getBytes();
        assertEquals(bytes.length, resp.getContentLength());

        ByteChunk chunk = outputBuffer.getByteChunk();
        assertEquals(new String(bytes), new String(chunk.getBytes()));
    }

    private SyndFeed createFeed() {
        SyndFeed feed = new SyndFeedImpl();
        feed.setTitle("RSS Blog");
        feed.setLink("http://localhost:8000/rss/feed");
        feed.setDescription("RSS Blogs are cool");
        feed.setFeedType("rss_2.0");

        SyndEntry entry1 = new SyndEntryImpl();
        entry1.setTitle("Entry 1");
        entry1.setLink("http://localhost:8000/rss/feed/entry1");
        entry1.setPublishedDate(new Date());

        SyndContent description = new SyndContentImpl();
        description.setType("text/plain");
        description.setValue("First Entry");
        entry1.setDescription(description);

        GeoRSSModule geoRSSModule = new W3CGeoModuleImpl();
        geoRSSModule.setPosition(new Position(Double.parseDouble("45.236"),
                Double.parseDouble("21.6547")));
        entry1.getModules().add(geoRSSModule);

        List<SyndEntry> list = new ArrayList<SyndEntry>();
        list.add(entry1);
        feed.setEntries(list);

        return feed;
    }

    public class BasicOutputBuffer implements OutputBuffer {
        private ByteChunk chunk;

        public int doWrite(ByteChunk byteChunk, Response response) {
            chunk = byteChunk;

            return 0;
        }

        public ByteChunk getByteChunk() {
            return chunk;
        }
    }
}
