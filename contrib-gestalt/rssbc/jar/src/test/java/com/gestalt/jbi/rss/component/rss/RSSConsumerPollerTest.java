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

import com.gestalt.jbi.component.manager.Endpoint;
import com.gestalt.jbi.rss.component.RSSConsumerHandler;
import com.gestalt.jbi.rss.extensions.RSSAddress;
import com.gestalt.jbi.rss.extensions.RSSOperationInput;

import com.sun.syndication.feed.synd.*;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.XmlReader;

import junit.framework.TestCase;

import org.jmock.Expectations;
import org.jmock.Mockery;

import org.jmock.integration.junit4.JUnit4Mockery;

import org.jmock.lib.legacy.ClassImposteriser;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;


public class RSSConsumerPollerTest extends TestCase {
    private static Logger log = Logger.getLogger(RSSConsumerPollerTest.class.getName());
    Mockery context = new JUnit4Mockery();
    private Date newDate = new Date();

    public void testRun() {
        try {
            context.setImposteriser(ClassImposteriser.INSTANCE);

            final RSSManager manager = context.mock(RSSManager.class);
            final Endpoint endpoint = context.mock(Endpoint.class);
            final MockRSSConsumerHandler handler = new MockRSSConsumerHandler(endpoint);
            final XmlReader reader = context.mock(XmlReader.class);
            final SyndFeedInput input = context.mock(SyndFeedInput.class);
            final RSSOperationInput rssOperationInput = context.mock(RSSOperationInput.class);
            final RSSAddress rssAddress = context.mock(RSSAddress.class);

            context.checking(new Expectations() {

                    {
                        one(manager).getExternalFeed(input, reader);
                    }
                });

            RSSConsumerPoller poller = new RSSConsumerPoller(handler,
                    rssOperationInput, rssAddress);
            assertNotNull("Poller was null: ", poller);
            poller.run();
            assertEquals("Entries are not the same", handler.getEntries(),
                getEntries());
        } catch (Exception e) {
            log.severe(e.toString());
        }
    }

    /**
     * Creates one feed with single entry.
     *
     * @return SyndFeed
     */
    private List<SyndEntry> getEntries() {
        SyndFeed feed = new SyndFeedImpl();
        feed.setTitle("RSS Blog");
        feed.setLink("http://localhost:8000/rss/feed");
        feed.setDescription("RSS Blogs are cool");
        feed.setFeedType("rss_2.0");

        SyndEntry entry1 = new SyndEntryImpl();
        entry1.setTitle("Entry 1");
        entry1.setLink("http://localhost:8000/rss/feed/entry1");
        entry1.setPublishedDate(newDate);

        SyndContent description = new SyndContentImpl();
        description.setType("text/plain");
        description.setValue("First Entry");

        entry1.setDescription(description);

        SyndEntry entry2 = new SyndEntryImpl();
        entry2.setTitle("Entry 2");
        entry2.setLink("http://localhost:8000/rss/feed/entry2");
        entry2.setPublishedDate(newDate);

        SyndContent description2 = new SyndContentImpl();
        description2.setType("text/plain");
        description2.setValue("Second Entry");

        entry2.setDescription(description2);

        List<SyndEntry> list = new ArrayList<SyndEntry>();
        list.add(entry1);
        list.add(entry2);

        return list;
    }

    class MockRSSConsumerHandler extends RSSConsumerHandler {
        private List<SyndEntry> entries;

        public MockRSSConsumerHandler(Endpoint endpoint) {
            super(endpoint);
        }

        public void process(List<SyndEntry> list) {
            setEntries(getEntries());
        }

        public List<SyndEntry> getEntries() {
            return entries;
        }

        public void setEntries(List<SyndEntry> entries) {
            this.entries = entries;
        }
    }
}
