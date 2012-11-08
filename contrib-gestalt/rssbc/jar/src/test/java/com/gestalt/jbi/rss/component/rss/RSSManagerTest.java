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

import com.gestalt.jbi.rss.extensions.RSSAddress;

import com.sun.syndication.feed.module.georss.GeoRSSModule;
import com.sun.syndication.feed.module.georss.GeoRSSUtils;
import com.sun.syndication.feed.synd.*;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.SyndFeedOutput;
import com.sun.syndication.io.XmlReader;

import junit.framework.TestCase;

import org.jmock.Expectations;
import org.jmock.Mockery;

import org.jmock.integration.junit4.JUnit4Mockery;

import org.jmock.lib.legacy.ClassImposteriser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.jbi.messaging.MessageExchange;


public class RSSManagerTest extends TestCase {
    private static final String LONGITUDE = "11.23";
    private static final String LATITUDE = "25.6234";
    private Mockery context = new JUnit4Mockery();
    private RSSManager rssManager = new RSSManager();
    private String feedURL = "http://localhost:8000/rss/feed";
    private String geofeedURL = "http://localhost:8000/rss/geofeed";
    private String geoPath = "/rss/geofeed";
    private String path = "/rss/feed";
    private Date newDate = new Date();

    /**
     * Tests that the publish adds the Entries in the correct order
     *
     * @throws Exception
     */
    @SuppressWarnings("unchecked")
    public void testOrder() throws Exception {
        rssManager.setPersist(false);

        String feedTitle = "Rome Test Feed";
        String feedDescription = "This is a test feed";
        String entryTitle = "Testing feed entry";
        String entryLink = "http://localhost:8000/rss/feed/test";
        String entryDescription = "Testing the rome publish method";
        String feedType = "atom_0.3";

        RSSAddress rssAddress = new RSSAddress();
        rssAddress.setFeedTitle(feedTitle);
        rssAddress.setFeedType(feedType);
        rssAddress.setLocation(feedURL);
        rssAddress.setFeedDescription(feedDescription);

        assertNull("getLocalFeed was not null", rssManager.getLocalFeed(path));

        rssManager.publish(entryTitle, entryLink, entryDescription, null, null,
            rssAddress, "");
        rssManager.publish(entryTitle + "1", entryLink, entryDescription + "1",
            null, null, rssAddress, "");

        assertEquals("Feed should be size of 2", 2,
            rssManager.getLocalFeed(path).getEntries().size());

        SyndFeed feed = rssManager.getLocalFeed(path);
        List<SyndEntry> list = (List<SyndEntry>) feed.getEntries();
        SyndEntry entry = (SyndEntry) list.get(0);

        assertEquals("Incorrect order", entryTitle + "1", entry.getTitle());
    }

    public void testPublish() throws Exception {
        RSSManager rssManager = new RSSManager();
        rssManager.setPersist(false);

        String feedTitle = "Rome Test Feed";
        String feedDescription = "This is a test feed";
        String entryTitle = "Testing feed entry";
        String entryLink = "http://localhost:8000/rss/feed/test";
        String entryDescription = "Testing the rome publish method";
        String feedType = "atom_0.3";
        String destinationUrl = "http://localhost:8000/rss/feed2";
        String dynamicPath = "/rss/feed2";

        RSSAddress rssAddress = new RSSAddress();
        rssAddress.setFeedTitle(feedTitle);
        rssAddress.setFeedType(feedType);
        rssAddress.setLocation(feedURL);
        rssAddress.setFeedDescription(feedDescription);

        assertNull("getLocalFeed was not null", rssManager.getLocalFeed(path));
        rssManager.publish(entryTitle, entryLink, entryDescription, null, null,
            rssAddress, "");
        assertNotNull("getLocalFeed was null", rssManager.getLocalFeed(path));
        assertEquals("No entries were found", 1,
            rssManager.getLocalFeed(path).getEntries().size());

        /* Test dynamic location */
        assertNull("getLocalFeed was not null",
            rssManager.getLocalFeed(dynamicPath));
        rssManager.publish(entryTitle, entryLink, entryDescription, null, null,
            rssAddress, destinationUrl);
        assertNotNull("getLocalFeed was null",
            rssManager.getLocalFeed(dynamicPath));
        assertEquals("No entries were found", 1,
            rssManager.getLocalFeed(dynamicPath).getEntries().size());
    }

    public void testGeoPublish() throws Exception {
        rssManager.setPersist(false);

        String feedTitle = "Rome Test Feed";
        String feedDescription = "This is a test feed";
        String entryTitle = "Testing feed entry";
        String entryLink = "http://localhost:8000/rss/feed/test";
        String entryDescription = "Testing the rome publish method";
        String feedType = "rss_2.0";

        RSSAddress rssAddress = new RSSAddress();
        rssAddress.setFeedTitle(feedTitle);
        rssAddress.setFeedType(feedType);
        rssAddress.setLocation(feedURL);
        rssAddress.setFeedDescription(feedDescription);

        assertNull("getLocalFeed was not null", rssManager.getLocalFeed(path));
        rssManager.publish(entryTitle, entryLink, entryDescription, LONGITUDE,
            LATITUDE, rssAddress, "");
        assertNotNull("getLocalFeed was null", rssManager.getLocalFeed(path));
        assertEquals("No entries were found", 1,
            rssManager.getLocalFeed(path).getEntries().size());

        SyndFeed feed = rssManager.getLocalFeed(path);
        SyndEntry geoEntry = (SyndEntry) feed.getEntries().get(0);
        GeoRSSModule geoRSSModule = GeoRSSUtils.getGeoRSS(geoEntry);
        assertEquals("Longitude incorrect", Double.parseDouble(LONGITUDE),
            geoRSSModule.getPosition().getLongitude());
        assertEquals("Latitude incorrect", Double.parseDouble(LATITUDE),
            geoRSSModule.getPosition().getLatitude());

        String feedString = new SyndFeedOutput().outputString(feed);

        assertTrue("Longitude element missing",
            feedString.indexOf("<geo:long>11.23</geo:long>") > 0);
        assertTrue("Latitude element missing",
            feedString.indexOf("<geo:lat>25.6234</geo:lat>") > 0);
    }

    @SuppressWarnings("unchecked")
    public void testGetExternalFeedReturnsFeed() throws Exception {
        rssManager.setPersist(false);
        context.setImposteriser(ClassImposteriser.INSTANCE);

        final SyndFeedInput input = context.mock(SyndFeedInput.class);
        final XmlReader reader = context.mock(XmlReader.class);

        final SyndFeed feed = createFeed();
        List<SyndEntry> entries = feed.getEntries();
        assertEquals("list entry", "http://localhost:8000/rss/feed/entry1",
            ((SyndEntry) entries.get(0)).getLink());

        context.checking(new Expectations() {

                {
                    atLeast(1).of(input).build(reader);
                    will(returnValue(feed));
                }
            });

        //		List<SyndEntry> list = RSSManager.getExternalFeed(input, reader);
        assertEquals("", "http://localhost:8000/rss/feed/entry1",
            ((SyndEntry) entries.get(0)).getLink());
    }

    public void testGetExternalFeedReturnsNull() throws Exception {
        rssManager.setPersist(false);
        context.setImposteriser(ClassImposteriser.INSTANCE);

        final SyndFeedInput input = context.mock(SyndFeedInput.class);
        final XmlReader reader = context.mock(XmlReader.class);

        context.checking(new Expectations() {

                {
                    atLeast(1).of(input).build(reader);
                    will(returnValue(null));
                }
            });

        List<SyndEntry> list = RSSManager.getExternalFeed(input, reader);
        assertEquals("List was not equal to Collections.EmptyList", list,
            Collections.EMPTY_LIST);
    }

    public void testAddSubscriber() {
        rssManager.setPersist(false);

        String id1 = "id1";

        rssManager.addSubscriber(id1, MessageExchange.Role.PROVIDER);
        assertFalse("Adding this subscriber again should fail",
            rssManager.addSubscriber(id1, MessageExchange.Role.PROVIDER));
        assertTrue("Adding this subscriber again should not fail",
            rssManager.addSubscriber(id1, MessageExchange.Role.CONSUMER));
    }

    /**
     * Creates one feed with single entry.
     *
     * @return SyndFeed
     */
    private SyndFeed createFeed() {
        rssManager.setPersist(false);

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
        feed.setEntries(list);

        return feed;
    }

    public void testPersistence() {
        RSSManager manager = new RSSManager();
        RSSManager.setPersist(true);

        String feedTitle = "Rome Test Feed";
        String feedDescription = "This is a test feed";
        String entryTitle = "Testing feed entry";
        String entryLink = "http://localhost:8000/rss/feed/test";
        String entryDescription = "Testing the rome publish method";
        String feedType = "atom_0.3";

        RSSAddress rssAddress = new RSSAddress();
        rssAddress.setFeedTitle(feedTitle);
        rssAddress.setFeedType(feedType);
        rssAddress.setLocation(feedURL);
        rssAddress.setFeedDescription(feedDescription);

        manager.publish(entryTitle, entryLink, entryDescription, null, null,
            rssAddress, "");

        String feed2Title = "Rome Test Feed 2";
        String feed2Description = "This is a test feed 2";
        String entry2Title = "Testing feed entry 2";
        String entry2Link = "http://localhost:8000/rss/feed/test/2";
        String entry2Description = "Testing the rome publish method 2";
        String feed2Type = "atom_0.3";

        RSSAddress rss2Address = new RSSAddress();
        rss2Address.setFeedTitle(feed2Title);
        rss2Address.setFeedType(feed2Type);
        rss2Address.setLocation(feedURL);
        rss2Address.setFeedDescription(feed2Description);

        manager.publish(entry2Title, entry2Link, entry2Description, null, null,
            rss2Address, "");

        RSSManager rssManager = new RSSManager();
        assertNotNull("getLocalFeed was null", rssManager.getLocalFeed(path));
        assertEquals("No entries were found", 2,
            rssManager.getLocalFeed(path).getEntries().size());

        assertEquals("Entries are in wrong order",
            ((SyndEntry) manager.getLocalFeed(path).getEntries().get(1)).getLink(),
            ((SyndEntry) rssManager.getLocalFeed(path).getEntries().get(1)).getLink());

        //this throws runtime excepton if SyndEntry.getModules() contains a null entry
        rssManager.getLocalFeed(path).createWireFeed();
    }

    public void testGeoPersistence() throws Exception {
        RSSManager manager = new RSSManager();
        RSSManager.setPersist(true);

        String geofeedTitle = "Rome Test geo Feed";
        String geofeedDescription = "This is ageo  test feed";
        String geoentryTitle = "Testing geo feed entry";
        String geoentryLink = "http://localhost:8000/rss/geofeed/geotest";
        String geoentryDescription = "Testing the rome geo publish method";
        String geofeedType = "rss_2.0";

        RSSAddress georssAddress = new RSSAddress();
        georssAddress.setFeedTitle(geofeedTitle);
        georssAddress.setFeedType(geofeedType);
        georssAddress.setLocation(geofeedURL);
        georssAddress.setFeedDescription(geofeedDescription);

        manager.publish(geoentryTitle, geoentryLink, geoentryDescription,
            LONGITUDE, LATITUDE, georssAddress, "");

        assertNotNull("getLocalFeed was null", manager.getLocalFeed(geoPath));
        assertEquals("No entries were found", 1,
            manager.getLocalFeed(geoPath).getEntries().size());

        RSSManager rssManager = new RSSManager();

        SyndFeed feed = rssManager.getLocalFeed(geoPath);
        SyndEntry geoEntry = (SyndEntry) feed.getEntries().get(0);
        GeoRSSModule geoRSSModule = GeoRSSUtils.getGeoRSS(geoEntry);
        assertEquals("Longitude incorrect", Double.parseDouble(LONGITUDE),
            geoRSSModule.getPosition().getLongitude());
        assertEquals("Latitude incorrect", Double.parseDouble(LATITUDE),
            geoRSSModule.getPosition().getLatitude());

        String feedString = new SyndFeedOutput().outputString(feed);

        assertTrue("Longitude element missing",
            feedString.indexOf("<geo:long>11.23</geo:long>") > 0);
        assertTrue("Latitude element missing",
            feedString.indexOf("<geo:lat>25.6234</geo:lat>") > 0);
    }
}
