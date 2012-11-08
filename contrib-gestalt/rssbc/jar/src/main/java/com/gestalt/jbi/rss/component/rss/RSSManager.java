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

import com.gestalt.jbi.rss.component.RSSConsumerHandler;
import com.gestalt.jbi.rss.component.rss.persistence.HibernateUtil;
import com.gestalt.jbi.rss.component.rss.persistence.SyndEntryPersistenceImpl;
import com.gestalt.jbi.rss.extensions.RSSAddress;
import com.gestalt.jbi.rss.extensions.RSSOperationInput;

import com.sun.syndication.feed.module.georss.GeoRSSModule;
import com.sun.syndication.feed.module.georss.W3CGeoModuleImpl;
import com.sun.syndication.feed.module.georss.geometries.Position;
import com.sun.syndication.feed.synd.*;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.XmlReader;

import org.hibernate.Session;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;


/**
 * Provide the RSS Utilites required to perform the operations getExternalFeed
 * and publish
 */
public class RSSManager {
    private static final Logger log = Logger.getLogger(RSSManager.class.getName());
    private static Boolean persist = false;
    private ScheduledThreadPoolExecutor scheduler = new ScheduledThreadPoolExecutor(1);
    private Map<String, SyndFeed> feeds = new ConcurrentHashMap<String, SyndFeed>();
    private Map<String, RSSSubscriber> subscribers = new ConcurrentHashMap<String, RSSSubscriber>();
    private final String FROM_QUERY = "from SyndFeedImpl";

    public RSSManager() {
        if (persist) {
            log.info("LOADING ENTRIES");
            loadPersistedEntities();
        }
    }

    /**
     * Publish a new entry to a feed that already exists. If the feed does not
     * exist create one first.
     *
     * @param title            -
     *                         The title of the entry
     * @param link             -
     *                         The link to the entry
     * @param descriptionValue -
     *                         The description of the entry.
     * @param destinationUrl   -
     *                         The destination url of the feed for this specific entry.  This may be null or empty,
     *                         in which case the original feed specified in the rssAddress will be used.
     */
    public void publish(String title, String link, String descriptionValue,
        String longitude, String latitude, RSSAddress rssAddress,
        String destinationUrl) {
        SyndFeed feed;
        String path;

        try {
            if ((destinationUrl != null) && (destinationUrl != "")) {
                path = getURI(destinationUrl);
            } else {
                path = getURI(rssAddress.getLocation());
            }

            log.fine("Publish: " + path);

            if (feeds.containsKey(path)) {
                feed = feeds.get(path);
            } else {
                log.fine(
                    "Feed does not exist, creating new feed and publishing new entry.");
                feed = createFeed(rssAddress.getFeedTitle(), path,
                        rssAddress.getFeedDescription(),
                        rssAddress.getFeedType());
            }

            SyndEntry entry = processPublish(title, link, descriptionValue,
                    longitude, latitude, path);

            //be sure to persist in reverse orders 
            // so parent has knowledge of children
            if (persist) {
                if ((null != feed) && (null != entry)) {
                    persistEntity(entry);
                    persistEntity(feed);
                }
            }

            //END PERSIST LOGIC
        } catch (URISyntaxException e) {
            log.severe("Error publishing Feed: " + e);
        }
    }

    /**
     * Processes that actual writing of the feed to the data store.
     * <p/>
     * Note the use of custom extended class SyndEntryPersistenceImpl
     * This is used to give hibernate access to georss module
     *
     * @param title
     * @param link
     * @param descriptionValue
     */
    private SyndEntry processPublish(String title, String link,
        String descriptionValue, String longitude, String latitude,
        String location) throws URISyntaxException {
        log.fine("processPublish, description is: " + descriptionValue);

        SyndEntry entry = null;
        String path = getURI(location);
        SyndFeed feed = feeds.get(path);

        if (feed != null) {
            entry = new SyndEntryPersistenceImpl();
            entry.setTitle(title);
            entry.setLink(link);
            entry.setPublishedDate(new Date());

            SyndContent description = new SyndContentImpl();
            description.setType("text/plain");
            description.setValue(descriptionValue);
            entry.setDescription(description);

            if ((longitude != null) && (latitude != null)) {
                log.fine("Longitude is " + longitude + " Latitude is " +
                    latitude);

                GeoRSSModule geoRSSModule = new W3CGeoModuleImpl();
                geoRSSModule.setPosition(new Position(Double.parseDouble(
                            latitude), Double.parseDouble(longitude)));
                entry.getModules().add(geoRSSModule);
            }

            feed.getEntries().add(0, entry);
        }

        return entry;
    }

    /**
     * Creates a new RSS Feed
     *
     * @param feedTitle       -
     *                        The title of the feed
     * @param feedLink        -
     *                        The link to the feed
     * @param feedDescription -
     *                        The description of the feed
     * @param feedType        -
     *                        The type of feed (i.e. atom_0.3); This must be a valid atom or
     *                        rss type.
     */
    private SyndFeed createFeed(String feedTitle, String feedLink,
        String feedDescription, String feedType) throws URISyntaxException {
        SyndFeed feed = new SyndFeedImpl();
        feed.setTitle(feedTitle);
        feed.setLink(feedLink);
        feed.setDescription(feedDescription);
        feed.setFeedType(feedType);

        String path = getURI(feedLink);
        log.fine("Get Local feed: " + path);
        feeds.put(path, feed);

        return feed;
    }

    /**
     * Creates a subscriber entry for the given ID returns false if that ID is
     * already in use
     *
     * @param id
     * @return boolean
     */

    // @TODO - re-think method of making sure the same ID is not used twice as a
    // PROVIDER or CONSUMER
    public boolean addSubscriber(String id, MessageExchange.Role role) {
        if (!subscribers.containsKey(id)) {
            RSSSubscriber subscriber = new RSSSubscriber();
            subscribers.put(id, subscriber);

            if (role.equals(MessageExchange.Role.PROVIDER)) {
                subscriber.setSubscribingToFeeds(true);
            } else {
                subscriber.setReceivingNotifications(true);
            }

            return true;
        } else if (role.equals(MessageExchange.Role.PROVIDER)) {
            RSSSubscriber subscriber = subscribers.get(id);

            if (subscriber.isSubscribingToFeeds()) {
                return false;
            }

            subscriber.setSubscribingToFeeds(true);

            return true;
        } else if (role.equals(MessageExchange.Role.CONSUMER)) {
            RSSSubscriber subscriber = subscribers.get(id);

            if (subscriber.isReceivingNotifications()) {
                return false;
            }

            subscriber.setReceivingNotifications(true);

            return true;
        }

        return false;
    }

    public RSSSubscriber removeSubscriber(String id) {
        return subscribers.remove(id);
    }

    /**
     * Subscribe to the list of RSS URIs provided in 'feedList' parameter on
     * behalf of the entity represented by 'rssAddress.getCorrelationId()'
     *
     * @param feedList   -
     *                   XML string containing list of RSS URIs
     * @param rssAddress
     */
    public void subscribe(List<URL> feedList, RSSAddress rssAddress) {
        String id = rssAddress.getCorrelationId();
        RSSSubscriber subscriber = subscribers.get(id);
        RSSOperationInput input = subscriber.getRssOperationInput();

        if (!subscriber.isReceivingNotifications()) {
            subscriber.setURLs(feedList);
            log.fine("Not yet receiving notifications. Returning.");

            return;
        }

        List<RSSConsumerPoller> existingPollerss = subscriber.getConsumerPollers();

        if (!existingPollerss.isEmpty()) {
            for (RSSConsumerPoller poller : existingPollerss) {
                scheduler.remove(poller);
            }
        }

        subscriber.setURLs(feedList);

        List<RSSConsumerPoller> pollers = new ArrayList<RSSConsumerPoller>();
        log.fine("Subscribing to " + feedList.size() + " feeds");

        for (URL url : feedList) {
            RSSConsumerPoller consumerPoller = new RSSConsumerPoller(url,
                    input, subscriber.getConsumerHandler());
            pollers.add(consumerPoller);
            scheduleConsumerPoller(consumerPoller, input.getPollingInterval());
        }

        subscriber.setConsumerPollers(pollers);
    }

    public void receiveNotifications(RSSOperationInput input,
        RSSAddress rssAddress, RSSConsumerHandler consumerHandler) {
        String id = rssAddress.getCorrelationId();
        RSSSubscriber subscriber = subscribers.get(id);
        subscriber.setRssOperationInput(input);
        subscriber.setConsumerHandler(consumerHandler);

        if (!subscriber.isSubscribingToFeeds()) {
            return;
        }

        List<URL> urls = subscriber.getURLs();
        List<RSSConsumerPoller> pollers = new ArrayList<RSSConsumerPoller>();

        for (URL url : urls) {
            RSSConsumerPoller consumerPoller = new RSSConsumerPoller(url,
                    input, subscriber.getConsumerHandler());
            pollers.add(consumerPoller);
            scheduleConsumerPoller(consumerPoller, input.getPollingInterval());
        }

        subscriber.setConsumerPollers(pollers);
    }

    public void scheduleConsumerPoller(RSSConsumerPoller consumerPoller,
        int pollingInterval) {
        if (scheduler.isShutdown()) {
            log.fine(
                "Scheduling a consumer poller and scheduler is shutdown.  Starting a new scheduler.");
            scheduler = new ScheduledThreadPoolExecutor(1);
        }

        scheduler.scheduleWithFixedDelay(consumerPoller, 0, pollingInterval,
            TimeUnit.SECONDS);
    }

    public void shutdown() {
        scheduler.shutdown();
    }

    /**
     * Parses the /cgallemore/feed from http://localhost:8000/cgallemore/feed
     *
     * @param feedLink
     * @return String - Request path of the URI.
     */
    private String getURI(String feedLink) throws URISyntaxException {
        URI uri = new URI(feedLink);

        return uri.getPath();
    }

    /**
     * Looks up the external RSS feed specified by the feedUrlStr and returns a
     * List of that feeds entries.
     *
     * @return List of feed entries of type SyndEntry
     */
    @SuppressWarnings("unchecked")
    public static List<SyndEntry> getExternalFeed(SyndFeedInput input,
        XmlReader reader) {
        SyndFeed feed = null;

        try {
            feed = input.build(reader);
        } catch (Exception e) {
            log.warning(
                "Error getting External feed, returning Collections.EMPTY_LIST" +
                e);

            return (List<SyndEntry>) Collections.EMPTY_LIST;
        }

        if ((feed == null) || feed.getEntries().isEmpty()) {
            return (List<SyndEntry>) Collections.EMPTY_LIST;
        } else {
            return (List<SyndEntry>) feed.getEntries();
        }
    }

    /**
     * Returns the local feed created from publishing.
     *
     * @param path -
     *             Request URI from the feed URL (i.e. if the feed URL is
     *             http://localhost:8000/cgallemore/feed, then the path is
     *             /cgallemore/feed.
     * @return SyndFeed, returns null if no feed is found.
     */
    public SyndFeed getLocalFeed(String path) {
        log.fine("Get Local feed: " + path);

        return feeds.get(path);
    }

    /**
     * Persists given object with Hibernate
     *
     * @param entity object to persist
     */
    private synchronized void persistEntity(Object entity) {
        try {
            Session session = HibernateUtil.getSessionFactory()
                                           .getCurrentSession();
            session.beginTransaction();

            session.saveOrUpdate(entity);

            session.flush(); //redundant with commit?
            session.getTransaction().commit();
        } catch (Exception e) {
            log.severe(e.getMessage());
        }
    }

    private void loadPersistedEntities() {
        try {
            Session session = HibernateUtil.getSessionFactory()
                                           .getCurrentSession();
            session.beginTransaction();

            List<SyndFeed> feeds = session.createQuery(FROM_QUERY).list();

            for (SyndFeed f : feeds) {
                for (Object o : f.getEntries()) {
                    ((SyndEntry) o).getModules().remove(null);
                }

                this.feeds.put(getURI(f.getLink()), f);
            }
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }

    public static void setPersist(boolean persist) {
        RSSManager.persist = persist;
    }
}
