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
import com.gestalt.jbi.rss.extensions.RSSAddress;
import com.gestalt.jbi.rss.extensions.RSSOperationInput;
import com.gestalt.jbi.rss.extensions.RSSOperationInput.FilterTypes;

import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.io.SyndFeedInput;
import com.sun.syndication.io.XmlReader;

import org.apache.commons.codec.binary.Base64;

import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.URL;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.logging.Logger;


/**
 * Manages the thread for polling a RSS feed to detect changes. Based on the
 * Filter, returns a list of entries. Author: cgallemore Date: May 18, 2007
 */
public class RSSConsumerPoller implements Runnable {
    private static final String SOURCE_CLASS = RSSConsumerPoller.class.getName();
    private static final Logger log = Logger.getLogger(SOURCE_CLASS);
    private static final DateFormat DATE_PARSER = new SimpleDateFormat(
            "yyyy-MM-dd HH:mm");
    private XmlReader xmlReader;
    private RSSConsumerHandler rssConsumerHandler;
    private RSSOperationInput rssOperationInput;
    private String location;
    private FilterTypes filterType;
    private Date filterDate;
    private String username = null;
    private String password = null;

    /**
     * Constructor
     *
     * @param rssConsumerHandler -
     *                           The handler for this service.
     * @param rssOperationInput
     * @param rssAddress
     */
    public RSSConsumerPoller(RSSConsumerHandler rssConsumerHandler,
        RSSOperationInput rssOperationInput, RSSAddress rssAddress) {
        this.location = rssAddress.getLocation();
        this.rssConsumerHandler = rssConsumerHandler;
        this.rssOperationInput = rssOperationInput;
        this.filterType = rssOperationInput.getFilterByType();
        this.username = rssAddress.getUsername();
        this.password = rssAddress.getPassword();

        String filterValue = rssOperationInput.getFilterByValue();
        initFiltering(filterValue);
    }

    /**
     * @param url
     * @param rssOperationInput
     * @param consumerHandler
     */
    public RSSConsumerPoller(URL url, RSSOperationInput rssOperationInput,
        RSSConsumerHandler consumerHandler) {
        this.location = url.toString();
        this.rssConsumerHandler = consumerHandler;
        this.filterType = rssOperationInput.getFilterByType();

        String filterValue = rssOperationInput.getFilterByValue();
        initFiltering(filterValue);
    }

    private void initFiltering(String filterValue) {
        if (this.filterType.equals(FilterTypes.none)) {
            this.filterDate = null;
        } else {
            try {
                if (filterValue.toLowerCase().equals("now")) {
                    this.filterDate = new Date();
                } else {
                    this.filterDate = DATE_PARSER.parse(filterValue);
                }
            } catch (ParseException pe) {
                log.severe(
                    "Error parsing filterDate, setting filterDate to now.");
                this.filterDate = new Date();
            }
        }
    }

    /**
     * Perfroms the getExternalFeed of the RSS Feed. Returns a list of entries
     * from that getExternalFeed and based of the filterType will process the
     * list of entries.
     */
    public void run() {
        try {
            try {
                if (((username != null) || (username != "")) &&
                        ((password != null) && (password != ""))) {
                    log.fine(
                        "Found username and password, setting XmlReader with secure connection");

                    String user = username + ":" + password;
                    URL location = new URL(this.location);
                    HttpURLConnection secureConn = (HttpURLConnection) location.openConnection();
                    byte[] encodedBytes = new Base64().encode(user.getBytes());
                    String encoded = new String(encodedBytes);
                    secureConn.setRequestProperty("Authorization",
                        "Basic " + encoded);
                    this.xmlReader = new XmlReader(secureConn);
                } else {
                    log.fine("No username and password found");
                    this.xmlReader = new XmlReader(new URL(location));
                }
            } catch (ConnectException ce) {
                log.warning("Unable to connect to location: " + location);

                return;
            }

            List<SyndEntry> list = RSSManager.getExternalFeed(new SyndFeedInput(),
                    this.xmlReader);

            if (list != Collections.EMPTY_LIST) {
                log.fine("FilterType=" + filterType + "; FilterDate=" +
                    filterDate);

                boolean isUpdated = false;

                if (filterType.equals(FilterTypes.none)) {
                    // Check for any updates to the feed. If there are any
                    // we will process the entire list.
                    if (filterDate != null) {
                        isUpdated = checkForUpdates(list);
                    }

                    // filterDate will be null the first time through so the
                    // user
                    // will get all entries back, and then we set the filter
                    // date
                    // to either the published date or the updated date, which
                    // ever
                    // is greater. Every time thourgh we will check for udates
                    // and
                    // if the feed has been updated we will process the entire
                    // feed.
                    if ((filterDate == null) || isUpdated) {
                        rssConsumerHandler.process(list, rssOperationInput);
                    } else {
                        log.fine("No current Entries");
                    }

                    filterDate = getLastPublishDate(list);
                } else {
                    // Filters the list by the filterDate, and only
                    // returns new or modified entries.
                    list = filterList(list);

                    if (list != Collections.EMPTY_LIST) {
                        log.fine("Received a new entry");
                        rssConsumerHandler.process(list, rssOperationInput);
                    } else {
                        log.fine("No current entries");
                    }
                }
            }
        } catch (Throwable t) {
            log.severe("Error with getExternalFeed " + t);
            t.printStackTrace();
        }
    }

    /**
     * Checks for any update any the List of entries, and once we find one we
     * break and process the entrie list. This is used for the filterByType
     * "now".
     *
     * @param list
     */
    private boolean checkForUpdates(List<SyndEntry> list) {
        boolean updated = false;

        for (SyndEntry entry : list) {
            Date publishDate = entry.getPublishedDate();
            Date updatedDate = entry.getUpdatedDate();

            if ((null != publishDate) && publishDate.after(filterDate)) {
                updated = true;

                break;
            } else if ((null != updatedDate) && updatedDate.after(filterDate)) {
                updated = true;

                break;
            }
        }

        return updated;
    }

    /**
     * Filters the List of entries and only returns updated or new entries to
     * the feed.
     *
     * @param list -
     *             List<SyndEntry>
     * @return List
     */
    @SuppressWarnings("unchecked")
    private List<SyndEntry> filterList(List<SyndEntry> list) {
        boolean isChanged = false;
        Date tempFilterDate = null;
        List<SyndEntry> tempList = new ArrayList<SyndEntry>();

        for (SyndEntry entry : list) {
            Date publishedDate = entry.getPublishedDate();
            Date changedDate = entry.getUpdatedDate();
            log.fine("Comparing publishedDate: " + publishedDate +
                " with FilterDate: " + filterDate + " Changed Date is: " +
                changedDate);

            if ((null != publishedDate) && publishedDate.after(filterDate)) {
                isChanged = true;
                tempList.add(entry);
                if (tempFilterDate == null || publishedDate.after(tempFilterDate)) {
                    tempFilterDate = publishedDate;
                }
            } else if ((null != changedDate) && changedDate.after(filterDate)) {
                isChanged = true;
                tempList.add(entry);
                if (tempFilterDate == null || changedDate.after(tempFilterDate)) {
                    tempFilterDate = changedDate;
                }
            }
        }

        if (isChanged) {
            log.fine("Feed Change, Setting filter date to: " + tempFilterDate);
            filterDate = tempFilterDate;
        } else {
            log.fine("No change, useing last publishedDate");
            filterDate = getLastPublishDate(list);
        }

        if (tempList.isEmpty()) {
            return (List<SyndEntry>) Collections.EMPTY_LIST;
        } else {
            return tempList;
        }
    }

    private Date getLastPublishDate(List<SyndEntry> list) {
        if ((list == null) || (list.size() == 0)) {
            return filterDate;
        }

        // Initialize with first date, then figure out if there is a later one in the list
        Date lastDate = list.get(0).getPublishedDate();
        for (SyndEntry entry : list) {

            Date pDate = entry.getPublishedDate();
            Date cDate = entry.getUpdatedDate();

            Date entryDate = null;
            if ((cDate == null) || pDate.after(cDate)) {
                entryDate = pDate;
            } else {
                entryDate = cDate;
            }
            if (entryDate.after(lastDate)) {
                lastDate = entryDate;
            }
        }
        log.fine("Last date found: " + lastDate);
        return lastDate;
    }
}
