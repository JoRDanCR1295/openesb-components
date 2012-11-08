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
import com.gestalt.jbi.rss.extensions.RSSOperationInput;

import java.net.URL;

import java.util.ArrayList;
import java.util.List;


public class RSSSubscriber {
    private RSSOperationInput rssOperationInput;
    private RSSConsumerHandler consumerHandler = null;
    private List<RSSConsumerPoller> consumerPollers;
    private List<URL> urls;
    private boolean receivingNotifications = false;
    private boolean subscribingToFeeds = false;

    public RSSSubscriber() {
        urls = new ArrayList<URL>();
        consumerPollers = new ArrayList<RSSConsumerPoller>();
    }

    public void setConsumerHandler(RSSConsumerHandler consumerHandler) {
        this.consumerHandler = consumerHandler;
    }

    public RSSConsumerHandler getConsumerHandler() {
        return this.consumerHandler;
    }

    public void setReceivingNotifications(boolean b) {
        this.receivingNotifications = b;
    }

    public boolean isReceivingNotifications() {
        return this.receivingNotifications;
    }

    public boolean isSubscribingToFeeds() {
        return subscribingToFeeds;
    }

    public void setSubscribingToFeeds(boolean subscribingToFeeds) {
        this.subscribingToFeeds = subscribingToFeeds;
    }

    public List<URL> getURLs() {
        return this.urls;
    }

    public void setURLs(List<URL> urls) {
        this.urls = urls;
    }

    public List<RSSConsumerPoller> getConsumerPollers() {
        return this.consumerPollers;
    }

    public void setConsumerPollers(List<RSSConsumerPoller> pollers) {
        this.consumerPollers = pollers;
    }

    public RSSOperationInput getRssOperationInput() {
        return rssOperationInput;
    }

    public void setRssOperationInput(RSSOperationInput rssOperationInput) {
        this.rssOperationInput = rssOperationInput;
    }
}
