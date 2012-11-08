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

import com.sun.grizzly.tcp.Adapter;
import com.sun.grizzly.tcp.OutputBuffer;
import com.sun.grizzly.tcp.Request;
import com.sun.grizzly.tcp.Response;
import com.sun.grizzly.util.buf.ByteChunk;

import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.io.SyndFeedOutput;

import java.net.HttpURLConnection;

import java.util.logging.Level;
import java.util.logging.Logger;


/**
 * Type of Grizzly Adapter who services incoming requests by being
 * notified through the service(Request request, Response response) method.
 * Uses an RSSManager to retreive the local feed entries.
 * Since RSS requests are HTTP GETs, the service method will only process
 * GET requests.
 */
public class GrizzlyEmbeddedAdapter implements Adapter {
    private static final Logger log = Logger.getLogger(GrizzlyEmbeddedAdapter.class.getName());
    private RSSManager rssManager;

    /**
     * Constructor
     *
     * @param rssManager
     */
    public GrizzlyEmbeddedAdapter(RSSManager rssManager) {
        this.rssManager = rssManager;
    }

    /**
     * Services the incoming request, only processes GET requests.
     * These requests should be from RSS Readers (i.e. RSSOwl)
     *
     * @param request
     * @param response
     * @throws Exception
     */
    public void service(final Request request, final Response response)
        throws Exception {
        String requestURI = request.requestURI().toString();

        if (log.isLoggable(Level.FINE)) {
            log.fine("New incoming request with URI: " + requestURI);
            log.fine("Request Method is: " + request.method());
        }

        if (!request.method().toString().equalsIgnoreCase("GET")) {
            response.setStatus(HttpURLConnection.HTTP_BAD_METHOD);

            return;
        }

        if (rssManager.getLocalFeed(requestURI) != null) {
            response.setStatus(HttpURLConnection.HTTP_OK);

            SyndFeed feed = rssManager.getLocalFeed(requestURI);
            SyndFeedOutput output = new SyndFeedOutput();
            byte[] bytes = output.outputString(feed).getBytes();

            ByteChunk chunk = new ByteChunk();
            response.setContentLength(bytes.length);
            response.setContentType("text/xml");
            chunk.append(bytes, 0, bytes.length);

            OutputBuffer buffer = response.getOutputBuffer();
            buffer.doWrite(chunk, response);
            response.finish();
        } else {
            response.setStatus(HttpURLConnection.HTTP_NOT_FOUND);
        }
    }

    public void afterService(Request request, Response response)
        throws Exception {
        request.recycle();
        response.recycle();
    }

    public void fireAdapterEvent(String string, Object object) {
    }
}
