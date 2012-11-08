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

import com.sun.grizzly.http.SelectorThread;

import java.util.logging.Logger;


/**
 * Embedded HTTP Server Controller to support HTTP GET Requests.
 * The RSS BC should only use one port for all local feeds. Therefore
 * once the first port has been chosen no other ports are allowed.
 */
public class EmbeddedServerController {
    private static final Logger log = Logger.getLogger(EmbeddedServerController.class.getName());
    private static int port = 0;
    private static SelectorThread selectorThread;
    private static RSSManager rssManager;

    private EmbeddedServerController() {
    }

    /**
     * Start selector thread in a new Thread because startEndpoint() blocks.
     *
     * @param suggestedPort
     */
    public static void startServer(int suggestedPort) {
        try {
            if (port == 0) {
                port = suggestedPort;
                log.fine("Starting server on port " + port);
                selectorThread.setPort(port);
                selectorThread.setAdapter(new GrizzlyEmbeddedAdapter(rssManager));
                selectorThread.initEndpoint();

                Runnable run = new Runnable() {
                        public void run() {
                            try {
                                selectorThread.startEndpoint();
                            } catch (Throwable t) {
                                log.severe("Error starting endpoint " +
                                    "for server " + t);
                            }
                        }
                    };

                Thread t = new Thread(run);
                t.setDaemon(true);
                t.start();
            } else if (port != suggestedPort) {
                log.warning("Can not use more than one port. Please use the " +
                    " existing port " + port + " in your location");
            }
        } catch (Exception e) {
            log.severe("Error starting server " + e);
            e.printStackTrace();
        }
    }

    /**
     * Stops the Server.
     */
    public static void stopServer() {
        if ((selectorThread != null) && selectorThread.isRunning()) {
            log.fine("Stoping server...");
            port = 0;
            selectorThread.stopEndpoint();
        }
    }

    public static int getPort() {
        return port;
    }

    public static SelectorThread getSelectorThread() {
        return selectorThread;
    }

    public static void setSelectorThread(SelectorThread selectorThread) {
        EmbeddedServerController.selectorThread = selectorThread;
    }

    public static RSSManager getRssManager() {
        return rssManager;
    }

    public static void setRssManager(RSSManager rssManager) {
        EmbeddedServerController.rssManager = rssManager;
    }
}
