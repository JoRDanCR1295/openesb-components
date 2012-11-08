/**
 *   sip-binding-component - SIP Binding Component
 *
 *   Copyright (C) 2007 Gestalt, LLC. All Rights Reserved.
 *   http://www.gestalt-llc.com/
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
package com.gestalt.jbi.sip;

import com.sun.jbi.internationalization.Messages;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.logging.Logger;
import java.util.logging.Level;


public final class SIPObservable {
    private static final Logger log = Messages.getLogger(SIPObservable.class);
    private static Map<String, List<Observer>> observerMap = new HashMap<String, List<Observer>>();
    private static ExecutorService pool = Executors.newCachedThreadPool();

    private SIPObservable() {
    }

    public static synchronized void addObserver(String localUri, Observer o) {
        List<Observer> l = observerMap.get(localUri.toLowerCase());

        if (l == null) {
            l = new ArrayList<Observer>();

            l.add(o);
            observerMap.put(localUri.toLowerCase(), l);
        } else {
            l.add(o);
        }
    }

    public static synchronized void removeObserver(String localUri, Observer o) {
        List<Observer> l = observerMap.get(localUri.toLowerCase());

        if (l != null) {
            l.remove(o);

            if (l.isEmpty()) {
                removeObservers(localUri);
            }
        }
    }

    public static synchronized void removeObservers(String localUrl) {
        observerMap.remove(localUrl.toLowerCase());
    }

    public static void notifyObservers(final String localUri, final Object arg) {

        if (log.isLoggable(Level.FINEST)){
            log.log(Level.FINEST,"Received a request to notify the observers");
        }
        pool.execute(new ObserverNotifier(localUri,arg));
    }

    static class ObserverNotifier implements Runnable {
        private String localUri;
        private Object arg;

        ObserverNotifier(final String localUri, final Object arg) {
            this.localUri = localUri;
            this.arg = arg;
        }

        public void run() {
            List<Observer> l = observerMap.get(localUri.toLowerCase());
            if (l != null) {
                if (log.isLoggable(Level.FINEST)){
                    log.log(Level.FINEST,"Updating " + l.size() +
                        "observers for the LocalUri: " + localUri);
                }

                for (Observer o : l) {
                    o.update(arg);
                }
            }
        }
    }
}
