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

import junit.framework.TestCase;


public class SIPObservableTest extends TestCase {
    public static final int WAIT_TIME = 2000;

    public void testNotAllObserversNotified() throws InterruptedException {
        String uri1 = "urix";
        String uri2 = "uriy";

        SIPTestObserver observer1 = new SIPTestObserver();
        SIPTestObserver observer2 = new SIPTestObserver();

        SIPObservable.addObserver(uri1, observer1);
        SIPObservable.addObserver(uri2, observer2);

        SIPObservable.notifyObservers(uri1, "event1");
        Thread.sleep(WAIT_TIME);

        assertTrue("Observer1 did not see the event", observer1.getSawEvent());
        assertTrue("Observer2 saw the event", !observer2.getSawEvent());

        SIPObservable.removeObservers(uri1);
    }

    class SIPTestObserver implements Observer {
        private boolean sawEvent = false;

        public void update(Object arg) {
            sawEvent = true;
        }

        public boolean getSawEvent() {
            return sawEvent;
        }

        public void reset() {
            sawEvent = false;
        }
    }
}
