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

import com.sun.grizzly.http.SelectorThread;
import com.sun.grizzly.tcp.Adapter;

import junit.framework.TestCase;

import org.jmock.Expectations;
import org.jmock.Mockery;

import org.jmock.integration.junit4.JUnit4Mockery;

import org.jmock.lib.legacy.ClassImposteriser;


public class EmbeddedServerControllerTest extends TestCase {
    private static final int SERVER_PORT_1 = 8282;
    private static final int SERVER_PORT_2 = 5656;
    private static final int SERVER_PORT_3 = 9191;
    private Mockery context = new JUnit4Mockery();

    public void testStart() throws Exception {
        context.setImposteriser(ClassImposteriser.INSTANCE);

        final SelectorThread selectorThread = context.mock(SelectorThread.class);

        context.checking(new Expectations() {

                {
                    allowing(selectorThread).setPort(SERVER_PORT_1);
                    allowing(selectorThread).setPort(SERVER_PORT_2);
                    allowing(selectorThread).setAdapter(with(any(Adapter.class)));
                    allowing(selectorThread).initEndpoint();
                    allowing(selectorThread).startEndpoint();
                    allowing(selectorThread).stopEndpoint();
                    atLeast(1).of(selectorThread).isRunning();
                    will(returnValue(true));
                }
            });

        EmbeddedServerController.setSelectorThread(selectorThread);
        EmbeddedServerController.setRssManager(null);

        assertEquals(0, EmbeddedServerController.getPort());

        EmbeddedServerController.startServer(SERVER_PORT_1);
        assertEquals(SERVER_PORT_1, EmbeddedServerController.getPort());
        EmbeddedServerController.startServer(SERVER_PORT_3);
        assertEquals(SERVER_PORT_1, EmbeddedServerController.getPort());

        EmbeddedServerController.stopServer();
        assertEquals(0, EmbeddedServerController.getPort());

        EmbeddedServerController.startServer(SERVER_PORT_2);
        assertEquals(SERVER_PORT_2, EmbeddedServerController.getPort());
    }
}
