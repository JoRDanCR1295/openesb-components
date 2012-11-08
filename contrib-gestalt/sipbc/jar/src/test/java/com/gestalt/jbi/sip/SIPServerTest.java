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

import com.gestalt.sip.utilities.message.RequestGenerator;
import com.gestalt.sip.utilities.message.ResponseGenerator;

import junit.framework.TestCase;

import org.jmock.Expectations;
import org.jmock.Mockery;

import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;

import org.jmock.lib.legacy.ClassImposteriser;

import org.junit.Test;

import org.junit.runner.RunWith;

import java.lang.reflect.Field;

import javax.sip.RequestEvent;
import javax.sip.ServerTransaction;
import javax.sip.header.ContentTypeHeader;
import javax.sip.message.Request;
import javax.sip.message.Response;


@RunWith(JMock.class)
public class SIPServerTest extends TestCase {
    public static final int WAIT_TIME = 2000;
    Mockery context = new JUnit4Mockery();

    @SuppressWarnings("unchecked")
    @Test
    public void testProcessTextMessageRequest() throws Exception {
        context.setImposteriser(ClassImposteriser.INSTANCE);

        final String uri1 = "uri1";
        final ResponseGenerator responseGenerator = context.mock(ResponseGenerator.class);
        final RequestGenerator requestGenerator = context.mock(RequestGenerator.class);
        final Response response = context.mock(Response.class);

        final RequestEvent requestEvent = context.mock(RequestEvent.class);
        final Request request = context.mock(Request.class);
        final ServerTransaction serverTransaction = context.mock(ServerTransaction.class);
        final ContentTypeHeader contentTypeHeader = context.mock(ContentTypeHeader.class);

        Class<RequestEvent> requestEventClass = (Class<RequestEvent>) Class.forName(
                "javax.sip.RequestEvent");

        Field serverTransactionField = requestEventClass.getDeclaredField(
                "m_transaction");
        serverTransactionField.setAccessible(true);
        serverTransactionField.set(requestEvent, serverTransaction);

        Field requestField = requestEventClass.getDeclaredField("m_request");
        requestField.setAccessible(true);
        requestField.set(requestEvent, request);

        context.checking(new Expectations() {

                {
                    atLeast(2).of(requestEvent).getRequest();
                    will(returnValue(request));
                    one(requestEvent).getServerTransaction();
                    will(returnValue(serverTransaction));

                    atLeast(1).of(request).getMethod();
                    will(returnValue(Request.MESSAGE));

                    one(serverTransaction).getRequest();
                    will(returnValue(request));

                    one(responseGenerator).generateByeOkResponse(request);
                    will(returnValue(response));
                    one(responseGenerator)
                        .sendResponse(response, serverTransaction);

                    one(requestGenerator).getUserURI();
                    will(returnValue(uri1));

                    one(request).getHeader(ContentTypeHeader.NAME);
                    will(returnValue(contentTypeHeader));
                    one(contentTypeHeader).getContentType();
                    will(returnValue("text"));
                }
            });

        SIPTestObserver o = new SIPTestObserver();
        SIPObservable.addObserver(uri1, o);

        // invoke
        SIPServer sipServer = new SIPServer(null, requestGenerator,
                responseGenerator);
        sipServer.processRequest(requestEvent);
        Thread.sleep(WAIT_TIME);

        // asseert requestWatchers have been called (only if message is text) <-- 2 tests?
        assertTrue("SIP Observer not Notified!", o.getSawEvent());

        SIPObservable.removeObservers(uri1);
    }

    @SuppressWarnings("unchecked")
    @Test
    public void testProcessNonTextMessageRequest() throws Exception {
        context.setImposteriser(ClassImposteriser.INSTANCE);

        final String uri1 = "uri1";

        final ResponseGenerator responseGenerator = context.mock(ResponseGenerator.class);
        final Response response = context.mock(Response.class);

        final RequestEvent requestEvent = context.mock(RequestEvent.class);
        final Request request = context.mock(Request.class);
        final ServerTransaction serverTransaction = context.mock(ServerTransaction.class);
        final ContentTypeHeader contentTypeHeader = context.mock(ContentTypeHeader.class);

        Class<RequestEvent> requestEventClass = (Class<RequestEvent>) Class.forName(
                "javax.sip.RequestEvent");

        Field serverTransactionField = requestEventClass.getDeclaredField(
                "m_transaction");
        serverTransactionField.setAccessible(true);
        serverTransactionField.set(requestEvent, serverTransaction);

        Field requestField = requestEventClass.getDeclaredField("m_request");
        requestField.setAccessible(true);
        requestField.set(requestEvent, request);

        context.checking(new Expectations() {

                {
                    exactly(2).of(requestEvent).getRequest();
                    will(returnValue(request));
                    one(requestEvent).getServerTransaction();
                    will(returnValue(serverTransaction));

                    one(request).getMethod();
                    will(returnValue(Request.MESSAGE));

                    one(serverTransaction).getRequest();
                    will(returnValue(request));

                    one(responseGenerator).generateByeOkResponse(request);
                    will(returnValue(response));
                    one(responseGenerator)
                        .sendResponse(response, serverTransaction);

                    one(request).getHeader(ContentTypeHeader.NAME);
                    will(returnValue(contentTypeHeader));
                    one(contentTypeHeader).getContentType();
                    will(returnValue("nontext"));
                }
            });

        SIPTestObserver o = new SIPTestObserver();
        SIPObservable.addObserver(uri1, o);

        // invoke
        SIPServer sipServer = new SIPServer(null, null, responseGenerator);
        sipServer.processRequest(requestEvent);
        Thread.sleep(WAIT_TIME);

        // asseert requestWatchers have been called (only if message is text) <-- 2 tests?
        assertTrue("SIP Observer unexpectedly notified!", !o.getSawEvent());

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
