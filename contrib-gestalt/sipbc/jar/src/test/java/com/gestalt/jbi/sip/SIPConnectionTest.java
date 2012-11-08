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

import javax.sip.ClientTransaction;
import javax.sip.ResponseEvent;
import javax.sip.address.SipURI;
import javax.sip.header.CallIdHeader;
import javax.sip.header.ExpiresHeader;
import javax.sip.message.Request;
import javax.sip.message.Response;


/**
 * Created by IntelliJ IDEA. User: csturtz Date: May 23, 2007 Time: 4:17:45 PM
 * To change this template use File | Settings | File Templates.
 */
@RunWith(JMock.class)
public class SIPConnectionTest extends TestCase {
    static final int PROXY_PORT = 12345;
    static final int PROXY_TIMEOUT = 3;
    static final int REGISTER_INTERVAL = 3600;
    Mockery context = new JUnit4Mockery();

    @Test
    public void testRegister() throws Exception {
        context.setImposteriser(ClassImposteriser.INSTANCE);

        final String uri1 = "uri1";

        SIPUser user = createTestUser();
        final RequestGenerator requestGenerator = context.mock(RequestGenerator.class);
        final ResponseGenerator responsegenerator = context.mock(ResponseGenerator.class);
        final SIPServer sipServer = context.mock(SIPServer.class);

        final Request request = context.mock(Request.class);
        final SipURI sipURI = context.mock(SipURI.class);

        context.checking(new Expectations() {

                {
                    one(sipServer)
                        .setSipConnection(with(any(SIPConnection.class)));
                    one(requestGenerator).getUserSipURI();
                    will(returnValue(sipURI));
                    one(requestGenerator)
                        .createRegisterRequest(sipURI, REGISTER_INTERVAL, null);
                    will(returnValue(request));
                    one(requestGenerator)
                        .sendRequest(request, null, null, true, false);
                    will(returnValue(null));
                    one(requestGenerator).getUserURI();
                    will(returnValue(uri1));
                }
            });

        // test register w/o security
        SIPConnection sipConnection = new SIPConnection(user, null,
                requestGenerator, responsegenerator, sipServer);
        boolean returnVal = sipConnection.register();

        assertTrue("Register returned true (shouldn't do this w/o being a full integration test)",
            !returnVal);
    }

    @Test
    public void testRegisterProxyAuthentication() throws Exception {
        // Test sending REGISTER and receiving 407 response; followed by sending
        // a REGISTER w/ Authentication and receiving 200 response
        context.setImposteriser(ClassImposteriser.INSTANCE);

        final String uri1 = "uri1";

        SIPUser user = createTestUser();
        final RequestGenerator requestGenerator = context.mock(RequestGenerator.class);
        final ResponseGenerator responsegenerator = context.mock(ResponseGenerator.class);
        final SIPServer sipServer = context.mock(SIPServer.class);

        final Request request = context.mock(Request.class);
        final ResponseEvent responseEvent = context.mock(ResponseEvent.class);
        final Response response = context.mock(Response.class);
        final ClientTransaction clientTransaction = context.mock(ClientTransaction.class);
        final ExpiresHeader expiresHeader = context.mock(ExpiresHeader.class);
        final CallIdHeader callIdHeader = context.mock(CallIdHeader.class);

        final SipURI sipURI = context.mock(SipURI.class);

        context.checking(new Expectations() {

                {
                    one(sipServer)
                        .setSipConnection(with(any(SIPConnection.class)));
                    one(requestGenerator).getUserSipURI();
                    will(returnValue(sipURI));
                    one(requestGenerator)
                        .createRegisterRequest(sipURI, REGISTER_INTERVAL,
                        callIdHeader);
                    will(returnValue(request));
                    one(requestGenerator)
                        .createRequestWithAuthentication(with(any(String.class)),
                        with(any(String.class)), with(any(String.class)),
                        with(any(Response.class)), with(any(Request.class)));
                    will(returnValue(request));
                    one(requestGenerator)
                        .sendRequest(request, null, null, true, false);
                    will(returnValue(null));
                    one(requestGenerator).getUserURI();
                    will(returnValue(uri1));
                    atLeast(1).of(responseEvent).getResponse();
                    will(returnValue(response));
                    one(response).getHeader(CallIdHeader.NAME);
                    will(returnValue(callIdHeader));
                    one(responseEvent).getClientTransaction();
                    will(returnValue(clientTransaction));
                    one(clientTransaction).getRequest();
                    will(returnValue(request));
                    one(request).getExpires();
                    will(returnValue(expiresHeader));
                    one(expiresHeader).getExpires();
                    will(returnValue(REGISTER_INTERVAL));
                }
            });

        // test register w/o security
        SIPConnection sipConnection = new SIPConnection(user, null,
                requestGenerator, responsegenerator, sipServer);
        boolean returnVal = sipConnection.sendRegisterWithAuthentication(responseEvent);

        assertTrue("Register returned false", returnVal);
    }

    @Test
    public void testReceiveRegisterResponse() {
        // @TODO - IMPLEMENT: OK, 407, unsupported
    }

    @Test
    public void testSendMessage() {
        // @ TODO - IMPLEMENT
    }

    @Test
    public void testMakeCall() {
        // @TODO - IMPLEMENT
    }

    @Test
    public void testEndCall() {
        // @TODO - IMPLEMENT
    }

    @Test
    public void testAcceptCall() {
        // @TODO - IMPLEMENT
    }

    private SIPUser createTestUser() throws Exception {
        String username = "username";
        String password = "pass";
        String proxyhost = "localhost";

        return SIPUser.getSIPUser(username, password, proxyhost, PROXY_PORT,
            PROXY_TIMEOUT);
    }
}
