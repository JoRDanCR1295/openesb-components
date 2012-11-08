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
import com.sun.jbi.internationalization.Messages;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sip.DialogTerminatedEvent;
import javax.sip.IOExceptionEvent;
import javax.sip.RequestEvent;
import javax.sip.ResponseEvent;
import javax.sip.ServerTransaction;
import javax.sip.SipListener;
import javax.sip.SipProvider;
import javax.sip.TimeoutEvent;
import javax.sip.TransactionTerminatedEvent;
import javax.sip.header.ContentTypeHeader;
import javax.sip.message.Request;
import javax.sip.message.Response;


/**
 * @author : csturtz
 */
public class SIPServer implements SipListener {
    private static final Logger log = Messages.getLogger(SIPServer.class);
    private static Messages messages = Messages.getMessages(SIPServer.class);
    private RequestGenerator requestGenerator;
    private ResponseGenerator responseGenerator;
    private SipProvider sipProvider;
    private SIPConnection sipConnection;

    public SIPServer(SipProvider sipProvider,
        RequestGenerator requestGenerator, ResponseGenerator responseGenerator) {
        this.sipProvider = sipProvider;
        this.requestGenerator = requestGenerator;
        this.responseGenerator = responseGenerator;
    }

    public SipProvider getSipProvider() {
        return this.sipProvider;
    }

    public SIPConnection getSipConnection() {
        return sipConnection;
    }

    public void setSipConnection(SIPConnection sipConnection) {
        this.sipConnection = sipConnection;
    }

    /**
     * After the Stack receives a Request, this class is notified through a
     * RequestEvent via this method.
     *
     * @param requestEvent
     */
    public void processRequest(RequestEvent requestEvent) {
        if (log.isLoggable(Level.FINER)){
            log.log(Level.FINER, "Received a request: \n" + requestEvent.getRequest().toString());
        }
        String method = requestEvent.getRequest().getMethod();

        if (method.equalsIgnoreCase(Request.MESSAGE)) {
            ServerTransaction serverTransaction = requestEvent.getServerTransaction();

            if (serverTransaction == null) {
                try {
                    serverTransaction = sipProvider.getNewServerTransaction(requestEvent.getRequest());
                } catch (Exception e) {
                    log.log(Level.WARNING,messages.getString("SIPBC-W00264.unableToGetTheServerTransactionMustSendWithSipProvidersendResponse"),e);
                }
            }

            try {
                Request request = serverTransaction.getRequest();
                Response response = responseGenerator.generateByeOkResponse(request);

                responseGenerator.sendResponse(response, serverTransaction);
            } catch (Exception e) {
                log.log(Level.WARNING, messages.getString("SIPBC-W00265.anExceptionOccuredWhileCreatingAResponse"), e);
            }

            String contentTypeMajor = "";
            ContentTypeHeader contentTypeHeader = (ContentTypeHeader) requestEvent.getRequest()
                                                                                  .getHeader(ContentTypeHeader.NAME);

            if (contentTypeHeader != null) {
                contentTypeMajor = contentTypeHeader.getContentType();
            }

            if (contentTypeMajor.equalsIgnoreCase("text")) {
                SIPObservable.notifyObservers(requestGenerator.getUserURI(),
                    requestEvent);
            } else {

                if (log.isLoggable(Level.FINER)){
                    log.log(Level.FINER,"The message did not contain text");
                }
            }
        } else {
            SIPObservable.notifyObservers(requestGenerator.getUserURI(),
                requestEvent);
        }
    }

    /**
     * After the Stack receives a Response, this class is notified through a
     * ResponseEvent via this method.
     *
     * @param responseEvent
     */
    public void processResponse(ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINER)){
            log.log(Level.FINER, "Received a response: \n" +
                responseEvent.getResponse().toString());
        }
        SIPObservable.notifyObservers(requestGenerator.getUserURI(),
            responseEvent);
    }

    public void processTimeout(TimeoutEvent timeoutEvent) {
        if (log.isLoggable(Level.FINER)){
            log.log(Level.FINER, "A timeout was received: \n" + timeoutEvent.getTimeout().toString());
        }
        // @TODO - IMPLEMENT
    }

    public void processIOException(IOExceptionEvent ioExceptionEvent) {
        if (log.isLoggable(Level.FINER)){
            log.log(Level.FINER, "An IOException was received!");
        }
        // @TODO - IMPLEMENT
    }

    public void processTransactionTerminated(
        TransactionTerminatedEvent transactionTerminatedEvent) {
        if (log.isLoggable(Level.FINER)){
            log.log(Level.FINER,"Received a TransactionTerminated!");
        }
        // @TODO - IMPLEMENT
    }

    public void processDialogTerminated(
        DialogTerminatedEvent dialogTerminatedEvent) {
        if (log.isLoggable(Level.FINER)){
            log.log(Level.FINER, "Received a DialogTerminated!");
        }
        // @TODO - IMPLEMENT
    }
}
