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
import com.gestalt.sip.utilities.message.SipUtil;
import com.sun.jbi.internationalization.Messages;

import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.sip.ClientTransaction;
import javax.sip.Dialog;
import javax.sip.RequestEvent;
import javax.sip.ResponseEvent;
import javax.sip.ServerTransaction;
import javax.sip.SipStack;
import javax.sip.Transaction;
import javax.sip.TransactionAlreadyExistsException;
import javax.sip.TransactionUnavailableException;
import javax.sip.address.SipURI;
import javax.sip.address.URI;
import javax.sip.header.CSeqHeader;
import javax.sip.header.CallIdHeader;
import javax.sip.header.FromHeader;
import javax.sip.header.ToHeader;
import javax.sip.message.Request;
import javax.sip.message.Response;


/**
 * @author : csturtz
 */
public class SIPConnection implements Observer {
    private static final Logger log = Messages.getLogger(SIPConnection.class);
    private static final Messages messages = Messages.getMessages(SIPConnection.class);
    private static final int REGISTER_DURATION = 3600;
    private static final int THREAD_SLEEP_TIME = 1000;
    private static final double REREGISTER_RATE = 0.85;
    private static final String END_REQUEST = "END";
    private static final long REGISTER_TIMER = 20000;
    private static final long INVITE_RESPONSE_TIMER = 15000;
    private static final int SCHEDULER_THREADS = 1;
    private Timer inviteResponseTimer;
    private ScheduledExecutorService scheduler;
    private boolean registered = false;
    private boolean attemptRegister = true;
    private SIPUser user;
    private SIPServer sipServer;
    private RequestGenerator requestGenerator;
    private ResponseGenerator responseGenerator;
    private SipStack sipStack;

    public SIPConnection(SIPUser user, SipStack sipStack,
        RequestGenerator requestGenerator, ResponseGenerator responseGenerator,
        SIPServer sipServer) {
        this.user = user;
        this.sipServer = sipServer;
        this.requestGenerator = requestGenerator;
        this.responseGenerator = responseGenerator;
        this.sipStack = sipStack;

        sipServer.setSipConnection(this);
        scheduler = Executors.newScheduledThreadPool(SCHEDULER_THREADS);

        SIPObservable.addObserver(requestGenerator.getUserURI(), this);
    }

    /**
     * Registers a user with their defined proxy. We block here for
     * REGISTER_TIMER milliseconds so that we may provide meaningful feedback on
     * the activation of a new SIPEndpoint.
     *
     * @return whether or not the register was successful
     */
    public Boolean register() {
        register(REGISTER_DURATION);

        long startTime = System.currentTimeMillis();

        while (((System.currentTimeMillis() - startTime) < REGISTER_TIMER) &&
                (!registered)) {
            try {
                if (log.isLoggable(Level.FINEST)) {
                    log.log(Level.FINEST,"Waiting for response to REGISTER Request");
                }
                Thread.sleep(THREAD_SLEEP_TIME);
            } catch (InterruptedException e) {
                log.log(Level.WARNING,messages.getString("SIPBC-W00200.caughtAnInterruptedExceptionErrorWhileWaitingForAREGISTERResponse"),e);
            }
        }

        if (!registered) {
            attemptRegister = false;
            log.log(Level.INFO,messages.getString("SIPBC-R00201.registrationFailed"));
        } else {
            log.log(Level.INFO,messages.getString("SIPBC-R00202.registrationSuccessful"));
        }

        return registered;
    }

    /**
     * Unregister a user from their defined proxy. Again, we block here for
     * REGISTER_TIMER milliseconds so that we may provide meaningful feedback on
     * the deactivation of a SIPEndpoint.
     *
     * @return whether or not the unregister was successful
     */
    public Boolean unregister() {
        register(0);

        long startTime = System.currentTimeMillis();

        while (((System.currentTimeMillis() - startTime) < REGISTER_TIMER) &&
                (registered)) {
            try {
                if (log.isLoggable(Level.FINEST)) {
                    log.log(Level.FINEST,"Waiting for response to REGISTER Request");
                }
                Thread.sleep(THREAD_SLEEP_TIME);
            } catch (InterruptedException e) {
                log.log(Level.WARNING,messages.getString("SIPBC-W00203.caughtAnInterruptedExceptionErrorWhileWaitingForAREGISTERResponse"),e);
            }
        }

        if (registered) {
            attemptRegister = false;
            log.log(Level.INFO,messages.getString("SIPBC-R00204.failedToUnregister"));
        } else {
            log.log(Level.INFO,messages.getString("SIPBC-R00205.successfullyUnregistered"));
        }

        return !registered;
    }

    /**
     * This methods sends a REGISTER request with the provided duration on its
     * EXPIRES header. A duration of 0 (Zero) will unregister the UA.
     *
     * We keep this method as private to control the duration a UA is
     * registered. We do provide two public methods, for registering and
     * unregistering.
     *
     * @param duration
     * @return
     */
    private Boolean register(int duration) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Registering the SIP User");
        }

        Boolean returnValue = false;

        if (duration <= 0) {
            duration = 0;

            ScheduledFuture<?> future = user.getScheduledFuture();

            if (future != null) {
                future.cancel(true);
            }
        }

        try {
            Request request = requestGenerator.createRegisterRequest(requestGenerator.getUserSipURI(),
                    duration, null);
            requestGenerator.sendRequest(request, null, null, true, false);
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Sending a REGISTER Request: " + request);
            }
            returnValue = true;
        } catch (Exception e) {
            log.log(Level.WARNING, messages.getString("SIPBC-W00206.exceptionWendingREGISTER"), e);
            returnValue = false;
        }

        return returnValue;
    }

    /**
     * This method is called when a 407 response is receivec to a REGISTER
     * request. This will occur when authentication is enabled on the proxy that
     * the UA is registering with.
     *
     * @param responseEvent
     * @return
     */

    // @TODO - make this method private, update SIPConnectionTest to use
    // Reflection to access it
    protected Boolean sendRegisterWithAuthentication(
        ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Registering the SIP User with Authentication");
        }
        Boolean returnValue = false;

        CallIdHeader callIdHeader = (CallIdHeader) responseEvent.getResponse()
                                                                .getHeader(CallIdHeader.NAME);

        Integer duration = responseEvent.getClientTransaction().getRequest()
                                        .getExpires().getExpires();

        try {
            Request request = requestGenerator.createRegisterRequest(requestGenerator.getUserSipURI(),
                    duration, callIdHeader);
            request = requestGenerator.createRequestWithAuthentication(user.getUsername(),
                    user.getPassword(), user.getProxyhost(),
                    responseEvent.getResponse(), request);
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Sending REGISTER with Authentication: " + request);
            }
            requestGenerator.sendRequest(request, null, null, true, false);
            returnValue = true;
        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00207.anExceptionOccuredSendingREGISTERWithAuthentication"), e);
        }

        return returnValue;
    }

    /**
     * We first check that then provided method parameter matches somethign we
     * can handle. We then call the appropriate method corresponding to the
     * method paramter to send the request.
     *
     * @param method -
     *            Should match a SipRequest type (e.g. MESSAGE, INVITE, BYE) or
     *            utilize a proprietary method (which only includes END right
     *            now).
     * @param remoteUri -
     *            The URI of the remote UA to receive this request
     * @param content -
     *            content for the request (the text of an instant message for
     *            example)
     * @return
     */
    public Boolean sendRequest(String method, String remoteUri, String content) {
        if (method.equalsIgnoreCase(Request.MESSAGE)) {
            return sendMessage(content, remoteUri);
        } else if (method.equalsIgnoreCase(Request.INVITE)) {
            return sendInvite(remoteUri, content);
        } else if (method.equalsIgnoreCase(Request.BYE)) {
            return sendBye(remoteUri);
        } else if (method.equalsIgnoreCase(Request.CANCEL)) {
            return sendCancel(remoteUri);
        } else if (method.equalsIgnoreCase(END_REQUEST)) {
            return endCall(remoteUri);
        } else {
            log.log(Level.WARNING,messages.getString("SIPBC-W00208.unsupportedRequestType", method));
        }

        return false;
    }

    /**
     * First check that the method parameter matches a response type we can
     * handle. Then call the appropriate method corresponding the the method
     * parameter to send the resposne.
     *
     * @param method -
     *            should match a SipRequest type (e.g. MESSAGE, INVITE, etc..)
     * @param remoteUri -
     *            the remote uri the response is going to
     * @param status -
     *            the status code of the response (type of response to send)
     * @param content -
     *            any content to be incldued in the response (only applies to a
     *            200 response to an INVITE request at this time)
     * @return
     */
    public Boolean sendResponse(String method, String remoteUri, int status,
        String content) {
        if (method.equalsIgnoreCase(Request.INVITE)) {
            // Cancel Invite Response Timer
            inviteResponseTimer.cancel();

            if (status == Response.OK) {
                return sendOkResponseToInvite(remoteUri, content);
            } else if (status == Response.DECLINE) {
                return sendDeclineResponse(remoteUri);
            } else if (status == Response.BUSY_HERE) {
                return sendBusyResponse(remoteUri);
            } else {
                log.log(Level.WARNING,
                    messages.getString("SIPBC-W00209.thereIsNoSupportForSendingThisStatusAsAResponseToAnINVITERequest"));
            }
        } else {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00210.sendingResponsesToTheGivenMethodIsNotSupported",
                method));
        }

        return false;
    }

    /**
     * Send a MESSAGE SipRequest.
     *
     * @param message -
     *            content of the SipRequest; the actual message
     * @param targetUri -
     *            remote uri to send the request to
     * @return
     */
    private Boolean sendMessage(String message, String targetUri) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Sending a MESSAGE Request to " + targetUri + "with content: " +
                message);
        }

        if (targetUri.startsWith("sip:")) {
            targetUri = targetUri.substring("sip:".length());
        }

        Boolean returnValue = false;

        SipURI myURI = requestGenerator.getUserSipURI();

        try {
            Request msgRequest = requestGenerator.createMessageRequest(myURI,
                    targetUri, message, null);
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Sending a MESSAGE Request: " + msgRequest);
            }
            requestGenerator.sendRequest(msgRequest, null, null, true, false);

            returnValue = true;
        } catch (Exception e) {
            log.log(Level.WARNING, messages.getString("SIPBC-W00211.exceptionSendingTheMESSAGERequest"), e);
            returnValue = false;
        }

        return returnValue;
    }

    /**
     * This method is called after receiving a 407 to a MESSAGE Request. We will
     * add the appropriate ProxyAuthorizationHeaeder and resend the request.
     *
     * @param responseEvent -
     *            the 407 response event to the original MESSAGE Request
     * @return
     */
    public Boolean sendMessageWithAuthentication(ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Sending a MESSAGE request with authentication");
        }
        Boolean returnValue = false;

        CallIdHeader callIdHeader = (CallIdHeader) responseEvent.getResponse()
                                                                .getHeader(CallIdHeader.NAME);

        byte[] content = responseEvent.getClientTransaction().getRequest()
                                      .getRawContent();
        String message = new String(content);

        ToHeader toHeader = (ToHeader) responseEvent.getClientTransaction()
                                                    .getRequest()
                                                    .getHeader(ToHeader.NAME);
        URI uri = toHeader.getAddress().getURI();
        String targetUri = SipUtil.buildSipUserAtHost(uri);

        SipURI myURI = requestGenerator.getUserSipURI();

        try {
            Request msgRequest = requestGenerator.createMessageRequest(myURI,
                    targetUri, message, callIdHeader);
            msgRequest = requestGenerator.createRequestWithAuthentication(user.getUsername(),
                    user.getPassword(), user.getProxyhost(),
                    responseEvent.getResponse(), msgRequest);
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Sending a MESSAGE request with authentication: " + msgRequest);
            }
            requestGenerator.sendRequest(msgRequest, null, null, true, false);
            returnValue = true;
        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00212.anExceptionOccuredWhileSendingMESSAGEWithAuthentication"), e);
            returnValue = false;
        }

        return returnValue;
    }

    /**
     * Send an INVITE SipRequest.
     *
     * @param targetUri -
     *            remote uri to send the request to
     * @param sdp -
     *            the SDP to include as content in the Request
     * @return
     */
    private Boolean sendInvite(String targetUri, String sdp) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Making the call to the TargetUri: " + targetUri);
        }

        boolean success = false;

        // make sure session is not already in progress with remote entity
        if (user.getSession(targetUri) != null) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00213.aSessionAlreadyExistsForTheRemoteUri", targetUri));

            return false;
        }

        SipURI myURI = requestGenerator.getUserSipURI();

        try {
            Request request = requestGenerator.createInviteRequest(myURI,
                    targetUri, sdp, null);
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Sending an INVITE Request: " + request);
            }
            ClientTransaction ctx = requestGenerator.sendRequest(request, null,
                    null, true, false);

            // Add session to user - this also creates and initializes the
            // session
            user.addSession(targetUri, ctx);

            success = true;
        } catch (Exception e) {
            log.log(Level.WARNING, messages.getString("SIPBC-W00214.exceptionCreating/SendingTheINVITERequest"), e);
            success = false;
        }

        return success;
    }

    /**
     * This method is called after receiving a 407 to a INVITE Request. We will
     * add the appropriate ProxyAuthorizationHeaeder and resend the request. In
     * addition, the ProxyAuthorizationHeader will be saved off to be used for
     * any more Requests inside a dialog started with this INVITE Request.
     *
     * @param responseEvent -
     *            the 407 respone event to the original INVITE request
     * @return
     */
    private Boolean sendInviteWithAuthentication(ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Sending an INVITE request with Authentication");
        }
        Boolean success = false;

        CallIdHeader callIdHeader = (CallIdHeader) responseEvent.getResponse()
                                                                .getHeader(CallIdHeader.NAME);

        SipURI myURI = requestGenerator.getUserSipURI();
        Request originalRequest = responseEvent.getClientTransaction()
                                               .getRequest();
        String targetUri = SipUtil.buildUserAtHost(((ToHeader) originalRequest
                                                    .getHeader(ToHeader.NAME)).getAddress()
                                                    .getURI());
        String sdp = new String(originalRequest.getRawContent());

        try {
            Request request = requestGenerator.createInviteRequest(myURI,
                    targetUri, sdp, callIdHeader);
            Request inviteWithAuth = requestGenerator.createRequestWithAuthentication(user.getUsername(),
                    user.getPassword(), user.getProxyhost(),
                    responseEvent.getResponse(), request);

            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Sending an INVITE request with Authentication: " + inviteWithAuth);
            }

            ClientTransaction ctx = requestGenerator.sendRequest(inviteWithAuth,
                    null, null, true, false);
            SIPSession session = user.getSession(targetUri);

            if (session != null) {
                // update transaction
                session.setTx(ctx);
            } else {
                log.log(Level.WARNING,
                    messages.getString("SIPBC-W00215.aSessionWithTheFollowingRemoteUriWasExpectedButNotFound",
                    targetUri));
                user.addSession(targetUri, ctx);
            }

            success = true;
        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00216.exceptionCreating/SendingAnINVITERequestWithAuthentication"), e);
            success = false;
        }

        return success;
    }

    /**
     * First, we check to make sure the UA is involved in an established session
     * or a pending session. Then, after analyzing the situation, we seend the
     * appropriate Request/Response to end the call.
     *
     * @param remoteUri
     * @return
     */
    private boolean endCall(String remoteUri) {
        if (log.isLoggable(Level.FINE)){
        log.log(Level.FINE,"End Call");
        }

        boolean success = false;

        if (user.sessionExists(remoteUri)) {
            SIPSession session = user.getSession(remoteUri);

            if (session.isEstablished()) {
                success = sendBye(remoteUri);
            } else { // pending status

                if (session.getTx() instanceof ServerTransaction) {
                    success = sendDeclineResponse(remoteUri);
                } else {
                    success = sendCancel(remoteUri);
                }
            }
        } else {
            log.log(Level.WARNING,messages.getString("SIPBC-W00217.currentlyThereIsNoSessionToEnd"));
            success = false;
        }

        return success;
    }

    /**
     * Send a BYE request to end an established session.
     *
     * We first verify that the UA has an established session with the provided
     * remoteUri parameter.
     *
     * @param remoteUri -
     *            target of the BYE request
     * @return
     */
    private boolean sendBye(String remoteUri) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Send Bye");
        }

        boolean success = false;
        Dialog dialog = null;
        String caller = "";
        String callee = "";
        Transaction tx = null;

        if (!user.sessionExists(remoteUri)) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00218.noSessionWasEstablishedWithTheProvidedRemoteURI",
                remoteUri));
            return false;
        } else if (!user.getSession(remoteUri).isEstablished()) { // pending
            log.log(Level.WARNING,messages.getString("SIPBC-W00219.aSessionMustFirstBeEstablishedToEndWithABYE"));

            return false;
        } else { // established
            tx = user.getSession(remoteUri).getTx();
        }

        if (tx instanceof ServerTransaction) {
            ServerTransaction stx = (ServerTransaction) tx;
            dialog = stx.getDialog();
            caller = SipUtil.buildUserAtHost(dialog.getRemoteParty().getURI());
            callee = requestGenerator.getUserURI();
        } else if (tx instanceof ClientTransaction) {
            ClientTransaction ctx = (ClientTransaction) tx;
            dialog = ctx.getDialog();
            caller = requestGenerator.getUserURI();
            callee = SipUtil.buildUserAtHost(dialog.getRemoteParty().getURI());
        }

        try {
            Request bye = requestGenerator.createByeRequest(dialog, caller,
                    callee, null);
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Sending a BYE Request: " + bye);
            }
            requestGenerator.sendRequest(bye, dialog, null, true, false);
            success = true;
        } catch (Exception e) {
            log.log(Level.WARNING, messages.getString("SIPBC-W00220.anExceptionOccuredWhileCreating/SendingTheBYERequest"), e);
            success = false;
        }

        return success;
    }

    /**
     * This method is called after receiving a 407 to a BYE Request. We will add
     * the appropriate ProxyAuthorizationHeaeder and resend the request.
     *
     * @param responseEvent -
     *            the 407 response event to the original BYE Request
     * @return
     */
    private boolean sendByeWithAuthentication(ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Sending a BYE with authentication");
        }

        boolean success = false;

        Transaction tx = null;
        Dialog dialog = null;
        String caller = "";
        String callee = "";

        CallIdHeader callIdHeader = (CallIdHeader) responseEvent.getResponse()
                                                                .getHeader(CallIdHeader.NAME);

        String targetUri = SipUtil.buildUserAtHost(((ToHeader) responseEvent
                                                    .getClientTransaction()
                                                    .getRequest()
                                                    .getHeader(ToHeader.NAME)).getAddress()
                                                    .getURI());
        SIPSession session = user.getSession(targetUri);

        if (session != null) {
            tx = session.getTx();
        } else {
            log.log(Level.WARNING,messages.getString("SIPBC-W00221.noSessionWasFoundForTheRemoteUri", targetUri));
            return false;
        }

        if (tx instanceof ServerTransaction) {
            ServerTransaction stx = (ServerTransaction) tx;
            dialog = stx.getDialog();
            caller = SipUtil.buildUserAtHost(dialog.getRemoteParty().getURI());
            callee = requestGenerator.getUserURI();
        } else if (tx instanceof ClientTransaction) {
            ClientTransaction ctx = (ClientTransaction) tx;
            dialog = ctx.getDialog();
            caller = requestGenerator.getUserURI();
            callee = SipUtil.buildUserAtHost(dialog.getRemoteParty().getURI());
        }

        try {
            Request newBye = requestGenerator.createByeRequest(dialog, caller,
                    callee, callIdHeader);
            newBye = requestGenerator.createRequestWithAuthentication(user.getUsername(),
                    user.getPassword(), user.getProxyhost(),
                    responseEvent.getResponse(), newBye);

            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Sending a BYE with authentication: " + newBye);
            }
            requestGenerator.sendRequest(newBye, dialog, null, true, false);
            success = true;
        } catch (Exception e) {
            log.log(Level.WARNING, messages.getString("SIPBC-W00222.exceptionSendingBYEWithAuthentication"), e);
            success = false;
        }

        return success;
    }

    /**
     * Send a CANCEL SipRequest to end a pending session that the UA has
     * initiated.
     *
     * @param remoteUri -
     *            the remote uri that the session is pending with
     * @return
     */
    private boolean sendCancel(String remoteUri) {
        boolean success = false;
        ClientTransaction tx = null;

        if (!user.sessionExists(remoteUri)) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00223.noSessionExistsForTheRemoteUri", remoteUri));

            return false;
        } else {
            SIPSession session = user.getSession(remoteUri);

            if (session.isPending()) {
                if (session.getTx() instanceof ClientTransaction) {
                    tx = (ClientTransaction) session.getTx();
                } else {
                    log.log(Level.WARNING,messages.getString("SIPBC-W00224.unableToSendACANCELSignalTheLocalPartyDidNotInitiateTheCall"));

                    return false;
                }
            } else {
                log.log(Level.WARNING,messages.getString("SIPBC-W00225.canNotCancelASessionThatIsNotPending"));

                return false;
            }
        }

        try {
            Request cancel = requestGenerator.createCancelRequest(tx);
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Sending a CANCEL request: " + cancel);
            }
            requestGenerator.sendRequest(cancel, null, null, true, false);

            // removing session
            user.removeSession(remoteUri);

            success = true;
        } catch (Exception e) {
            log.log(Level.WARNING, messages.getString("SIPBC-W00226.anExceptionOccuredWhileCreating/SendingACANCELRequest"), e);
            success = false;
        }

        return success;
    }

    /**
     * Send a 603 DECLINE Response to an INVITE Request.
     *
     * We verify that the provided remote uri matches the remote uri of a
     * pending session.
     *
     * @param remoteUri
     * @return
     */
    private boolean sendDeclineResponse(String remoteUri) {
        boolean success = false;
        ServerTransaction tx = null;

        if (!user.sessionExists(remoteUri)) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00227.noCurrentSessionToEnd"));

            return false;
        } else {
            SIPSession session = user.getSession(remoteUri);

            if (session.isPending()) {
                if (session.getTx() instanceof ServerTransaction) {
                    tx = (ServerTransaction) session.getTx();
                }
            } else {
                log.log(Level.WARNING,
                    messages.getString("SIPBC-W00228.canNotEndAnEstablishedSessionWithADECLINEResponse"));

                return false;
            }
        }

        try {
            Response response = responseGenerator.generateDeclineInviteResponse(tx.getRequest(),
                    tx.getDialog());
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER, "Sending the following response to the INVITE request: " + response);
            }
            responseGenerator.sendResponse(response, tx);

            // Removing Session
            user.removeSession(remoteUri);

            success = true;
        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00229.anExceptionOccuredWhileCreating/SendingTheDECLINEResponse"), e);
            success = false;
        }

        return success;
    }

    /**
     * Send a 486 BUSY Response to an INVITE Request.
     *
     * This is performed when, for example, we receive an incoming INVITE
     * request, but are already involved in a session.
     *
     * @param remoteUri
     * @return
     */
    private Boolean sendBusyResponse(String remoteUri) {
        boolean success = false;
        ServerTransaction tx = null;

        if (!user.sessionExists(remoteUri)) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00230.noPendingSessionToRespondTo"));

            return false;
        } else {
            SIPSession session = user.getSession(remoteUri);

            if (session.isPending()) {
                if (session.getTx() instanceof ServerTransaction) {
                    tx = (ServerTransaction) session.getTx();
                }
            } else {
                log.log(Level.WARNING,
                    messages.getString("SIPBC-W00231.canNotEndAnEstablishedSessionWithABUSY_HEREResponse"));

                return false;
            }
        }

        Response busy = null;

        try {
            busy = responseGenerator.generateBusyResponse(tx.getRequest());
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Sending the following response to INVITE: " + busy);
            }
            responseGenerator.sendResponse(busy, tx);

            // remove session
            user.removeSession(remoteUri);

            success = true;
        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00232.anExceptionOccuredWhileCreating/SendingABUSYResponse"), e);
        }

        return success;
    }

    /**
     * Send a 200 OK Response to an INVITE Request.
     *
     * We first verify that the provided remoteUri matches the remote uri of a
     * pending session.
     *
     * @param remoteUri -
     *            remote uri to send the response to
     * @param content -
     *            the SDP to send to the remote uri in the content of the
     *            resposne
     * @return
     */
    private Boolean sendOkResponseToInvite(String remoteUri, String content) {
        ServerTransaction tx = null;
        Boolean success = false;

        if (content == null) {
            log.log(Level.SEVERE,messages.getString("SIPBC-E00233.theContentIsNULLPleaseProvideContentWhenRespondingWithOKToAnINVITERequest"));

            return false;
        }

        if (!user.sessionExists(remoteUri)) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00234.noSessionExistsWithTheProvidedRemoteUri",
                remoteUri));

            return false;
        } else {
            SIPSession session = user.getSession(remoteUri);

            if (session.isPending()) {
                if (session.getTx() instanceof ServerTransaction) {
                    tx = (ServerTransaction) session.getTx();
                } else {
                    log.log(Level.WARNING,messages.getString("SIPBC-W00235.aSessionMustBeInAPendingStateInitiatedByTheRemotePartyToSendAnOKResponse"));
                }
            } else {
                log.log(Level.WARNING,messages.getString("SIPBC-W00236.aSessionMustBeInAPendingStateInitiatedByTheRemotePartyToSendAnOKResponse"));
            }
        }

        Response ok = null;

        try {
            ok = responseGenerator.generateInviteOkResponse(tx.getRequest(),
                    tx.getDialog(), content);
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER, "Sending the following response to the INVITE request: " + ok);
            }
            responseGenerator.sendResponse(ok, tx);
            success = true;
        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00237.anExceptionOccuredWhileCreating/SendingAnOKResponseToTheINVITERequest"), e);
        }

        return success;
    }

    /**
     * Acknowledge that we have received a 2xx Response. All other responses
     * will be acknowledged by the stack on our behalf.
     *
     * @param responseEvent -
     *            ResposneEvent to acknowledge
     */
    private void sendAck(ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Sending ACK");
        }
        try {
            Request ack = requestGenerator.create2xxAck(responseEvent);
            requestGenerator.send2xxAck(ack, responseEvent.getDialog());
        } catch (RequestGenerator.RequestGeneratorException e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00238.anExceptionOccuredWhileTryingToCreate/SendACK "), e);
        }
    }

    /**
     * Handles a Response to a REGISTER Request. Based on the response status
     * code, the appropriate action will be taken (resend with authorization,
     * schedule auto- registration on behalf of the user, etc)
     *
     * @param responseEvent
     */
    private void handleRegisterResponse(ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Processing the response to a REGISTER request");
        }
        Response response = responseEvent.getResponse();
        int status = response.getStatusCode();

        if ((status == Response.PROXY_AUTHENTICATION_REQUIRED) ||
                (status == Response.UNAUTHORIZED)) {
            log.log(Level.INFO,
                messages.getString("SIPBC-W00239.receivedA407/401ErrorToAREGISTERRequestResendingRequestWithAuthentication"));

            if (attemptRegister) {
                sendRegisterWithAuthentication(responseEvent);
            } else {
                log.log(Level.WARNING,
                    messages.getString("SIPBC-W00240.stillUnableToRegister/UnregisterNoAdditionalAttemptsWillBeMade"));
            }
        } else if (status == Response.OK) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Received a 200 to a REGISTER request");
            }

            int duration = responseEvent.getClientTransaction().getRequest()
                                        .getExpires().getExpires();

            if (duration > 0) {
                registered = true;

                ScheduledFuture<?> future = user.getScheduledFuture();

                if (future == null) {
                    user.setScheduledFuture(scheduleAutoRegister());
                }
            } else {
                registered = false;
            }
        } else {
            log.log(Level.WARNING,messages.getString("SIPBC-W00241.thereWasAnUnsupportedResponseToAREGISTERRequest",
                status));
        }
    }

    /**
     * Handle responses to a MESSAGE Request. Take the appropriate action based
     * on the status code of the response.
     *
     * @param responseEvent
     */
    private void handleMessageResponse(ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Processing the response to a MESSAGE request");
        }

        int status = responseEvent.getResponse().getStatusCode();

        if (status == Response.PROXY_AUTHENTICATION_REQUIRED) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,
                    "Received a 407 error to a MESSAGE request.  Resending the request with authentication");
            }
            sendMessageWithAuthentication(responseEvent);
        } else {
            log.log(Level.WARNING,messages.getString("SIPBC-W00242.thereWasAnUnsupportedResponseToAREGISTERRequest", status));
        }
    }

    /**
     * Handle responses to INVITE Requests. Take the appropriate action based on
     * the status code of the response: - resend with ProxyAuthorizationHeader -
     * Mark a pending session as established - Send an ACK - log an unsupported
     * response
     *
     * @param responseEvent
     */
    private void handleInviteResponse(ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Processing the response to an INVITE request");
        }
        int status = responseEvent.getResponse().getStatusCode();
        String targetUri = SipUtil.buildUserAtHost(((ToHeader) responseEvent
                                                    .getClientTransaction()
                                                    .getRequest()
                                                    .getHeader(ToHeader.NAME)).getAddress()
                                                    .getURI());

        if (status == Response.OK) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Received a 200 to an INVITE request.");
            }

            if (user.sessionExists(targetUri)) {
                user.getSession(targetUri).establish();
                sendAck(responseEvent);
            } else {
                log.log(Level.WARNING,messages.getString("SIPBC-W00243.unableToFindASessionWithTheRemoteUri",
                    targetUri));
                // @TODO - what do we send here?
            }
        } else if (status == Response.PROXY_AUTHENTICATION_REQUIRED) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,
                    "Received a 407 error to an INVITE request. Resending the request with authentication");
            }
            sendInviteWithAuthentication(responseEvent);
        } else if ((status >= Response.TRYING) && (status < Response.OK)) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Received a 1XX response: " + status);
            }
        } else if ((status >= Response.OK) &&
                (status < Response.MULTIPLE_CHOICES)) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Received an unsupported 2XX response: " + status);
            }
        } else if ((status >= Response.MULTIPLE_CHOICES) &&
                (status < Response.BAD_REQUEST)) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Received an unsupported 3XX response: " + status +
                    "\nClearing the session");
            }

            if (user.sessionExists(targetUri)) {
                user.removeSession(targetUri);
            }
        } else if ((status >= Response.BAD_REQUEST) &&
                (status < Response.SERVER_INTERNAL_ERROR)) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Received an unsupported 4XX response: " + status +
                    "\nClearing the session");
            }

            if (user.sessionExists(targetUri)) {
                user.removeSession(targetUri);
            }
        } else if ((status >= Response.SERVER_INTERNAL_ERROR) &&
                (status < Response.BUSY_EVERYWHERE)) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Received an unsupported 5XX response: " + status +
                    "\nClearing the session");
            }

            if (user.sessionExists(targetUri)) {
                user.removeSession(targetUri);
            }
        } else if ((status >= Response.BUSY_EVERYWHERE)) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Received an unsupported 6XX response: " + status +
                    "\nClearing the session");
            }
            
            if (user.sessionExists(targetUri)) {
                user.removeSession(targetUri);
            }
        } else {
            log.log(Level.WARNING,messages.getString("SIPBC-W00244.receivedAnUnsupportedResponseToAnINVITERequest", status));

            if (user.sessionExists(targetUri)) {
                user.removeSession(targetUri);
            }
        }
    }

    /**
     * Hanlde responses to BYE Requests. Take appropriate action based on the
     * status code of the response.
     *
     * @param responseEvent
     */
    private void handleByeResponse(ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE, "Processing the response to a BYE request");
        }

        int status = responseEvent.getResponse().getStatusCode();
        String targetUri = SipUtil.buildUserAtHost(((ToHeader) responseEvent
                                                    .getClientTransaction()
                                                    .getRequest()
                                                    .getHeader(ToHeader.NAME)).getAddress()
                                                    .getURI());

        if (status == Response.OK) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Received a 200 to the BYE request");
            }
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE,"Clearing the session");
            }
            if (user.sessionExists(targetUri)) {
                user.removeSession(targetUri);
            }
        } else if (status == Response.PROXY_AUTHENTICATION_REQUIRED) {
            if (log.isLoggable(Level.FINE)){
                log.log(Level.FINE, "Received a 407 error to the BYE request");
            }
            sendByeWithAuthentication(responseEvent);
        } else {
            log.log(Level.WARNING, messages.getString("SIPBC-W00245.receivedAnUnsupportedResponseToABYERequest",status));
        }
    }

    /**
     * Hanlde responses to CANCEL Requests. Simply logging all responses at this
     * time.
     *
     * @param responseEvent
     */
    private void handleCancelResponse(ResponseEvent responseEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Processing the response to a CANCEL request");
        }
        int status = responseEvent.getResponse().getStatusCode();
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Received a CANCEL response: " + status);
        }
    }

    /**
     * Handle incoming BYE Requests Close any corresponding established sessions
     * and respond with a 200.
     *
     * @param requestEvent
     */
    private void handleByeRequest(RequestEvent requestEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Processing the incoming BYE request");
        }

        String remoteUri = SipUtil.buildUserAtHost(requestEvent.getDialog()
                                                               .getRemoteParty()
                                                               .getURI());

        if (!user.sessionExists(remoteUri)) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00246.noSessionIsEstablishedToEndWithRemoteUri", remoteUri));
        } else {
            SIPSession session = user.getSession(remoteUri);

            if (session.isPending()) {
                log.log(Level.WARNING,messages.getString("SIPBC-W00247.thePendingSessionCantBeEndedWithABYERequest"));
            }
        }
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Ending the currently established session");
        }
        user.removeSession(remoteUri);

        // Send 200 to BYE Request
        Response ok = null;

        try {
            ok = responseGenerator.generateByeOkResponse(requestEvent.getRequest());
            responseGenerator.sendResponse(ok,
                requestEvent.getServerTransaction());
        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00248.anExceptionOccuredWhileCreating/SendingAnOKResponseToABYERequest"), e);
        }
    }

    /**
     * Handle incoming CANCEL Request Cancel any corresponding pending sessions
     * and respond with a 200
     *
     * @param requestEvent
     */
    private void handleCancelRequest(RequestEvent requestEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Processing the incoming CANCEL request");
        }
        String remoteUri = SipUtil.buildUserAtHost(((FromHeader) requestEvent
                                                    .getRequest()
                                                    .getHeader(FromHeader.NAME)).getAddress()
                                                    .getURI());

        if (!user.sessionExists(remoteUri)) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00249.noSessionIsEstablishedToEndWithRemoteUri", remoteUri));
        } else {
            SIPSession session = user.getSession(remoteUri);

            if (session.isEstablished()) {
                log.log(Level.WARNING,messages.getString("SIPBC-W00250.theEstablishedSessionCanNotBeEndedWithABYERequest"));
            }
        }

        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Canceling the session");
        }
        user.removeSession(remoteUri);

        // respond with 200
        Response ok = null;

        try {
            ok = responseGenerator.generateCancelOkResponse(requestEvent.getRequest(),
                    requestEvent.getServerTransaction().getDialog());
            responseGenerator.sendResponse(ok,
                requestEvent.getServerTransaction());
        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00251.anExceptionOccuredWhileCreating/SendingAnOKResponseToTheCANCELRequest"), e);
        }

        // @TODO - do we need to send a 487 here?
    }

    /**
     * Handle incoming INVITE Request
     *
     * We first check if the UA has an available session. If not, we respond
     * with a BUSY response. Otherwise, we respond with a 180 Ringing.
     *
     * We also start an InviteResponseTimer. When this timer has run out, we
     * will automatically respond with a 603 DECLINE on behalf of the user.
     *
     * @param requestEvent
     */
    private void handleInviteRequest(RequestEvent requestEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Processing the incoming INVITE request");
        }

        ServerTransaction tx = requestEvent.getServerTransaction();

        if (tx == null) {
            log.log(Level.SEVERE,
                messages.getString("SIPBC-E00252.serverTransactionIsNULL!CreatingANewServerTransaction"));

            try {
                tx = this.sipServer.getSipProvider()
                                   .getNewServerTransaction(requestEvent.getRequest());
            } catch (TransactionAlreadyExistsException e) {
                log.log(Level.WARNING,
                    messages.getString("SIPBC-W00253.anExceptionOccuredWhileCreating/SendingARINGINGResponse"), e);

                return;
            } catch (TransactionUnavailableException e) {
                log.log(Level.WARNING,
                    messages.getString("SIPBC-W00254.anExceptionOccuredWhileCreating/SendingARINGINGResponse"), e);

                return;
            }
        }

        String remoteUri = SipUtil.buildUserAtHost(((FromHeader) requestEvent
                                                    .getRequest()
                                                    .getHeader(FromHeader.NAME)).getAddress()
                                                    .getURI());

        if (user.sessionExists(remoteUri)) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00255.aSessionIsAlreadyEastablishedWithTheRemoteUri",
                remoteUri));

            // @TODO - should we do something here?
        } else {
            try {
                user.addSession(remoteUri, tx);
            } catch (Exception e) {
                log.log(Level.WARNING,
                    messages.getString("SIPBC-W00256.anExceptionOccuredWhileTryingToAddAsessionWithTheRemoteUri",
                    remoteUri),e);

                return;
            }
        }

        // start timer
        startInviteResponseTimer(remoteUri);

        // Send 180 on behalf of user
        Response ringing = null;

        try {
            ringing = responseGenerator.generateRingingResponse(tx.getRequest());
            responseGenerator.sendResponse(ringing, tx);
        } catch (Exception e) {
            log.log(Level.WARNING,
                messages.getString("SIPBC-W00257.anExceptionOccuredWhileCreating/SendingARINGINGResponse"), e);
        }
    }

    /**
     * Hanlde incoming ACK Request. Mark a corresponding pending session as
     * established.
     *
     * @param requestEvent
     */
    private void handleAckRequest(RequestEvent requestEvent) {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Processing the incoming ACK request");
        }

        String remoteUri = SipUtil.buildUserAtHost(((FromHeader) (requestEvent
                                                    .getRequest()
                                                    .getHeader(FromHeader.NAME))).getAddress()
                                                    .getURI());

        if (!user.sessionExists(remoteUri)) {
            log.log(Level.WARNING,messages.getString("SIPBC-W00258.noSessionIsCurrentlyEstablishedWithTheRemoteUri", remoteUri));
        } else {
            SIPSession session = user.getSession(remoteUri);

            if (session.isPending()) {
                session.establish();
            } else {
                log.log(Level.WARNING,messages.getString("SIPBC-W00259.theSessionIsNotInAPendingState"));
            }
        }
    }

    /**
     * Here we spin off a new thread to keep the user registered with the proxy
     * on their behalf.
     *
     * Refactor for scalability?
     *
     * @return
     */
    private ScheduledFuture<?> scheduleAutoRegister() {
        if (log.isLoggable(Level.FINE)){
            log.log(Level.FINE,"Scheduleing the Auto-ReRegister");
        }

        Runnable runner = new Runnable() {
                public void run() {
                    register();
                }
            };

        int period = (int) (REGISTER_DURATION * REREGISTER_RATE);
        final ScheduledFuture<?> future = scheduler.scheduleAtFixedRate(runner,
                period, period, TimeUnit.SECONDS);

        return future;
    }

    public RequestGenerator getRequestGenerator() {
        return this.requestGenerator;
    }

    public ResponseGenerator getResponseGenerator() {
        return this.responseGenerator;
    }

    public SIPServer getSipServer() {
        return sipServer;
    }

    public boolean isRegistered() {
        return registered;
    }

    public SipStack getSipStack() {
        return this.sipStack;
    }

    public void close() {
        SIPObservable.removeObservers(requestGenerator.getUserURI());
    }

    /**
     * Starts the InviteResponseTimer. If the TimerTask is ran before the user
     * responds to the received INVITE reqeust, we will decline with a 603
     * Response on their behalf.
     *
     * @param remoteUri
     */
    private void startInviteResponseTimer(final String remoteUri) {
        TimerTask inviteResponseTimerTask = new TimerTask() {
                public void run() {
                    sendDeclineResponse(remoteUri);
                }
            };

        inviteResponseTimer = new Timer();
        inviteResponseTimer.schedule(inviteResponseTimerTask,
            INVITE_RESPONSE_TIMER);
    }

    /**
     * Called when the SipServer receives a Request/Response for this UA.
     *
     * @param arg
     */
    public void update(Object arg) {
        if (arg instanceof ResponseEvent) {
            ResponseEvent responseEvent = (ResponseEvent) arg;
            String method = ((CSeqHeader) responseEvent.getResponse()
                                                       .getHeader(CSeqHeader.NAME)).getMethod();

            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Received notification of a ResponseEvent. Method=" +
                    method);
            }

            if (method.equals(Request.REGISTER)) {
                handleRegisterResponse(responseEvent);
            } else if (method.equals(Request.MESSAGE)) {
                handleMessageResponse(responseEvent);
            } else if (method.equals(Request.INVITE)) {
                handleInviteResponse(responseEvent);
            } else if (method.equals(Request.BYE)) {
                handleByeResponse(responseEvent);
            } else if (method.equals(Request.CANCEL)) {
                handleCancelResponse(responseEvent);
            }
        } else if (arg instanceof RequestEvent) {
            RequestEvent requestEvent = (RequestEvent) arg;
            if (log.isLoggable(Level.FINER)){
                log.log(Level.FINER,"Received Notification of a RequestEvent");
            }
            String method = requestEvent.getRequest().getMethod();

            if (method.equals(Request.BYE)) {
                handleByeRequest(requestEvent);
            } else if (method.equals(Request.CANCEL)) {
                handleCancelRequest(requestEvent);
            } else if (method.equals(Request.INVITE)) {
                handleInviteRequest(requestEvent);
            } else if (method.equals(Request.ACK)) {
                handleAckRequest(requestEvent);
            }
        }
    }
}
