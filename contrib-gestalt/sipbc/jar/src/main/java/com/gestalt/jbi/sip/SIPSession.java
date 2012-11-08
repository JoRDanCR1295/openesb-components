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

import java.util.logging.Logger;
import java.util.logging.Level;

import javax.sip.Transaction;


/**
 * This class is used to track the state and properties of a Session.
 */
public final class SIPSession {
    private static final Logger log = Messages.getLogger(SIPSession.class);
    private static Messages messages = Messages.getMessages(SIPSession.class);
    private String remoteUri = "";
    private Transaction tx = null;
    private SessionStatus status = null;

    public SIPSession(String remoteUri, Transaction tx) {
        this.init(remoteUri, tx);
    }

    public Transaction getTx() {
        return tx;
    }

    public void setTx(Transaction tx) {
        this.tx = tx;
    }

    public Boolean isEstablished() {
        return (this.status.equals(SessionStatus.establsihed));
    }

    public Boolean isPending() {
        return (this.status.equals(SessionStatus.pending));
    }

    public String getRemoteUri() {
        return this.remoteUri;
    }

    /**
     * Init the session.
     *
     * @param targetUri
     * @param tx
     */
    private void init(String targetUri, Transaction tx) {

        if (log.isLoggable(Level.FINER)){
            log.log(Level.FINER,"The session was initiated with the RemoteUri: " + targetUri);
        }
        this.remoteUri = targetUri;
        this.tx = tx;
        this.status = SessionStatus.pending;
    }

    /**
     * Establish the session.
     */
    public void establish() {
        if (this.status.equals(SessionStatus.pending)) {
            this.status = SessionStatus.establsihed;
        } else {
            log.log(Level.WARNING,messages.getString("SIPBC-W00266.cannotUdpateTheSessionToEstablishedBecauseThereIsNotAPendingSession"));
        }
    }

    /**
     * This enum defines the state of the session.
     */
    private static enum SessionStatus {pending,
        establsihed;
    }
}
