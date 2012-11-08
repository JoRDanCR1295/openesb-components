/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)DataEndState.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/***************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc.extservice.server;

import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

/**
 *
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class DataEndState extends DefaultStateImpl {

    private Set mEmailListeners;

    public DataEndState(final StateFactory factory, final State parent,
                        final Set emailListeners) {
        super(factory, parent);
        mEmailListeners = emailListeners;
    }

    @Override
	public boolean performAction(final Writer out) throws Exception {
        if (!super.performAction(out)) {

            // No errors.  Send the email out to all our listeners.
            // In general, we will wait for a response from all listeners
            // before writing back a response to the client.
            if (mEmailListeners.size() != 0) {

                // Find our message
                // Convert the State object into an MimeMessage
                String content = null;
                String address = null;
                State parent = null;
                State state = this;
                do {
                    parent = state.previousState();
                    if (parent instanceof DataReadState) {
                        content = ((DataReadState)parent).getContent();
                    } else if (parent instanceof RcptState) {
                        address = ((RcptState)parent).getEmailAddress();
                    }
                    state = parent;
                } while (parent != null);

                final byte[] buffer = content.getBytes("US-ASCII");

                // Notify our listeners that have the proper address
                final Iterator it = mEmailListeners.iterator();
                while (it.hasNext()) {
                    final EmailListener listener = (EmailListener)it.next();
                    if (listener.getAddressBook().hasEmailAddress(address)) {
                        final DefaultEmailCallback callback =
                            new DefaultEmailCallback(out, 1);
                        listener.onMessage(buffer, callback);
                    }
                }

            } else {
                writeSuccessfulResponse(out);
            }
            return true;
        }
        return false;
    }

    public boolean parse(final String content) throws InvalidParseException {
        if (content.equals(".")) {
            return true;
        }
        return false;
    }

    /**
     * Returns the array of next possible states.  Sub-classes
     * should override this method to add their own list of states
     *
     * @return       the array of next possible states
     */
    @Override
	protected State[] nextStates() {
        final List nextStates = new ArrayList();
        nextStates.add(mStateFactory.createMailState(this));
        nextStates.addAll(Arrays.asList(super.nextStates()));
        return (State[])nextStates.toArray(new State[0]);
    }

    protected void writeSuccessfulResponse(final Writer out) throws Exception {
        out.write("250 2.0.0 Message accepted for delivery\r\n");
        out.flush();
    }

    public class DefaultEmailCallback implements EmailCallback {

        private Writer mOut;
        private Object mLock;
        private int mNumListeners;
        private int mNumListenersResponded = 0;;

        public DefaultEmailCallback(final Writer out,
                                    final int numListeners) {
            mOut = out;
            mLock = new Object();
            mNumListeners = numListeners;
        }
        
        public void emailProcessed() {
            synchronized (mLock) {
                try {
                    mNumListenersResponded++;
                    if (mNumListenersResponded == mNumListeners) {
                        writeSuccessfulResponse(mOut);
                    }
                } catch (final Exception ex) {
                    ex.printStackTrace();
                }
            }
        }
    }

}
