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
 * @(#)DefaultStateImpl.java 
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

/**
 *
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public abstract class DefaultStateImpl implements State {

    // Content for next state
    private String mInvalidMessage = null;

    protected State mParent;
    protected StateFactory mStateFactory;

    public DefaultStateImpl(final StateFactory stateFactory) {
        mStateFactory = stateFactory;
        mParent = null;
    }

    public DefaultStateImpl(final StateFactory stateFactory, final State parent) {
        mStateFactory = stateFactory;
        mParent = parent;
    }

    public boolean performAction(final Writer out) throws Exception {
        if (mInvalidMessage != null) {
            out.write(mInvalidMessage + "\r\n");
            out.flush();
            return true;
        }
        return false;
    }

    public State nextState(final String content) {
        // Next possible states.  There should be a
        // better way of doing this.  I shouldn't have
        // to create all next possible states to parse the 
        // command line.  Maybe I can separate out the
        // parser logic into another interface
        final State[] nextStates = nextStates();
        for (final State element : nextStates) {
            try {
                if (element.parse(content)) {
                    return element;
                }
            } catch (final InvalidParseException ipe) {
                mInvalidMessage = ipe.getMessage();
                return this;
            }
        }
        
        mInvalidMessage = "550 5.5.1 Command Unrecognized: " +
            content;

        return this;
    }

    public State previousState() {
        return mParent;
    }

    /**
     * Returns the array of next possible states.  Sub-classes
     * should override this method to add their own list of states
     *
     * @return       the array of next possible states
     */
    protected State[] nextStates() {
        final State[] nextStates =
            { mStateFactory.createHeloState(this),
              mStateFactory.createEhloState(this),
              mStateFactory.createRsetState(),
              mStateFactory.createVrfyState(this),
              mStateFactory.createNoopState(this),
              mStateFactory.createQuitState(this)
            };

        return nextStates;
    }

}
