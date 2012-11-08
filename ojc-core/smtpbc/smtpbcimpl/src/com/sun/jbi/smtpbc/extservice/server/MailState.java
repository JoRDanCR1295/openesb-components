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
 * @(#)MailState.java 
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
import java.util.List;

/**
 *
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class MailState extends DefaultStateImpl {

    // My content
    private String mEmailAddress = "";

    public MailState(final StateFactory factory, final State parent) {
        super(factory, parent);
    }

    @Override
	public boolean performAction(final Writer out) throws Exception {
        final StringBuffer responseBuffer = new StringBuffer();
        if (!super.performAction(out)) {
            responseBuffer.append("250 2.1.0 <")
                .append(mEmailAddress)
                .append(">... Sender ok");
            out.write(responseBuffer.toString() + "\r\n");
            out.flush();
            return true;
        }
        return false;
    }


    public boolean parse(final String content) throws InvalidParseException  {

        final String normalizedContent = content.toUpperCase();
        if (normalizedContent.startsWith("MAIL")) {

            final String parameters = content.substring(4, content.length());

            // First check the parameters
            final int space = normalizedContent.indexOf(" FROM:<");
            if (space == -1) {
                throw new
                    InvalidParseException("550 5.5.1 Command unrecognized: " +
                                          parameters);
            }
            final int endEmailAddress =
                normalizedContent.indexOf('>', space + " FROM:<".length());
            if (endEmailAddress == -1) {
                throw new
                    InvalidParseException("550 5.5.1 Command unrecognized: " +
                                          parameters);
            }

            // Check if we've already issued the MAIL command once before
            State parent = null;
            State state = this;
            do {
                parent = state.previousState();
                if (parent instanceof MailState) {
                    throw new 
                        InvalidParseException("503 5.5.0 Sender already specified");
                }
                state = parent;
            } while (parent != null);

            // If we got here, everything looks good.
            mEmailAddress = content.substring(space + " FROM:<".length(),
                                              endEmailAddress);
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
        nextStates.add(mStateFactory.createRcptState(this));
        nextStates.addAll(Arrays.asList(super.nextStates()));
        return (State[])nextStates.toArray(new State[0]);
    }
}
