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
 * @(#)VrfyState.java 
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
import java.util.Iterator;
import java.util.Set;

/**
 *
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class VrfyState extends DefaultStateImpl {

    // My content
    private String mEmailAddress = "";
    protected Set mEmailListeners;

    public VrfyState(final StateFactory factory, final State parent,
                     final Set emailListeners) {
        super(factory, parent);
        mEmailListeners = emailListeners;
    }

    @Override
	public boolean performAction(final Writer out) throws Exception {
        final StringBuffer responseBuffer = new StringBuffer();
        if (!super.performAction(out)) {
            if (verify(mEmailAddress)){
                responseBuffer.append("252 2.1.5 <")
                    .append(mEmailAddress)
                    .append(">");
                out.write(responseBuffer.toString() + "\r\n");
                out.flush();
            } else {
                responseBuffer.append("551 5.5.1 Mailbox for <")
                    .append(mEmailAddress)
                    .append("> not available");
                out.write(responseBuffer.toString() + "\r\n");
                out.flush();
            }
            return true;
        }
        return false;
    }

    @Override
	public State nextState(final String content) {
        return mParent.nextState(content);
    }

    public boolean parse(final String content) throws InvalidParseException {
        if (content.toUpperCase().startsWith("VRFY")) {
            final int space = content.indexOf(' ');
            if (space == -1) {
                throw new InvalidParseException("501 5.5.2 Argument required");
            }
            mEmailAddress = content.substring(space + 1,
                                              content.length());
            return true;
        }

        return false;
    }

    protected boolean verify (final String emailAddress) {
        final Iterator it = mEmailListeners.iterator();
        while (it.hasNext()) {
            final EmailListener listener = (EmailListener)it.next();
            final AddressBook addressBook = listener.getAddressBook();
            if (addressBook.hasEmailAddress(emailAddress)) {
                return true;
            }
        }
        return false;
    }
}
