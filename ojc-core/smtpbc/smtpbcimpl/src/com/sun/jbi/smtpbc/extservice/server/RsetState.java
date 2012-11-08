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
 * @(#)RsetState.java 
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
public class RsetState extends DefaultStateImpl {

    public RsetState(final StateFactory factory) {
        super(factory);
    }

    @Override
	public boolean performAction(final Writer out) throws Exception {
        if (!super.performAction(out)) {
            out.write("250 2.0.0 Reset State\r\n");
            out.flush();
            return true;
        }
        return false;
    }

    public boolean parse(final String content) throws InvalidParseException {
        if (content.toUpperCase().startsWith("RSET")) {
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
}
