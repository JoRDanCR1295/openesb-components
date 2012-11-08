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
 * @(#)OtdTrace.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.runtime;

import java.io.PrintStream;

/**
 * The OTD trace interface is meant for the OTD tester to control debugging
 * facilities inside the runtime code used by the OTD, especially unmarshal().
 * The implementation of this interface also functions as a signal to the
 * tester that the implementing OTD is fact has such facilities.
 *
 * The interface needs to be implemented by the instance root node, the same
 * that implements OtdRoot. More fine-grained control should use the same
 * flag system as the check overlay validation runtime parameters.
 *
 * @author Michael Libourel
 * @version 
 */
public interface OtdTrace {
    /**
     * Redirects the debugging trace output to the given stream.
     * Setting the stream to null will switch of debugging.
     *
     * @param out  the output stream
     */
    void setDebugStream (PrintStream out);

    /**
     * Retrieves the debugging trace stream, if any.
     *
     * @return the stream, or null if none
     */
    PrintStream getDebugStream ();

    /**
     * Passes the given magic string to the OTD instance.
     * The semantics for this are completely up to the OTD implementation.
     *
     * @param arg  some string, meaningful to the OTD only
     */
    void command (String arg);
}
