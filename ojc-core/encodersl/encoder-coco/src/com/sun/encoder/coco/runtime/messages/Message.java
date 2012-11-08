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
 * @(#)Message.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.runtime.messages;

public interface Message
{
    /**
     * Unique identification for a Message.
     */ 
    Object id();
    
    /**
     * Textual content of the Message. Implementations may differentiate the
     * content returned by this method and {@link Object#toString} with the
     * intention that this method returns a textual representation that is more
     * concise than Object.toString().
     */ 
    String text();
    
    /**
     * Treat the text of the Message (as produced by {@link #text}) as a
     * pattern, formatting the specified arguments against it. The pattern
     * system is implementation defined.  The result of the call is an
     * instantiation of the pattern without altering the state of the Message
     * implementation.
     *
     * @param args Arguments to apply to the Message's pattern
     *
     * @return An instantiation of the pattern with the given arguments
     *         applied.
     */ 
    String formatText(Object[] args);
}

// FINIS $RCSfile: Message.java,v $
