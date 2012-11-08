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
 * @(#)NDC.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.iep.core.runtime.util;

import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Nested Diagnoistic Context
 *
 * @author Bing Lu
 */
public class NDC {
    private static final Messages mMessages = Messages.getMessages(NDC.class);
    private static Logger mEnter = Logger.getLogger("com.sun.EnterContext");
    private static Logger mExit = Logger.getLogger("com.sun.ExitContext");

    private static String buildString(Object[] ctx) {
        StringBuffer buff = new StringBuffer();
        if (ctx == null || ctx.length == 0) {
            throw new IllegalArgumentException(mMessages.getString("Messages.Invalid_nested_diagnostic_context_may_not_be_NULL_or_zero_length"));
        }

        int len = ctx.length;
        if (len == 1) {
            if (ctx[0] == null) {
                throw new IllegalArgumentException(mMessages.getString("Messages.Invalid_nested_diagnostic_context_may_not_be_NULL_or_zero_length"));
            } else {
                buff.append("Context={0}");
            }
        } else if ((len % 2) == 0) {    // even count, key-value pairs
            buff.append("Context: ");
            boolean comma = false;
            // ndc prints out key values in backwards order, so shall we
            for (int i = (len - 1); i >= 0; i -= 2) {
                if (comma) {
                    buff.append(",");
                }
                buff.append("{").append((i - 1)).append("}={").append(i);
                comma = true;
            }
        } else {
            throw new IllegalArgumentException(mMessages.getString("Messages.Invalid_nested_diagnostic_context_must_be_in_key_value_pairs"));
        }

        return buff.toString();
    }

    /**
     * Enters the nested diagnostic context.
     */
    public static void enter(Object... ctx) {    
        String str = buildString(ctx);
        mEnter.log(Level.FINE, str, ctx);
    }

    /**
     * Exits the nested diagnostic context.
     * 
     * @param ctx The name (or name-value pairs) of the diagnostic context.
     */
    public static void exit(Object... ctx) {
        String str = buildString(ctx);
        mExit.log(Level.FINE, str, ctx);
    }
}
