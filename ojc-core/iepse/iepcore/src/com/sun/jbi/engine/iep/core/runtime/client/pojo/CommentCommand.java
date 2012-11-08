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
 * @(#)CommentCommand.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.iep.core.runtime.client.pojo;

import com.sun.jbi.engine.iep.core.runtime.util.Messages;
import java.util.Map;
import java.util.logging.Level;

/**
 * CommentCommand.java
 *
 * Created on September 8, 2005, 1:28 AM
 *
 * @author Bing Lu
 */
public class CommentCommand implements Command {
    private static final Messages mMessages = Messages.getMessages(CommentCommand.class);
    
    /** Creates a new instance of CommentCommand */
    public CommentCommand() {
    }
    
    public void init(NWaySheperd sheperd, String props) {
    }
    
    public Runnable createRunnable(Map<String, Input> inputTable, final String[] args) {
        return new Runnable() {
            public void run() {
                if (mMessages.isLoggable(Level.FINE)) {
                    StringBuffer sb = new StringBuffer();
                    for (int i = 0; i < args.length; i++) {
                        if (i > 0) {
                            sb.append(" ");
                        }
                        sb.append(args[i]);
                    }
                    mMessages.logOriginal(Level.FINE, sb.toString());
                }
            }
        };
    }

    public void destroy() {
    }
}
