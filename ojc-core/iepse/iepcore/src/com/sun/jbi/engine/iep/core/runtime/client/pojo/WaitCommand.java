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
 * @(#)WaitCommand.java 
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
 * WaitCommand.java
 *
 * Created on September 8, 2005, 1:28 AM
 *
 * @author Bing Lu
 */
public class WaitCommand implements Command {
    private static final Messages mMessages = Messages.getMessages(UserCommand.class);
    
    /** Creates a new instance of WaitCommand */
    public WaitCommand() {
    }
    
    public void init(NWaySheperd sheperd, String props) {
    }
    
    public Runnable createRunnable(Map<String, Input> inputTable, final String[] args) {
        return new Runnable() {
            public synchronized void run() {
                long miliSec = 1000L;
                try {
                     miliSec = 1000*Integer.parseInt(args[1]);
                } catch (Exception e) {
                    mMessages.log(Level.SEVERE, "WaitCommand.run_fails", e);
                }
                try {
                    if (mMessages.isLoggable(Level.FINE)) {
                        mMessages.log(Level.FINE, "WaitCommand.wait_for_seconds", args[1]);
                    }    
                    wait(miliSec);
                } catch (Exception e) {
                    mMessages.log(Level.SEVERE, "WaitCommand.run_fails", e);
                }
            }
        };
    }

    public void destroy() {
    }
}
