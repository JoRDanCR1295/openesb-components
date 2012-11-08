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
 * @(#)DebuggerServerRunnerContinousSend.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.bpeldebugger;

import java.util.Timer;
import java.util.TimerTask;

public class DebuggerServerRunnerContinousSend {
	static DebuggerEngine debuggerServer;
	
	static class MessageSender extends TimerTask {

		public void run() {
			// TODO Auto-generated method stub
            new Thread () {
                public void run() {
                    debuggerServer.sendMessage();
                };                
            }.start();		
		}
    }

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		
		debuggerServer = new DebuggerEngine();		
		new Thread(debuggerServer).run();
		//Schedule the message to be sent every 50 seconds, 2 seconds from now
		TimerTask msgSender = new MessageSender ();
		new Timer ().schedule(msgSender, 2000, 50000);
		
		while (true) {
			try {
				Thread.sleep(1000000000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

}
