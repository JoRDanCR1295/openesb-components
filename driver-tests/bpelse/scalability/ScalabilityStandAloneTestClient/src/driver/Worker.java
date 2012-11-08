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
 * @(#)EngineChannel.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package driver;

/**
 * @author mbhasin
 *
 */
class Worker implements Runnable {
    
    private MessageSender sender = null;
    int threadId = 1;
    
    /**
     * @param driver
     * @param iterations
     */
    public Worker(MessageSender driver, int threadId) {
        this.sender = driver;
        this.threadId = threadId;
    }
    
    public void run() {
        int i = 0;
        try {
            sender.runTest(threadId);
        } catch (Exception e) {
            System.out.println("Ran into exceptions at count " + i);
            e.printStackTrace();
        }
    }
}
