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

package com.sun.jbi.imsbc.ims;

import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.imsbc.IMSException;

import com.sun.jbi.internationalization.Messages;

public class IMSClientRequestSynchronizer
{
    private boolean isAvail;     // Lock has been acquired by some thread
    private int nWaiting;        // Number of waiting threads for lock

    private static final Messages mMessages = Messages.getMessages(IMSClientRequestSynchronizer.class);

    private static final Logger mLogger = Messages.getLogger(IMSClientRequestSynchronizer.class);
    
    /**
     * Default constructor
     */
    public IMSClientRequestSynchronizer() {
        isAvail = true;
        nWaiting = 0;
    }

    /**
     * Try to acquire a lock if lock is not available then wait for it.
     *
     * @param waitTimeout long Number of msecs to wait for the lock if it's not
     *                         available.
     *
     * @preturn If lock is acquire then true is returned false if timed out.
     *
     * @throws Exception If failed to wait for the lock.
     */
    public boolean acquire(long waitTimeout) throws Exception {
        boolean acquired = false;

        synchronized (this) {
            if (isAvail) {
                isAvail = false;
                acquired = true;
            }
            else { // wait for availability
                try {
                    nWaiting++;
                    long startWait = System.currentTimeMillis();
                    wait(waitTimeout);
                    long endWait = System.currentTimeMillis();
                    if (isAvail) {
                        isAvail = false;
                        acquired = true;
                    }
                }
                catch (Throwable t) {
                	String errMsg = mMessages.getString("IMSBC-E00816.Wait_Lock_Failed", new Object[] { t.getLocalizedMessage()});
                	mLogger.log(Level.INFO, errMsg);
                	throw new IMSException(errMsg);                 	
                }
                finally {
                    nWaiting--;
                }
            }
        }

        return acquired;
    }

    /**
     * Release acquired lock from previous call to acquire.
     * Also, notify any possible waiting thread that is waiting for the lock.
     *
     */
    public void release() {
        synchronized (this) {
            isAvail = true;
            if (nWaiting > 0) {
                notify(); // wake up one of the waiting threads
            }
        }
    }

    /**
     * Determines whether there are any thread(s) waiting for the lock.
     *
     * @return If there are any waiting threads true will be returned; false
     *         is returned otherwise.
     */
    public boolean isWaiting() {
        synchronized (this) {
            return (nWaiting > 0);
        }
    }
}
