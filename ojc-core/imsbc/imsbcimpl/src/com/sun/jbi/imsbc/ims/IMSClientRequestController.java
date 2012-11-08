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

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.imsbc.IMSException;

import com.sun.jbi.internationalization.Messages;

public class IMSClientRequestController {

    private static Map clientIDLocks =
                       Collections.synchronizedMap(new HashMap());
    
    private static final Messages mMessages = Messages.getMessages(IMSClientRequestController.class);

    private static final Logger mLogger = Messages.getLogger(IMSClientRequestController.class);

    /**
     * Acquires lock before processing IMS request using the supplied client ID.
     * If a lock is not already created, then create the lock, acquire the lock,
     * and register the lock with the client ID.
     *
     * @param clientID String  The IMS client ID to use to create and/or acquire
     *                         the lock.  This is so that the same client ID
     *                         used by multiple connects for multiple requests
     *                         can be serialized.
     *
     * @param waitTimeout long The timeout in milliseconds to wait for the lock
     *                         if the lock was already obtained by some other
     *                         thread.
     *
     * @preturn If lock is acquire then true is returned false if timed out.
     *
     * @throws Exception If failed to wait for the lock.
     */
    public static boolean acquireLock (String clientID, long waitTimeout)
    throws Exception {
        boolean acquired = false;

        IMSClientRequestSynchronizer lock = null;

        synchronized (clientIDLocks) {
            if (!clientIDLocks.containsKey(clientID)) {
                lock = new IMSClientRequestSynchronizer();
                clientIDLocks.put(clientID, lock);
            } else {
                lock = (IMSClientRequestSynchronizer)
                       clientIDLocks.get(clientID);
            }
        }

        if (lock != null) {
            acquired = lock.acquire(waitTimeout);
        } else {
        	throw new IMSException(mMessages.getString("IMSBC-E00814.Acquire_Lock_Failed"));        	
        }

        return acquired;
    }

    /**
     * Release acquired lock from previous call to acquireLock to acquire the
     * lock for the supplied client ID.Also, notify any possible waiting thread
     * that is waiting for the lock on that client ID.
     *
     * @param clientID String The client ID lock to release.
     *
     * @throws Exception If no lock is has been created for the client ID.
     */
    public static void releaseLock (String clientID)
    throws Exception {
        IMSClientRequestSynchronizer lock = null;

        synchronized (clientIDLocks) {
            if (clientIDLocks.containsKey(clientID)) {
                lock = (IMSClientRequestSynchronizer)
                        clientIDLocks.get(clientID);
            } else {
            	String errMsg = mMessages.getString("IMSBC-E00815.Locate_Lock_Failed", new Object[] { clientID});
				if (mLogger.isLoggable(Level.INFO))
	            	mLogger.log(Level.INFO, errMsg);
            	throw new IMSException(errMsg);            	
            }
        }

        if (lock != null) {
            // "unregister" this lock if it's not being waited on by other
            // threads
            if (!lock.isWaiting()) {
                synchronized (clientIDLocks) {
                    clientIDLocks.remove(clientID);
                }
            }

            lock.release();
        }
    }

}

