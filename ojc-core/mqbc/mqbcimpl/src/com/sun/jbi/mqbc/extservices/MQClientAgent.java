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
 * @(#)MQClientAgent.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;


import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;

import com.ibm.mq.MQC;
import com.ibm.mq.MQEnvironment;
import com.ibm.mq.MQException;
import com.ibm.mq.MQQueue;
import com.ibm.mq.MQQueueManager;
import com.ibm.mq.MQSecurityExit;
import com.ibm.mq.MQXAQueueManager;
import com.ibm.mq.MQXAResource;
import com.ibm.mqservices.Trace;
import com.sun.jbi.mqbc.I18n;

/**
 * Facade for connecting to MQ, and sending/receiving messages from MQ.
 * 
 * @author Noel.Ang@sun.com
 */
public class MQClientAgent {
    public static final int MQ_DEFAULT_PORT = 1414;

    private final Logger mLog = Logger.getLogger(getClass().getName());
    private final MQClientConfiguration configuration;
    private final Map<String, MQQueue> openQueues;
    private MQXAQueueManager xaQueuemanager;
    private MQQueueManager queuemanager;
    private MQQueue currentQueue;
    private final boolean isXaEnabled;
    private final boolean isReceiveMode;
    private volatile boolean isInvalid;
    private final boolean requireSFIPS;
    private final String hostname;
    private final int port;
    private String queuemanagerName;
    private String currentQueueName;
    private final String channelName;
    private final String userName;
    private final String password;
    private final String cipherSuite;
    private final String sslPeerName;
    private final Map<MQXAResource, XAResource> seenXAResources;
    private volatile boolean useXAWrapper = false;
    
    public MQClientAgent(MQClientConfiguration config)
            throws UnknownCipherSuiteException {
        seenXAResources = new HashMap<MQXAResource, XAResource>();
        configuration = config;
        openQueues = new HashMap<String, MQQueue>(); // must use map whose entrySet iterator supports removal
        
        hostname = configuration.getHost();
        port = (configuration.getPort().intValue() > 0
                ? configuration.getPort().intValue() : MQ_DEFAULT_PORT);
        queuemanagerName = configuration.getQueueManagerName().trim();
        currentQueueName = configuration.getQueueName().trim();
        channelName  = configuration.getChannelName().trim();
        userName = configuration.getUser().trim();
        password = configuration.getPassword();
        cipherSuite = configuration.getCipherSuite().trim();
        sslPeerName = configuration.getSslPeerName().trim();
        isReceiveMode = configuration.getReceiveMode();
        isXaEnabled = configuration.getXAMode();

        // Match the CipherSuite with a known MQ CipherSpec.
        if (cipherSuite.equals("")) {
            requireSFIPS = false;
        } else {
            String cipherSpec;
            requireSFIPS = (((cipherSpec = findCipherSpec(cipherSuite)) == null)
                    && ((cipherSpec = findSFIPSCipherSpec(cipherSuite)) != null));
            if (cipherSpec == null) {
                throw new UnknownCipherSuiteException(I18n.msg(
                        "1940: The specified cipher suite {0} is not one" 
                                + " supported by WebSphere MQ.",
                        cipherSuite));
            } else {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(I18n.msg(
                            "CipherSuite {0} matched with MQ CipherSpec {1}.",
                            cipherSuite,
                            cipherSpec));
                }
            }
        }

        try {
            MQException.logExclude(MQException.MQRC_NO_MSG_AVAILABLE);
        } catch (Exception e) {
            ;
        }
        
        // Match MQ API internal trace level with logger's.
        if (mLog.isLoggable(Level.FINE)) {
            Trace.turnTracingOn();
        } else if (mLog.isLoggable(Level.FINER)) {
            Trace.turnTracingOn();
            Trace.turnMethodTracingOn();
        } else {
            Trace.turnTracingOff();
            Trace.turnMethodTracingOff();
        }
        
        mLog.fine(I18n.msg("Connection parameters:\nHost: {0}\nPort: {1}"
                + "\nQueue Manager: {2}\nQueue: {3}\nChannel: {4}"
                + "\nUser: {5}\nTransactional: {6}"
                + "\nCipher Suite: {7}"
                + "\nSSL Peer Name: {8}",
                hostname,
                port,
                queuemanagerName, currentQueueName,
                channelName,
                userName,
                isXaEnabled,
                cipherSuite,
                sslPeerName));
    }
    
    /**
     * Establsh this MQClientAgent's connection to MQ.
     *
     * @throws MQBCExtServiceException        if any error prevents successful
     *                                        connection.
     */
    public void connect() throws MQBCExtServiceException {
        if (isInvalid) {
            throw new MQBCExtServiceException(I18n.msg(
                    "1900: MQ Client no longer usable - invalidated"));
        }

        if (empty(queuemanagerName)) {
            throw new MQBCExtServiceException(I18n.msg(
                    "1901: Failed to connect - queue manager unspecified"));
        }

        _connect();
    }

    /**
     * Call this to tell the current queue manager that all gets and puts since
     * the last syncpoint are to be backed out. As in commit, this should NOT be
     * called in XA mode since in XA mode eGate is acting as the transaction
     * coordinator. An attempt to call it in XA mode will cause an exception to
     * be thrown.
     * 
     * @throws MQException
     * @throws IllegalStateException
     */
    public void backout() throws IllegalStateException,
                                 MQException {
        
        synchronized(this) {
            if (!isConnected()) {
                throw new IllegalStateException(I18n.msg(
                        "1900: MQ Client no longer usable - invalidated"));
            }
        }
        
        if (isXaEnabled) {
            throw new IllegalStateException(I18n.msg(
                    "1902: Syncpoint backout not applicable -"
                            + " Transactions are in use"));
        }

        try {
            queuemanager.backout();
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg(
                        "Syncpoint backout executed for queue manager {0}",
                        queuemanager.name));
            }
        }
        catch (MQException mqx) {
            mLog.severe(I18n.msg(
                    "1903: Failed syncpoint backout for queue manager {0},"
                            + " reason code {1}: {2}",
                    queuemanager.name,
                    String.valueOf(mqx.reasonCode),
                    MQXDecode.decode(mqx.reasonCode)));
            throw mqx;
        }
    }
    
    /**
     * Call this to tell the current queue manager that your collaboration has
     * reached a syncpoint and that all gets and puts that have occurred since
     * the last syncpoint are to be made permanent. Note that this should NOT be
     * called in XA compliant mode. (In XA mode, eGate is acting as the
     * transaction coordinator). If this is called in XA mode it will throw an
     * exception.
     * 
     * @throws IllegalStateException
     * @throws MQException
     */
    public void commit() throws IllegalStateException,
                                MQException {
        synchronized(this) {
            if (!isConnected()) {
                throw new IllegalStateException(I18n.msg(
                        "1900: MQ Client no longer usable - invalidated"));
            }
        }
        
        if (isXaEnabled) {
            throw new IllegalStateException(I18n.msg("1904: Syncpoint commit not applicable" 
                    + " - Transactions in use"));
        }

        try {
            queuemanager.commit();
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg(
                        "Syncpoint commit executed for queue manager {0}",
                        queuemanager.name));
            }
        }
        catch (MQException mqx) {
            mLog.severe(I18n.msg("1905: Failed syncpoint commit"
                    + " for queue manager {0}, reason {1}: {2}",
                    queuemanager.name,
                    String.valueOf(mqx.reasonCode),
                    MQXDecode.decode(mqx.reasonCode)));
            throw mqx;
        }
    }
    
    /**
     * Access a queue on a queue manager.
     *
     * @param queueName Name of queue to open.
     * @param options   options to control the opening of the queue. Not used if
     *                  the queue is already opened.
     * @param qmgrName  Name of queue manager that owns the named queue. May be
     *                  <code>null</code> to use the agent's current queue
     *                  manager.
     *
     * @throws Exception
     */
    public void accessQueue(String queueName, int options, String qmgrName)
            throws Exception {

        MQQueue oldQueue;
        MQQueue newQueue;
        
        if (isInvalid) {
            throw new IllegalStateException(I18n.msg(
                    "1900: MQ Client no longer usable - invalidated"));
        }

        if (null == queueName || "".equals(queueName)) {
            throw new IllegalArgumentException(I18n.msg(
                    "1906: Cannot access queue - queue name not specified"));
        }

        // MQOO_INQUIRE option is needed by connection check mechanism
        // but only if local queue (queue manager unspecified)
        if (null == qmgrName) {
            options |= MQC.MQOO_INQUIRE;
        }

        // Check cache in case this is a previously opened queue
        // Cached queue instance acceptable only if opened with the same option
        queueName = queueName.trim();
        oldQueue = selectQueue(queueName);
        newQueue = null;
        final boolean foundOldQueue = (oldQueue != null);
        final boolean reopenQueue = foundOldQueue && (oldQueue.openOptions != options);
        if (foundOldQueue) {
            if (!reopenQueue) {
                // queue handle is reusable
                newQueue = oldQueue;
                if (mLog.isLoggable(Level.FINER)) {
                    mLog.finer(I18n.msg(
                            "Found an existing handle to queue {0}"
                                    + " that can be reused"
                                    + " (options: {1})",
                            queueName,
                            String.valueOf(oldQueue.openOptions)));
                }
                return;
            }
        }
        
        if (foundOldQueue) {
            if (mLog.isLoggable(Level.FINER)) {
                mLog.fine(I18n.msg(
                        "Found an existing handle to queue {0}"
                                + " but with different options"
                                + " (old: {1}, new: {2})"
                                + " - will close handle and opening a new one.",
                        queueName,
                        String.valueOf(oldQueue.openOptions),
                        String.valueOf(options)));
            }
        }
        
        // Sometimes accessQueue will thrown an exception even though it worked
        // That's what the WSMQ docs say; look up reason code 2009 in
        // document [amqzao03].
        //
        // The corrective action advised is to repeatedly attempt to reconnect
        // until it succeeds. The following loop is for that purpose.
        final int maxConnectionRetries = 10;
        int connectionRetry;
        MQException lastException;
        
        synchronized (this) {
        
        for (connectionRetry = 0, lastException = null;
             newQueue == null && (connectionRetry <= maxConnectionRetries);
             connectionRetry++) {

            // Try opening the requested queue. If opening fails, revert state.
            try {
                if (null == qmgrName) {
                    newQueue = queuemanager.accessQueue(queueName, options);
                } else {
                    try {
                        newQueue = queuemanager.accessQueue(queueName,
                                options | MQC.MQOO_INQUIRE,
                                qmgrName,
                                null, // TODO: requires change when/if model queues are supported
                                null); // TODO: requires change when/if MQOO_ALTERNATE_USER_AUTHORITY supported
                    }
                    catch (MQException e) {
                        // When qmgrName is specified, it may be a queue manager alias,
                        // which means the accompanying queue name may refer to a
                        // remote queue with no local definition.
                        // In this case, the MQOO_INQUIRE option is unsupported;
                        // retry without it.
                        newQueue = queuemanager.accessQueue(queueName,
                                options,
                                qmgrName,
                                null, // TODO: requires change when/if model queues are supported
                                null); // TODO: requires change when/if MQOO_ALTERNATE_USER_AUTHORITY supported
                    }
                }

                // Reaching this point means opening the queue worked.
                // Close the previous queue, then make new queue the current one.
                if (oldQueue != null) {
                    closeQueue(oldQueue);
                }
                currentQueue = newQueue;
                currentQueueName = queueName;
                openQueues.put(queueName, newQueue);

                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(I18n.msg("Connected to {0}", currentQueueName));
                }
            }
            catch (MQException e) {
                // Getting here means I failed to open the new queue
                lastException = e;
                mLog.severe(I18n.msg(
                        "1908: Failed to connect to queue {0}, reason {1}: {2}",
                        queueName,
                        String.valueOf(e.reasonCode),
                        MQXDecode.decode(e.reasonCode)));
                // Post an alert for the failed access attempt.
                postConnected(false, queuemanagerName,
                              queueName,
                              configuration.getChannelName(),
                              configuration.getHost(),
                              configuration.getPort().intValue(),
                              configuration.getUser(),
                              MQXDecode.decode(e.reasonCode));

                // When working with XA, we are not going to try to
                // reconnect; we are going to pass the error to the user
                // and let whatever rollback mechanism is in place to do its job
                if (isXaEnabled) {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.fine("In transaction mode, not going to retry" 
                                + " to connect, letting rollback take place.");
                    }
                    break;
                }

                // If we are going to retry to reconnect to the queue manager
                // post a notification to that effect and do it.
                int retry = connectionRetry + 1;
                if (retry < maxConnectionRetries) {
                    if (mLog.isLoggable(Level.FINE)) {
                        mLog.fine(I18n.msg("Client to qm {0}, queue {1}"
                                + " re-attempting connection"
                                + " (retry {2} of {3})...",
                                qmgrName,
                                queueName,
                                retry,
                                maxConnectionRetries));
                    }
                }

                disconnect();
                connect();
            }

        }
        }

        if (lastException != null && isXaEnabled) {
            // Getting here means I failed to open the new queue
            mLog.severe(I18n.msg(
                    "1908: Failed to connect to queue {0}, reason {1}: {2}",
                    queueName,
                    String.valueOf(lastException.reasonCode),
                    MQXDecode.decode(lastException.reasonCode)));
            // Post an alert for the failed access attempt.
            postConnected(false, queuemanagerName,
                          queueName,
                          configuration.getChannelName(),
                          configuration.getHost(),
                          configuration.getPort().intValue(),
                          configuration.getUser(),
                          MQXDecode.decode(lastException.reasonCode));
            // But post a notification that the current queue connection is OK.
            postConnected(true, queuemanagerName, currentQueueName,
                          configuration.getChannelName(),
                          configuration.getHost(),
                          configuration.getPort().intValue(),
                          configuration.getUser(),
                          null);
            throw lastException;
        }
    }
    
    private void closeQueue(MQQueue queue) {
        String queueName = queue.name.trim();
        synchronized (this) {
        try {
            queue.close();
            if (mLog.isLoggable(Level.FINER)) {
                mLog.fine(I18n.msg("Closed handle to queue {0}", queueName));
            }
        } catch (MQException e) {
            mLog.severe(I18n.msg("1907: Failed to close access to queue {0}"
                    + " prior to reopening with new options."
                    + " Reason {1}: {2}.",
                    queueName,
                    String.valueOf(e.reasonCode),
                    MQXDecode.decode(e.reasonCode)));
        } finally {
            openQueues.remove(queueName);
        }
        }
    }

    /**
     * Select one of the previously accessed queues and make it the current
     * one.
     *
     * @param queueName Name of queue to select.
     *
     * @return The selected/current queue, or null if the named queue has
     * not yet been accessed.
     */
    public MQQueue selectQueue(String queueName) {

        if (isInvalid) {
            throw new IllegalStateException(I18n.msg(
                    "1900: MQ Client no longer usable - invalidated"));
        }

        if (null == queueName) {
            throw new IllegalArgumentException(I18n.msg(
                    "1909: Cannot select queue - queue name not specified"));
        }
        
        queueName = queueName.trim();
        
        synchronized (this) {
       
        // If requested queue is on tap, I'm done.
        if (null != currentQueueName) {
            if (currentQueueName.equals(queueName)) {
                return currentQueue;
            }
        }

        // If requested queue is in our cache, switch to it
        MQQueue queue = openQueues.get(queueName);
        if (null == queue) {
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg(
                        "No match found for queue name {0} in handles map",
                        queueName));
                if (mLog.isLoggable(Level.FINER)) {
                    if (openQueues.size() == 0) {
                        mLog.finer(I18n.msg("No open queue handles"));
                    }
                    else {
                        Set queuenames = openQueues.keySet();
                        for (Object name : queuenames) {
                            mLog.finer(I18n.msg("Map keys: {0}",
                                    name.toString()));
                        }
                    }
                }
            }
        }
        
        if (queue != null) {
            currentQueueName = queueName;
            currentQueue = queue;
        }
        
        return queue;
        }
    }

    /**
     * Close this MQClientAgent's connection to MQ.
     */
    private synchronized void disconnect() {

        if (isInvalid) {
            return;
        }

        if (null == queuemanager) {
            mLog.warning(I18n.msg("1911: Client cannot disconnect - not connected"));
            return;
        }

        MQQueue aQueue;
        if (openQueues.size() > 0) {
            Set queues = openQueues.entrySet();
            for (Object queue : queues) {
                Map.Entry entry = (Map.Entry) queue;
                Object name = entry.getKey();
                aQueue = (MQQueue) entry.getValue();
                try {
                    aQueue.close();
                } catch (MQException e) {
                    mLog.severe(I18n.msg(
                            "1912: Failed to close queue connection to {0}, reason {1}: {2}",
                            name.toString(),
                            e.reasonCode,
                            MQXDecode.decode(e.reasonCode)));
                }
            }
        }
       
        if (isXaEnabled) {
            if (null != xaQueuemanager) {
                xaQueuemanager.close(); // will do qmanager and queue cleanup
                xaQueuemanager = null;
            }
            queuemanager = null;
            currentQueue = null;
            openQueues.clear();
            return;
        }

        try {
            xaQueuemanager = null;
            queuemanager.disconnect(); // will do queue cleanup
        }
        catch (MQException e) {
            mLog.severe(I18n.msg(
                    "1913: Failed to disconnect from queue manager {0}, reason {1}: {2}",
                    queuemanagerName,
                    String.valueOf(e.reasonCode),
                    MQXDecode.decode(e.reasonCode)));
        }
        finally {
            queuemanager = null;
            currentQueue = null;
            openQueues.clear();
        }

        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine(I18n.msg("1914: Disconnected client from {0}",
                    queuemanagerName));
        }
    }
    
    public MQClientConfiguration getConfiguration() {
        // I kept a copy of the original MQClientConfiguration object
        // used to construct this agent, so I will use that as a base.
        MQClientConfiguration cfg = configuration.clone();
        
        // But there are parameters specified by the configuration object
        // that I have validated and possibly normalized or otherwise modified
        // (e.g., queue manager port), and also parameters that naturally
        // change during the lifetime of this agent (e.g., queue name), so I
        // update those values.
        cfg.setChannelName(this.channelName);
        cfg.setCipherSuite(this.cipherSuite);
        cfg.setHost(this.hostname);
        cfg.setPassword(this.password);
        cfg.setPort(this.port);
        cfg.setQueueManagerName(this.queuemanagerName);
        cfg.setQueueName(this.currentQueueName);
        cfg.setReceiveMode(this.isReceiveMode);
        cfg.setSslPeerName(this.sslPeerName);
        cfg.setUser(this.userName);
        cfg.setXAMode(this.isXaEnabled);
        return cfg;
    }

    /**
     * Determine whether or not this MQClientAgent is connected.  Being
     * connected means: if it's a receiving MQClientAgent, it is connected
     * to its queue manager and queue; if it's a sending MQClientAgent, it
     * is connected to its queue manager and, optionally, to a queue.
     *
     * @return <code>true</code> if this MQClientAgent is connected.
     */
    public boolean isConnected() {
        MQQueueManager qmgr = null;

        if (isInvalid) {
            return false;
        }

        if (isXaEnabled) {
            if (null != xaQueuemanager) {
                // getQueueManager is deprecated in MQ 5.3; retained for 5.2 compatibility
                qmgr = xaQueuemanager.getMQQueueManager();
            }
        } else {
            qmgr = queuemanager;
        }

        // no queue manager
        if (null == qmgr) {
            return false;
        }

        // disconnected qm
        // we cannot depend on qmgr.isConnected() because it evaluates
        // to false only if the object was properly closed.
        try {
            qmgr.getCharacterSet();
        } catch (MQException e) {
            return false;
        }

        // queue access required in receive mode
        if (isReceiveMode && null == currentQueue) {
            return false;
        }

        // queue access optional in send mode
        if (!isReceiveMode && null == currentQueue) {
            return true;
        }

        // if queues accessed, is the current queue connection still OK?
        boolean isDisconnected = false;
        synchronized (this) {
        MQQueue aQueue;
        Set queues = openQueues.entrySet();
        Iterator thisQueue = queues.iterator();
        while (thisQueue.hasNext()) {
            Map.Entry entry = (Map.Entry) thisQueue.next();
            aQueue = (MQQueue) entry.getValue();

            // prune all disconnected queues from cache
            // we cannot depend on aQueue.isOpen() because,
            // like qmgr.isConnected(), it, too, is a POS engineering
            try {
                // only local queues can be queried
                // but checking the type of queue is a query (way to go IBM)
                // so we infer type in a roundabout way
                if ((aQueue.openOptions & MQC.MQOO_INQUIRE) == MQC.MQOO_INQUIRE) {
                    // Even if MQOO_INQUIRE option was used, still have to
                    // distinguish between alias queues and local queues because
                    // alias queues are local but cannot be queried!!
                    if (aQueue.getQueueType() == MQC.MQQT_LOCAL) {
                        aQueue.getCurrentDepth();
                    }
                }
            }
            catch (MQException e) {
                try {
                    aQueue.close();
                }
                catch (MQException e1) {
                    ;
                }
                finally {
                    thisQueue.remove();
                }

                // if the current queue is disconnected, it invalidates agent viability
                if (aQueue == currentQueue) {
                    isDisconnected = true;
                }
            }
        }
        }

        return !isDisconnected || !pickValidQueueFromCache();
    }

    /**
     * Invalidate this MQClientAgent object.  When the object is invalidated,
     * any attempt to interact with WebSphere MQ thru this object causes an
     * IllegalStateException to be raised by the method calls, where applicable.
     */
    public synchronized void invalidate() {
        disconnect();
        isInvalid = true;
    }

    /**
     * Get a message from the current queue.
     * 
     * @returns A message from the current queue, or null if no messages were
     * available.
     * 
     * @throws com.ibm.mq.MQException
     * @throws Exception
     */
    public synchronized ExtServiceMQMessage retrieveMessage(boolean syncpoint)
            throws MQException, IOException {

        if (!isConnected()) {
            throw new IllegalStateException(I18n.msg(
                    "1926: Cannot get message - client is not connected"));
        }

        GMO gmo = new GMO();
        
        if (mLog.isLoggable(Level.FINE)) {
            mLog.fine(I18n.msg(
                    "Retreving message with GET options {0}, match options {1}, wait interval {2}",
                    "0x" + Integer.toHexString(gmo.options),
                    "0x" + Integer.toHexString(gmo.matchOptions),
                    String.valueOf(gmo.waitInterval)));
        }

        ExtServiceMQMessage m_currentMessage = null;
        try {
            m_currentMessage = newMessage();
            gmo.setMQGMO_SYNCPOINT(syncpoint);
            currentQueue.get(m_currentMessage, gmo);
            if (mLog.isLoggable(Level.FINE)) {
                mLog.fine(I18n.msg("Message retrieved from queue {0}",
                        this.currentQueue.name));
            }
        } catch (IOException e) {
            mLog.log(Level.SEVERE, I18n.msg(
                    "1924: Failed to prepare message container."), e);
        } catch (MQException mqx) {
            if (mqx.reasonCode == MQException.MQRC_NO_MSG_AVAILABLE) {
                if (mLog.isLoggable(Level.FINE)) {
                    mLog.fine(I18n.msg(
                            "No messages available to retrieve. {0}",
                            MQXDecode.decode(mqx.reasonCode)));
                }
                m_currentMessage = null;
            } else if (mqx.reasonCode == MQException.MQRC_TRUNCATED_MSG_ACCEPTED) {
                // if the GET failed because the message in the queue
                // is bigger than the buffer size, accept the truncated
                // message anyway if the user wants it
                mLog.fine(I18n.msg("Truncated message accepted, queue {0}"
                        + " reason code {1}: {2}",
                        currentQueueName,
                        mqx.reasonCode,
                        MQXDecode.decode(mqx.reasonCode)));
            } else {
                mLog.severe(I18n.msg(
                        "1927: Failed to get message from queue {0}, reason code {1}: {2}",
                        this.currentQueue.name,
                        String.valueOf(mqx.reasonCode),
                        MQXDecode.decode(mqx.reasonCode)));
                throw mqx;
            }
        }
        return m_currentMessage;
    }
    
    /**
     * Put a message into the current queue.
     * 
     * @see #newMessage()
     */
    public synchronized void putMessage(ExtServiceMQMessage msg, PMO pmo)
            throws MQException, IOException {
        
        if (!isConnected()) {
            throw new IllegalStateException(I18n.msg(
                    "1928: Cannot put message - client is not connected"));
        }
        
         if (mLog.isLoggable(Level.FINE)) {
             if (pmo != null) mLog.fine(I18n.msg(
                     "Putting message with PUT options {0}",
                     "0x" + Integer.toHexString(pmo.options)));
             else {
                 mLog.fine(I18n.msg("Putting message with default PUT options"));
             }
         }
        

        try {
            if (pmo == null) {
                currentQueue.put(msg);
            } else {
                currentQueue.put(msg, pmo);
            }
            if (mLog.isLoggable(Level.FINE)) {
                mLog.log(Level.FINE,
                        I18n.msg("Message put in queue {0}",
                                this.currentQueue.name));
            }
        }
        catch (MQException mqx) {
            mLog.severe(I18n.msg("1929: Failed to put message in queue {0},"
                    + " reason code {1}: {2}",
                    this.currentQueue.name,
                    String.valueOf(mqx.reasonCode),
                    MQXDecode.decode(mqx.reasonCode)));
            throw mqx;
        }
    }
    
    /**
     * Retrieve the XAResource object associated with this MQClientAgent's queue
     * manager.
     *
     * @return XAResource for the queue manager.
     *
     * @throws IllegalStateException
     * @throws MQBCExtServiceException
     * @throws XAException
     */
    public synchronized XAResource getXAResource()
            throws XAException, MQBCExtServiceException  {

        if (isInvalid) {
            throw new IllegalStateException(I18n.msg(
                    "1900: MQ Client no longer usable - invalidated"));
        }

        if (!isXaEnabled) {
            throw new IllegalStateException(I18n.msg(
                    "1930: Cannot provide XA resource - not in transactional mode"));
        }
        MQXAResource xares = null;
        xares= xaQueuemanager.getXAResource();
        if (mLog.isLoggable(Level.FINER)) {
            xares.setDebugXA(true);
        }
        
        XAResource resource;
        synchronized (seenXAResources) {
            if (seenXAResources.containsKey(xares)) {
                resource = seenXAResources.get(xares);
            } else {
                resource = (useXAWrapper ? new XAResourceWrapper(xares) : xares);
                seenXAResources.put(xares, resource);
            }
        }
        return resource;
    }



    /**
     * Create a message appropriately prepared with respect to the
     * MQClientAgent's configuration settings.
     *
     * @return a new Message
     */
    public ExtServiceMQMessage newMessage() throws MQException, IOException {

        if (isInvalid) {
            throw new IllegalStateException(I18n.msg(
                    "1900: MQ Client no longer usable - invalidated"));
        }
        
        ExtServiceMQMessage msg = new ExtServiceMQMessage();
        initializeMessage(msg);
        synchronized (this) {
        msg.assignSyncpointController(new SyncpointControl(queuemanager));
        }
        return msg;
    }

    /**
     * Prepare a message appropriately with respect to the MQClientAgent's
     * configuration settings.
     *
     * @param msg Message to initialize.
     */
    private void initializeMessage(ExtServiceMQMessage msg)
            throws IOException, MQException {
        if (isInvalid) {
            throw new IllegalStateException(I18n.msg(
                    "1900: MQ Client no longer usable - invalidated"));
        }

        if (isReceiveMode) {
            initializeReceiveMessage(msg);
        }
        else {
            initializeGetPutMessage(msg);
        }
    }

    private void initializeReceiveMessage(ExtServiceMQMessage msg)
            throws IOException, MQException {
        String charsetId = null;

        try {
            msg.clearMessage();

            // This code tries to convert IBM's charset name to a Java
            // charset name. This computation was necessary to decode
            // character data on some fields that are no longer exposed.
            // This code originated from SeeBeyond eGate eWay days.
            // I'm keeping it in case there's a need to restore these
            // fields to use.
//            synchronized (this) {
                // Translate IBM's charset name to a Java charset name.
                // First try a CCSID look-up.
                // Second try to resolve using Java's encoding support.
                // If we can't find a translation, we'll use the charset name
                // as-is, and let the actual Java method that uses it complain
                // as appropriate.  We do this because there have been cases
                // in testing where java.nio.Charset complains that an MQ charset
                // name is unrecognized, but the String.getBytes method is OK with
                // the name (e.g., "437").
//                int qm_ccsid = queuemanager.getCharacterSet();
//                charsetId = MQCcsidTable.lookup(String.valueOf(qm_ccsid));
//                mLog.fine(I18n.msg("Queue manager {0} CCSID: {1}.",
//                        queuemanagerName,
//                        qm_ccsid));
//                if (charsetId != null && !"".equals(charsetId)) {
//                    mLog.fine(I18n.msg(
//                            "(via MQ CCSID table) CCSID resolves to name: {0}.",
//                            charsetId));
//                } else {
//                    try {
//                        Charset charset = Charset.forName(Integer.toString(qm_ccsid));
//                        charsetId = charset.name();
//                        mLog.fine(I18n.msg(
//                                "(via Java Runtime Charset) CCSID resolves to name: {0}.",
//                                charsetId));
//                    }
//                    catch (Exception e) {
//                        charsetId = Integer.toString(qm_ccsid);
//                    }
//                    mLog.fine(I18n.msg(
//                            "CCSID {0} unresolvable, will attempt to use it as itself.",
//                            charsetId));
//                }
//            } // synchronize
        }
        catch (UnsupportedEncodingException e) {
            mLog.severe(I18n.msg("1931: Failed to prepare new message"
                    + " - queue manager's charset {0} is an unsupported"
                    + " encoding in the current runtime environment.",
                    charsetId));
            throw e;
        }
        
    }

    private void initializeGetPutMessage(ExtServiceMQMessage msg)
            throws MQException, IOException {
        String charsetId = null;

        try {
            msg.clearMessage();
            
            if (queuemanager != null) {
                msg.characterSet = queuemanager.getCharacterSet();
            }

            // This code tries to convert IBM's charset name to a Java
            // charset name. This computation was necessary to decode
            // character data on some fields that are no longer exposed.
            // This code originated from SeeBeyond eGate eWay days.
            // I'm keeping it in case there's a need to restore these
            // fields to use.
//            synchronized (this) {
            
            // Translate IBM's charset name to a Java charset name.
            // First try a CCSID look-up.
            // Second try to resolve using Java's encoding support.
            // If we can't find a translation, we'll use the charset name
            // as-is, and let the actual Java method that uses it complain
            // as appropriate.  We do this because there have been cases
            // in testing where java.nio.Charset complains that an MQ charset
            // name is unrecognized, but the String.getBytes method is OK with
            // the name (e.g., "437").
//            int qm_ccsid = queuemanager.getCharacterSet();
//            charsetId = MQCcsidTable.lookup(String.valueOf(qm_ccsid));
//            mLog.fine(I18n.msg("Queue manager {0} CCSID: {1}.", queuemanagerName,
//                    qm_ccsid));
//            if (charsetId != null && !"".equals(charsetId)) {
//                mLog.fine(I18n.msg(
//                        "(via MQ CCSID table) CCSID resolves to name: {0}.",
//                        charsetId));
//            } else {
//                try {
//                    Charset charset = Charset.forName(Integer.toString(qm_ccsid));
//                    charsetId = charset.name();
//                    mLog.fine(I18n.msg(
//                            "(via Java Runtime Charset) CCSID resolves to name: {0}.",
//                            charsetId));
//                }
//                catch (Exception e) {
//                    charsetId = Integer.toString(qm_ccsid);
//                }
//                mLog.fine(I18n.msg(
//                        "CCSID {0} unresolvable, will attempt to use it as itself.",
//                        charsetId));
//            }
//            } // synchronized
        }
        catch (UnsupportedEncodingException e) {
            mLog.severe(I18n.msg("1931: Failed to prepare new message"
                    + " - queue manager's charset {0} is an unsupported"
                    + " encoding in the current runtime environment.",
                    charsetId));
            throw e;
        }
    }

    /**
     * Establsh this MQClientAgent's connection to a queue manager.  If a queue
     * name is specified, the queue will be accessed as well.
     *
     * @param qmName   Queue manager to which to connect
     * @param qName    Optional queue name
     * @param hostname Queue manager's host machine name
     * @param port     Host machine port on which the queue manager is bound.
     * @param channel  Channel name
     * @param userid   Userid for connection
     * @param password Password for connection
     *
     * @throws MQBCExtServiceException if any error prevents successful
     *                                    connection.
     */
    private synchronized void _connect()
            throws MQBCExtServiceException {

        if (isConnected()) {
            throw new MQBCExtServiceException(I18n.msg(
                    "1933: Client already connected"));
        }

        try {
            final boolean bindingsMode = "".equals(hostname);
            
            java.util.Hashtable mqProps = new Hashtable();
            mqProps.put("channel", channelName);
            mqProps.put("hostname", hostname);
            mqProps.put("userID", userName);            
            mqProps.put("password", password);
            mqProps.put("port", (port >= 0 ? port : MQ_DEFAULT_PORT));
            mqProps.put(com.ibm.mq.MQC.SSL_CIPHER_SUITE_PROPERTY, cipherSuite);
            mqProps.put(com.ibm.mq.MQC.SSL_PEER_NAME_PROPERTY, sslPeerName);
            mqProps.put(com.ibm.mq.MQC.SSL_FIPS_REQUIRED_PROPERTY, requireSFIPS);
            mqProps.put(com.ibm.mq.MQC.TRANSPORT_PROPERTY,
                                         (bindingsMode
                                          ? MQC.TRANSPORT_MQSERIES_BINDINGS
                                          : MQC.TRANSPORT_MQSERIES_CLIENT));
            try {
                setupSecurityExit();
            }
            catch (Exception e) {
                mLog.severe(I18n.msg(
                        "1938: Failed to initialize security exit for queue manager {0}",
                        configuration.getQueueManagerName()));
                postConnected(false,
                              queuemanagerName,
                              currentQueueName,
                              channelName,
                              hostname,
                              port,
                              userName,
                              e.getLocalizedMessage());
                throw new MQBCExtServiceException(e);
            }
            
            if (!bindingsMode) {
                if ("".equals(channelName)) {
                    throw new MQBCExtServiceException(I18n.msg(
                            "1932: Failed to connect - a channel must be" 
                                    + " specified for client connections"));
                }
            }

            // Connect to queue manager
            if (isXaEnabled) {
                mqProps.put(MQC.THREAD_AFFINITY, true);
                xaQueuemanager = new MQXAQueueManager(queuemanagerName,mqProps);
                // getQueueManager is deprecated in MQ 5.3; retained for 5.2 compatibility
                queuemanager = xaQueuemanager.getMQQueueManager();
            }
            else {
                queuemanager = new MQQueueManager(queuemanagerName,mqProps);
                xaQueuemanager = null;
            }

            if (bindingsMode) {
                mLog.fine(I18n.msg("Binding connection to {0} established.",
                        queuemanager));
            } else {
                mLog.fine(I18n.msg("Client connection to {0} established.",
                        queuemanager));
            }

            // Connect to queue
            currentQueue = null;
            if (currentQueueName.equals("")) {
                mLog.fine(I18n.msg("Queue name not specified,"
                        + " connection process finished"));
            } else {
                try {
                    mLog.fine(I18n.msg(
                            "Attempting to access specified queue: {0}",
                            currentQueueName));
                    
                    // If in receive mode, I must be accessing a local queue
                    // (because remote queues can't be opened for input), so
                    // MQOO_INPUT_AS_Q_DEF | MQOO_INQUIRE options are my open
                    // options.
                    if (isReceiveMode) {
                        mLog.fine(I18n.msg("Receive mode; accessing queue"
                                + " with MQOO_INPUT_AS_Q_DEF | MQOO_INQUIRE"));
                        currentQueue = queuemanager.accessQueue(currentQueueName,
                                MQC.MQOO_INPUT_AS_Q_DEF | MQC.MQOO_INQUIRE);
                    }

                    // But if not in receive mode, I am either gearing up for
                    // PUTs or GETs.  I should open with
                    // MQOO_OUTPUT | MQOO_INPUT_AS_Q_DEF options | MQOO_INQUIRE,
                    // so both PUTs and GETs can be done on the queue.
                    //
                    // But remote queues cannot be opened with
                    // MQOO_INPUT_AS_Q_DEF.  And if a remote queue has no local
                    // alias, it cannot be opened by MQOO_INQUIRE either (ARGH!)
                    // Compounding the problem is that I can't determine if a
                    // queue is remote or not until after I open it.
                    //
                    // So, try to open the queue several times with different
                    // open options combinations, from less restrictive to most
                    // restrictive combos, stopping as soon as it is opened
                    // successfully.
                    else {
                        if (currentQueue == null) {
                            mLog.finer(I18n.msg("Starting access chain in case it" 
                                    + " is a remote queue."));
                            try {
                                mLog.finer(I18n.msg("First assume local queue;" 
                                        + " open with MQOO: +INPUT +OUTPUT +INQUIRE"));
                                currentQueue = queuemanager.accessQueue(currentQueueName,
                                        configuration.getQueueAccessOptions().getOptions()
                                                | MQC.MQOO_INPUT_AS_Q_DEF | MQC
                                                .MQOO_OUTPUT | MQC.MQOO_INQUIRE);
                            } catch (MQException e) {
                                if (e.reasonCode != 2045) {
                                    throw e;
                                }
                                // Fallthru for reason 2045 to try other access options
                            }
                        }
                        
                        if (currentQueue == null) {
                            try {
                                // MQRC_OPTION_NOT_VALID_FOR_TYPE
                                // might be due to MQOO_INQUIRE or MQOO_INPUT_AS_Q_DEF
                                // Try without MQOO_INPUT_AS_Q_DEF
                                mLog.finer(I18n.msg(
                                        "open failed, next assume remote queue;"
                                                + " open with MQOO: -INPUT +OUTPUT +INQUIRE"));
                                currentQueue = queuemanager.accessQueue(currentQueueName,
                                        (configuration.getQueueAccessOptions().getOptions()
                                                & ~MQC.MQOO_INPUT_AS_Q_DEF) | MQC
                                                .MQOO_OUTPUT | MQC
                                                .MQOO_INQUIRE);
                            } catch (MQException e) {
                                if (e.reasonCode != 2045) {
                                    throw e;
                                }
                                // Fallthru for reason 2045 to try other access options
                            }
                        }

                        if (currentQueue == null) {
                            try {
                                // MQRC_OPTION_NOT_VALID_FOR_TYPE
                                // looks like it's due to MQOO_INQUIRE, try again
                                // with MQOO_INPUT_AS_Q_DEF put back
                                mLog.finer(I18n.msg(
                                        "open failed again, next assume remote queue"
                                                + " with no local aliase;"
                                                + " open with MQOO: +INPUT +OUTPUT -INQUIRE"));
                                currentQueue = queuemanager.accessQueue(currentQueueName,
                                        (configuration.getQueueAccessOptions().getOptions()
                                                & ~MQC.MQOO_INQUIRE) | MQC
                                                .MQOO_INPUT_AS_Q_DEF | MQC
                                                .MQOO_OUTPUT);
                            }
                            catch (MQException e) {
                                if (e.reasonCode == 2045) {
                                    throw e;
                                }
                            }
                        }
                        
                        if (currentQueue == null) {
                            // MQRC_OPTION_NOT_VALID_FOR_TYPE
                            // It must be due to BOTH
                            // MQOO_INQUIRE and MQOO_INPUT_AS_Q_DEF
                            // Retry, open for PUTs only
                            mLog.finer(I18n.msg("open failed again; last resort: "
                                    + " open with MQOO: +OUTPUT"));
                            currentQueue = queuemanager.accessQueue(currentQueueName,
                                    (configuration.getQueueAccessOptions().getOptions()
                                            & ~MQC.MQOO_INQUIRE
                                            & ~MQC.MQOO_INPUT_AS_Q_DEF) | MQC
                                            .MQOO_OUTPUT);
                        }
                    }

                    openQueues.put(currentQueueName, currentQueue);
                    mLog.fine(I18n.msg("Client successfully accessed queue {0}", currentQueueName));
                }
                catch (MQException e) {
                    // A minimal connection to MQ means a queue manager connection
                    // A valid queue connection (an "accessed" queue) is nice but optional,
                    // so if we fail to open a queue, that's OK, we still have
                    // the queue manager.
                    mLog.warning(I18n.msg(
                                 "1939: Client valid for queue manager {0}" 
                                         + " but failed to access queue {1}," 
                                         + " reason {2}: {3}",
                            queuemanagerName,
                            currentQueueName,
                            e.reasonCode,
                            MQXDecode.decode(e.reasonCode)));
                    currentQueueName = "";
                    currentQueue = null;
                }
            }

            postConnected(true,
                          queuemanagerName,
                          currentQueueName,
                          channelName,
                          hostname,
                          port,
                          userName,
                          null);

        } catch (MQException e) {
            Throwable cause = e.getCause();
            mLog.log(Level.SEVERE,
                    I18n.msg("1934: Failed to connect to queue manager {0}, reason {1}: {2}",
                            configuration.getQueueManagerName(),
                            e.reasonCode,
                            MQXDecode.decode(e.reasonCode)),
                    (cause == null ? e : cause));
            postConnected(false,
                          queuemanagerName,
                          currentQueueName,
                          channelName,
                          hostname,
                          port,
                          userName,
                          MQXDecode.decode(e.reasonCode));
            throw new MQBCExtServiceException(cause == null ? e : cause);
        }
    }

    private void setupSecurityExit()
            throws Exception
    {
        MQSecurityExit exit;
        Class claz;
        String exitName;

        exit = null;
        exitName = configuration.getSecurityExit().trim();

        if (!"".equals(exitName)) {
            mLog.fine(I18n.msg("Loading security exit {0}", exitName));
            claz = Class.forName(exitName);
            exit = (MQSecurityExit) claz.newInstance();
        }
        MQEnvironment.securityExit = exit;
    }

    /**
     * Pick one of the cached open queue connections to be the current queue
     * connection.
     *
     * @return <code>true</code> if an open queue connection was found.
     */
    private synchronized boolean pickValidQueueFromCache() {
        MQQueue aQueue;
        Set queues;
        Iterator thisQueue;
        boolean gotGoodQueue;

        queues = openQueues.entrySet();
        thisQueue = queues.iterator();
        gotGoodQueue = false;

        while (thisQueue.hasNext() && !gotGoodQueue) {
            Map.Entry entry = (Map.Entry) thisQueue.next();
            aQueue = (MQQueue) entry.getValue();

            // prune all disconnected queues from cache
            // not using aQueue.isOpen() because it is not reliable
            try {
                // only local queues can be queried
                // but checking the type of queue is a query (way to go IBM)
                // so we infer type in a roundabout way
                if ((aQueue.openOptions & MQC.MQOO_INQUIRE) == MQC.MQOO_INQUIRE) {
                    // Even if MQOO_INQUIRE option was used, still have to
                    // distinguish between alias queues and local queues because
                    // alias queues are local but cannot be queried!!
                    if (aQueue.getQueueType() == MQC.MQQT_LOCAL) {
                        aQueue.getCurrentDepth();
                    }
                }
                gotGoodQueue = true;
                currentQueue = aQueue;
                currentQueueName = (String) entry.getKey();
            }
            catch (MQException e) {
                try {
                    aQueue.close();
                }
                catch (MQException e1) {
                    ;
                }
                finally {
                    thisQueue.remove();
                }
            }

        }
        return gotGoodQueue;
    }

    
    /**
     * Post a info/error connection notification.
     *
     * @param success Use <code>true</code> to cause a "is connected"
     *                notification to be posted.
     * @param qmName  Name of connected queue manager
     * @param qName   Name of queue
     * @param channel Name of connected channel
     * @param host    Name of queue manager host
     * @param port    Queue manager bind port
     * @param user    Connection's login
     * @param reason  Optional detail message included in the alert message when
     *                posting a fail notification (when <code>success</code> is
     *                <code>false</code>)
     */
    private void postConnected(boolean success,
                               String qmName,
                               String qName,
                               String channel,
                               String host,
                               int port,
                               String user,
                               String reason) {

        final boolean inBindingsMode = ("".equals(host));

        if (success) {
            if (inBindingsMode) {
                mLog.finer(I18n.msg("Client connected (binding connection):"
                        + " Queue manager {0}, channel {1}, queue {2}, user {3}",
                        qmName,
                        channel,
                        qName,
                        user));
            } else {
                mLog.finer(I18n.msg("Client connected (client connection):"
                        + " Queue manager {0}, channel {1}, queue {2}," 
                        + " host {3}, port {4}, user {5}",
                        qmName,
                        channel,
                        qName,
                        host,
                        port,
                        user));
            }
        } else {
            if (inBindingsMode) {
                mLog.severe(I18n.msg("1936: Failed to connect (binding):"
                        + " Queue manager {0}, channel {1}, queue {2}, user {3}",
                        qmName,
                        channel,
                        qName,
                        user));
            } else {
                mLog.severe(I18n.msg("1937: Failed to connect (client):"
                        + " Queue manager {0}, channel {1}, queue {2},"
                        + " host {3}, port {4}, user {5}, reason: {6}",
                        qmName,
                        channel,
                        qName,
                        host,
                        port,
                        user,
                        reason));
            }
        }
    }

    private boolean empty(String value) {
        return value == null || "".equals(value.trim());
    }
    
    private final Map<String, String> cipherSuiteToCipherSpecMap =
            new HashMap<String, String>();
    private final Map<String, String> cipherSuiteToSFIPSCipherSpecMap =
            new HashMap<String, String>();
    {
        cipherSuiteToCipherSpecMap.put(
                "SSL_RSA_EXPORT_WITH_RC4_40_MD5", "RC4_MD5_EXPORT");
        cipherSuiteToCipherSpecMap.put(
                "SSL_RSA_WITH_3DES_EDE_CBC_SHA", "TRIPLE_DES_SHA_US");
        cipherSuiteToCipherSpecMap.put(
                "SSL_RSA_WITH_DES_CBC_SHA", "DES_SHA_EXPORT");
        cipherSuiteToCipherSpecMap.put(
                "SSL_RSA_WITH_NULL_MD5", "NULL_MD5");
        cipherSuiteToCipherSpecMap.put(
                "SSL_RSA_WITH_NULL_SHA", "NULL_SHA");
        cipherSuiteToCipherSpecMap.put(
                "SSL_RSA_WITH_RC4_128_MD5", "RC4_MD5_US");
        cipherSuiteToCipherSpecMap.put(
                "SSL_RSA_WITH_RC4_128_SHA", "RC4_SHA_US");
        cipherSuiteToSFIPSCipherSpecMap.put(
                "TLS_RSA_WITH_AES_128_CBC_SHA", "TLS_RSA_WITH_AES_128_CBC_SHA");
        cipherSuiteToSFIPSCipherSpecMap.put(
                "TLS_RSA_WITH_AES_256_CBC_SHA", "TLS_RSA_WITH_AES_256_CBC_SHA");
    }
    
    private String findCipherSpec(String cipherSuite) {
        return !empty(cipherSuite)
                ? cipherSuiteToCipherSpecMap.get(cipherSuite.toUpperCase())
                : null;
    }
    
    private String findSFIPSCipherSpec(String cipherSuite) {
        return !empty(cipherSuite)
                ? cipherSuiteToSFIPSCipherSpecMap.get(cipherSuite.toUpperCase())
                : null;
    }

    /**
     * Enables XAResource wrapping as a workaround for a WSMQ transaction 
     * support bug. The defect causes transaction resource enlistment to lock up
     * when multiple resources representing the same resource manager attempt
     * to start work on the same transaction branch.  Multiple resources
     * representing the resource manager can occur for WebSphere MQ when
     * multiple connections to the same queue manager are participating
     * together in the same transaction. In WSMQ, the resource manager is the
     * queue manager. See:
     * https://open-esb.dev.java.net/issues/show_bug.cgi?id=1183
     * <p/>
     * When the workaround is enabled, calls to {@link #getXAResource()} return
     * a XAResource object whose {@link XAResource#isSameRM(javax.transaction.xa.XAResource)}
     * implementation will never return true. This behaviour is intended to
     * cause the JTS to begin work on separate transaction branches, avoiding
     * the lock-up.
     * <p/>
     * By default, the workaround is disabled.
     * 
     * @param enableWorkaround Specify true to enable the workaround, false to
     *        disable it.
     */
    public void enableIsSameRMWorkaround(boolean enableWorkaround) {
        useXAWrapper = enableWorkaround;
        synchronized (seenXAResources) {
            for (Map.Entry<MQXAResource, XAResource> entry : seenXAResources.entrySet()) {
                MQXAResource mqxares = entry.getKey();
                XAResource xares = entry.getValue();
                if (enableWorkaround) {
                    // Wrap resources
                    if (!XAResourceWrapper.class.isAssignableFrom(xares.getClass())) {
                        entry.setValue(new XAResourceWrapper(xares));
                    }
                } else {
                    // Unwrap resources
                    if (XAResourceWrapper.class.isAssignableFrom(xares.getClass())) {
                        entry.setValue(mqxares);
                    }
                }
            }
        }
    }
}
