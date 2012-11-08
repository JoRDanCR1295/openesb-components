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
 * @(#)SNMPRAImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine.impl;

import com.sun.jbi.snmpengine.SNMPCallback;
import com.sun.jbi.snmpengine.SNMPQueryInterceptor;
import com.sun.jbi.snmpengine.SNMPRAConfig;
import com.sun.jdmk.tasks.DaemonTaskServer;
import com.sun.management.snmp.SnmpEngineParameters;
import com.sun.management.snmp.SnmpPdu;
import com.sun.management.snmp.SnmpPduRequest;
import com.sun.management.snmp.SnmpPduTrap;
import com.sun.management.snmp.SnmpScopedPduRequest;
import com.sun.management.snmp.SnmpVarBind;
import com.sun.management.snmp.SnmpVarBindList;
import com.sun.management.snmp.manager.SnmpParameters;
import com.sun.management.snmp.manager.SnmpPeer;
import com.sun.management.snmp.manager.SnmpRequest;
import com.sun.management.snmp.manager.SnmpRequestHandler;
import com.sun.management.snmp.manager.SnmpSession;
import com.sun.management.snmp.manager.SnmpTrapListener;
import com.sun.soabi.snmpbc.getrequests.GetRequest;
import com.sun.soabi.snmpbc.getrequests.GetRequests;
import com.sun.soabi.snmpbc.getrequests.RequestType;
import com.sun.soabi.snmpbc.getresponses.GetResponse;
import com.sun.soabi.snmpbc.getresponses.GetResponses;
import com.sun.soabi.snmpbc.getresponses.ResponseStatusType;
import com.sun.soabi.snmpbc.getresponses.ResponseVarBind;
import com.sun.soabi.snmpbc.metadataquery.MetaDataQuery;
import com.sun.soabi.snmpbc.metadataquery.NetworkElementIdent;
import com.sun.soabi.snmpbc.metadataquery.ObjectFactory;
import com.sun.soabi.snmpbc.metadataresponse.MetaDataResponse;
import com.sun.soabi.snmpbc.metadataresponse.NetworkElementProperties;
import com.sun.soabi.snmpbc.traps.Trap;
import com.sun.soabi.snmpbc.traps.Traps;
import com.sun.soabi.snmpbc.traps.VarBinding;
import java.util.Enumeration;
import java.util.Iterator;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.util.JAXBSource;
import javax.xml.transform.Source;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Listens to traps and forwards them for processing
 * 
 * @author fkieviet
 */
public class SNMPRAImpl {
    private static Logger sLog = Logger.getLogger(SNMPRAImpl.class.getName());
    private SnmpTrapListener mTrapListener;
    private SnmpSession mSession;
    private CopiedSnmpEventReportDispatcher2 mTrapAgent;
    private DaemonTaskServer mTaskServer;
    private boolean mStarted;
    private TrapQueue mTrapQueue;
    private ScheduledThreadPoolExecutor mThreadpool;
    private SNMPRAConfig mConfig;
    private SNMPCallback mCallback;
    private JAXBContext mJAXBContext;
    private JAXBContext mJAXBContextQuery;
    private JAXBContext mJAXBContextQueryReply;
    private JAXBContext mJAXBContextPMRequest;
    private JAXBContext mJAXBContextPMReply;
    private QueryProcessor mQueryProcessor;
    private PMProcessor mPMProcessor;
    private ConcurrentMap < String, TrapDeliveryQueue > mTrapProcessorQueues;

    /**
     * SOCKET ----> TrapQueue
     *                        -----> Processor(s)
     */
    private class TrapQueue extends BatchQueue < SnmpPdu > {

        public TrapQueue(int batchSize, int maxWait, ScheduledThreadPoolExecutor exec) throws Exception {
            super(batchSize, maxWait, exec);
        }

        @Override
        public SnmpPdu[] newArray(int size) {
            return new SnmpPdu[size];
        }

        @Override
        public void processBatch(SnmpPdu[] batch, int batchId) {
            if (sLog.isLoggable(Level.FINE)) {
                sLog.fine("TrapQueue: processing batch #" + batchId + " with " 
                    + batch.length + " pdus");
            }
            
            processBatch(batch);
        }

        /**
         * Processes a batch of PDUs: gets the meta data and sends the PDUs to the 
         * processors
         * 
         * @param batch batch to process
         */
        public void processBatch(SnmpPdu[] batch) {
            // Get all metadata for this batch in one batch
            MetaData[] data = mQueryProcessor.getMetaData(batch);
            
            int nPosted = 0;
            int nPending = 0;
            int nQueued = 0;
            
            // Process each PDU
            for (int i = 0; i < batch.length; i++) {
                SnmpPdu p = batch[i];
                MetaData d = data[i];
                if (d == null) {
                    // No meta data found; packet is already moved to a holding area
                    // simply skip processing here
                    nPending++;
                } else {
                    nPosted++;
                    String[] ds = d.getDestinations();
                    for (String s : ds) {
                        TrapDeliveryQueue q = getTrapProcessorQueue(s);
                        q.add(p);
                        nQueued++;
                    }
                }
            }
            
            if (sLog.isLoggable(Level.FINE)) {
                sLog.fine("TrapQueue: processed " + batch.length + " pdus, " + nPending 
                    + " are pending, " + nPosted + " are posted, " + nQueued 
                    + " pdus are queued for delivery");
            }
        }
    }
    
    /**
     * TRAP QUEUE ----> QUERY PROCESSOR ----> METADATA SERVICE ENGINE
     *                      +----> HOLD                 |
     *                               +            <-----+
     *                               |
     *                 TRAP PROCESSOR QUEUE
     */                 
    private class QueryProcessor extends BatchQueue < TrapID > {
        private Map < TrapID, MetaData > mCache;
        private Map < Integer, List < SnmpPdu > > mHolds;
        private Map < Integer, TrapID[] > mQueries;

        public QueryProcessor(int batchSize, int maxWait, ScheduledThreadPoolExecutor exec) {
            super(batchSize, maxWait, exec);
            mCache = new HashMap < TrapID, MetaData > ();
            mHolds = new HashMap < Integer, List < SnmpPdu > > ();
            mQueries = new HashMap < Integer, TrapID[] > ();
        }
        
        public synchronized String dump() {
            StringBuilder buf = new StringBuilder();
            buf.append("QueryProcessor internal state: in cache=").append(mCache.size()).append(" items, ");
            buf.append("holds=").append(mHolds.size()).append(" (");
            for (Map.Entry < Integer, List < SnmpPdu > > h : mHolds.entrySet()) {
                buf.append('#').append(h.getKey()).append('=').append(h.getValue().size()).append(' ');
            }
            buf.append(") ").append(", queries=").append(mQueries.size()).append(" (");
            for (Map.Entry < Integer, TrapID[] > h : mQueries.entrySet()) {
                buf.append('#').append(h.getKey()).append('=').append(h.getValue().length).append(' ');
            }
            buf.append(") ");
            return buf.toString();
        }

        @Override
        public TrapID[] newArray(int size) {
            return new TrapID[size];
        }

        @Override
        public void processBatch(TrapID[] idbatch, int batchId) {
            // Create a query
            MetaDataQuery query = new MetaDataQuery();

            // Batch is a set of unique TrapIDs (i.e. there are no duplicates)
            for (TrapID id : idbatch) {
                NetworkElementIdent ne = new NetworkElementIdent();
                ne.setIPAddress(PacketTools.addressToString(id.getAddress()));
                ne.setPort(id.getPort());
                ne.setReplyID(TrapID.serializeTrapIDToString(id));
                query.getNetworkElementIdents().add(ne);
            }
            
            // Remember the full query that was sent
            List < SnmpPdu > hold;
            synchronized (this) {
                mQueries.put(batchId, idbatch);
                hold = mHolds.get(batchId);
                assert hold != null;
            }
            
            if (sLog.isLoggable(Level.FINE)) {
                sLog.fine("QueryProcessing: delivering query request [" + batchId + "] for " + idbatch.length 
                    + " elements; there are " + hold.size() + " pdus waiting on this");
                sLog.fine(dump());
            }
            
            try {
                // Deliver
                ObjectFactory objectFactory = new ObjectFactory();
                JAXBElement < MetaDataQuery > jaxbElement = objectFactory.createMetaDataQuery(query);
                JAXBSource source = new JAXBSource(mJAXBContextQuery, jaxbElement);
                mCallback.getMetaData(Integer.toString(batchId), source);
            } catch (Exception e) {
                sLog.log(Level.SEVERE, "Failed to deliver traps: " + e, e);
            }
        }
        
        public MetaData[] getMetaData(SnmpPdu[] batch) {
            MetaData[] ret = new MetaData[batch.length];
            
            // Stats for debugging
            int nSatisfied = 0;
            int nUnsatisfied = 0;
            int nNeverSeen = 0;
            
            // For testing
            SNMPQueryInterceptor testInterceptor = mConfig.getQueryInterceptor(); 
            if (testInterceptor != null) {
                for (int i = 0; i < ret.length; i++) {
                    ret[i] = new MetaData(-1);
                    ret[i].setProcessorIds(
                        testInterceptor.getDestinations(batch[i].address, batch[i].port));
                    nSatisfied++;
                }                
            } else {
                // Create IDs (not synchronized)
                TrapID[] ids = new TrapID[batch.length];
                for (int i = 0; i < ret.length; i++) {
                    ids[i] = TrapID.createID(batch[i]);
                }

                // Lookup metadata, or create tasks if necessary
                synchronized (this) { 
                    for (int i = 0; i < ret.length; i++) {
                        TrapID id = ids[i];
                        MetaData d = mCache.get(id);

                        // If never seen before, query for it
                        if (d == null) {
                            int queryid = add(id);
                            d = new MetaData(queryid);
                            mCache.put(id, d);
                            nNeverSeen++;
                        }

                        // If query pending, hold the packet for later processing
                        if (d.available()) {
                            ret[i] = d;
                            nSatisfied++;
                        } else {
                            List < SnmpPdu > hold = mHolds.get(d.getQueryID());
                            if (hold == null) {
                                hold = new ArrayList < SnmpPdu > ();
                                mHolds.put(d.getQueryID(), hold);
                            }
                            hold.add(batch[i]);
                            nUnsatisfied++;
                        }
                    }
                }
            }
            
            if (sLog.isLoggable(Level.FINE)) {
                sLog.fine("QueryProcessor: MetaData lookup: processed " + batch.length + " pdus; "
                    + nSatisfied + " pdus were in the cache; " + nUnsatisfied + " pdus "
                    + "are now pending of which " + nNeverSeen + " pdus were never seen before.");
                sLog.fine(dump());
            }
            
            return ret;
        }

        public void replyMetaData(String queryId, Source results) {
            try {
                // Unmarshall
                Object o  = mJAXBContextQueryReply.createUnmarshaller().unmarshal(results);
                final MetaDataResponse resp = (MetaDataResponse) (o instanceof JAXBElement 
                ? ((JAXBElement) o).getValue() : o);
                
                List < NetworkElementProperties > elements = resp.getNetworkElements();
                
                if (sLog.isLoggable(Level.FINE)) {
                    sLog.fine("QueryProcessor: Processing metadata results (queryid=" + queryId 
                        + "); there are " + elements.size() + " elements");
                    sLog.fine(dump());
                }
                
                // Update the cache with the received data 
                List < SnmpPdu > hold;
                TrapID[] query;
                int nMissed = 0;
                synchronized (this) {
                    // Ensure that the queryid is valid and that the query was indeed done
                    Integer parsedQueryid = Integer.valueOf(queryId);
                    hold = mHolds.remove(parsedQueryid);
                    query = mQueries.remove(parsedQueryid);

                    // Update cache
                    for (NetworkElementProperties p : elements) {
                        try {
                            TrapID id = TrapID.unserializeTrapIDFromString(p.getReplyID());
                            MetaData meta = mCache.get(id);
                            meta.setProcessorIds(p.getProcessorID().toArray(new String[0]));
                        } catch (Exception e) {
                            sLog.log(Level.SEVERE, "Could not update cache with " + p + ": " + e, e);
                        }
                    }
                    
                    // Verify that all network elements were replied to
                    if (query != null) {
                        for (int i = 0; i < query.length; i++) {
                            MetaData meta = mCache.get(query[i]);
                            if (meta.available()) {
                                // ok
                            } else {
                                // PDUs with this network address are not going to be sent 
                                // anywhere anymore
                                meta.setProcessorIds(new String[0]);
                                nMissed++;
                            }
                        }
                    }
                }
                
                if (nMissed != 0) {
                    sLog.warning("The MetaData received (queryid=" + queryId + ") did not contain "
                        + " all requested data: " + nMissed + " network elements were missing.");
                }
                
                if (hold == null) {
                    sLog.warning("The MetaData received (queryid=" + queryId + ") did not match "
                        + " a currently outstanding query.");
                } else {
                    // Process these pdus in the same thread
                    SnmpPdu[] pdus = hold.toArray(new SnmpPdu[0]);
                    
                    if (sLog.isLoggable(Level.FINE)) {
                        sLog.fine("QueryProcessor: There are " + pdus.length
                            + " pdus waiting for the metadata in queryid=" + queryId);
                        sLog.fine(dump());
                    }
                    
                    // Process the pdus 
                    mTrapQueue.processBatch(pdus);
                }
            } catch (Exception e) {
                sLog.log(Level.SEVERE, "Could not process query reply: " + queryId + ": " + e, e);
            }
        }
    }
    
    /**
     * TRAP QUEUE / QUERY PROCESSOR DONE QUEUE 
     *    ----> TRAPPROCESSORQUEUE 
     *        ----> PROCESSOR SERVICE ENGINE
     */
    private class TrapDeliveryQueue extends BatchQueue < SnmpPdu > {
        private String mProcessorID;

        public TrapDeliveryQueue(String processorID, int batchSize, 
            int maxWait, int maxTasks, ScheduledThreadPoolExecutor exec, boolean isAsync) {
            super(batchSize, maxWait, exec, maxTasks, isAsync);
            mProcessorID = processorID;
        }
        
        public String getID() {
            return mProcessorID;
        }

        @Override
        public SnmpPdu[] newArray(int size) {
            return new SnmpPdu[size];
        }
        
        private String safeString(Object o) {
            return o == null ? "" : o.toString();
        }
        
        @Override
        public void processBatch(SnmpPdu[] batch, int batchId) {
            if (sLog.isLoggable(Level.FINE)) {
                sLog.fine("Processing " + batch.length + " pdus for delivery to " + mProcessorID);
            }
            
            Traps traps = new Traps();
            for (SnmpPdu pdu : batch) {
                if (pdu instanceof SnmpPduTrap || pdu instanceof SnmpPduRequest) {
                    // Convert into Traps object
                    Trap t = new Trap();
                    t.setUDPSource(safeString(pdu.address.toString()));
                    t.setUDPPort(pdu.port);
                    t.setType(pdu.type);
                    t.setVersion(pdu.version);
                    t.setRequestID(pdu.requestId);
                    
                    if (pdu instanceof SnmpPduTrap) {
                        SnmpPduTrap v1 = (SnmpPduTrap) pdu;
                        t.setV1AgentAddress(safeString(v1.agentAddr));
                        t.setV1EnterpriseOID(safeString(v1.enterprise));
                        t.setV1GenericTrap(v1.genericTrap);
                        t.setV1SpecificTrap(v1.specificTrap);
                        t.setV1Timestamp(v1.timeStamp);
                    }
                    
                    for (int i = 0; i < pdu.varBindList.length; i++) {
                        SnmpVarBind b = pdu.varBindList[i];
                        t.getValues().add(new VarBinding());
                        t.getValues().get(i).setOID(b.getOid().toString());
                        t.getValues().get(i).setType(-1);
                        t.getValues().get(i).setValue(b.getStringValue());
                    } 

                    traps.getTrap().add(t);

                    if (mConfig.getSecondaryInterceptor() != null) {
                        mConfig.getSecondaryInterceptor().packetReceived(pdu);
                    }
                } else {
                    sLog.warning("Unknown type " + pdu);
                }
            }

            try {
                // Deliver
                JAXBSource source = new JAXBSource(mJAXBContext, traps);
                mCallback.deliverTraps(Integer.toString(batchId), mProcessorID, source);
            } catch (Exception e) {
                sLog.log(Level.SEVERE, "Failed to deliver traps: " + e, e);
            }
        }
    }
    
    /**
     * Receives trap from primary thread (packet listener) and posts it to the
     * TrapQueue
     * 
     * PRIMARY THREAD ----> TRAP QUEUE
     */
    private class TrapListenerImpl implements SnmpTrapListener {
        public void processSnmpTrapV1(SnmpPduTrap trap) {
            // Log
            if (sLog.isLoggable(Level.FINER)) {
                sLog.finer("V1 trap received from " + trap.address + ":" + trap.port);
                if (sLog.isLoggable(Level.FINEST)) {
                    sLog.finest(PacketTools.toString(trap));
                }
            }
            // Invoke
            try {
                if (mConfig.getPrimaryInterceptor() != null) {
                    mConfig.getPrimaryInterceptor().packetReceived(trap);
                }
                mTrapQueue.add(trap);
            } catch (Exception e) {
                sLog.log(Level.WARNING, "Exception while processing V1 trap. Trap info=[" 
                    + PacketTools.toString(trap) + "], error=" + e, e);
            }
        }

        public void processSnmpTrapV2(SnmpPduRequest trap) {
            if (mConfig.getPrimaryInterceptor() != null) {
                mConfig.getPrimaryInterceptor().packetReceived(trap);
            }
            mTrapQueue.add(trap);
        }

        public void processSnmpTrapV3(SnmpScopedPduRequest trap) {
            // Log
            if (sLog.isLoggable(Level.FINER)) {
                sLog.finer("V1 trap received from " + trap.address + ":" + trap.port);
                if (sLog.isLoggable(Level.FINEST)) {
                    sLog.finest(PacketTools.toString(trap));
                }
            }
            // Invoke
            try {
                if (mConfig.getPrimaryInterceptor() != null) {
                    mConfig.getPrimaryInterceptor().packetReceived(trap);
                }
                mTrapQueue.add(trap);
            } catch (Exception e) {
                sLog.log(Level.WARNING, "Exception while processing V1 trap. Trap info=[" 
                    + PacketTools.toString(trap) + "], error=" + e, e);
            }
        }
    }
    
    /**
     * Constructor
     *
     * @param config immutable configuration object
     * @param callback callback
     */
    public SNMPRAImpl(SNMPRAConfig config, SNMPCallback callback) {
        mConfig = config;
        mTrapListener = new TrapListenerImpl();
        mCallback = callback;
        mTrapProcessorQueues = new ConcurrentHashMap < String, TrapDeliveryQueue > ();
    }
    
    /**
     * @param d metadata identifying the processor
     * @return queue
     */
    public TrapDeliveryQueue getTrapProcessorQueue(String d) {
        TrapDeliveryQueue ret = mTrapProcessorQueues.get(d);
        if (ret == null) {
            ret = new TrapDeliveryQueue(d, 
                mConfig.getProcessorBatchSize(), 
                mConfig.getProcessorMaxWait(), 
                mConfig.getProcessorMaxConcurrency(),
                mThreadpool,
                mConfig.getIsAsync()
            );
            mTrapProcessorQueues.putIfAbsent(d, ret);
            ret = mTrapProcessorQueues.get(d);
        }
        return ret;
    }

    /**
     * Starts listening to traps
     * 
     * @throws Exception on startup failure
     */
    public void start() throws Exception {
        if (mStarted) {
            throw new Exception("Already started");
        }
        try {
            // Cache JAXB
            mJAXBContext = JAXBContext.newInstance("com.sun.soabi.snmpbc.traps", 
                this.getClass().getClassLoader());
            mJAXBContextQuery = JAXBContext.newInstance("com.sun.soabi.snmpbc.metadataquery", 
                this.getClass().getClassLoader());
            mJAXBContextQueryReply = JAXBContext.newInstance("com.sun.soabi.snmpbc.metadataresponse", 
                this.getClass().getClassLoader());
            mJAXBContextPMRequest = JAXBContext.newInstance("com.sun.soabi.snmpbc.getrequests",
                this.getClass().getClassLoader());
            mJAXBContextPMReply = JAXBContext.newInstance("com.sun.soabi.snmpbc.getresponses",
                this.getClass().getClassLoader());
            
            // Setup JDMK
            SnmpEngineParameters parameters = new SnmpEngineParameters();
            parameters.activateEncryption();
            mSession = new SnmpSession(parameters, null, "SNMP engine on " + mConfig.getPort(), null);
            mTaskServer = new DaemonTaskServer();
            mTaskServer.start(Thread.NORM_PRIORITY);
            mTrapAgent = new ChangedSnmpEventReportDispatcher(mSession.getEngine(), 
                mConfig.getPort(), mTaskServer, null);
            mTrapAgent.addTrapListener(mTrapListener);
            
            // Setup trap processing
            mThreadpool = new ScheduledThreadPoolExecutor(mConfig.getNThreads(), new ThreadFactory() {
                AtomicInteger mThreadCounter = new AtomicInteger();
                public Thread newThread(Runnable r) {
                    Thread ret = new Thread(r);
                    ret.setName("TrapQueue" + mThreadCounter.incrementAndGet());
                    ret.setPriority(Thread.MIN_PRIORITY);
                    return ret;
                }
            });
            mTrapQueue = new TrapQueue(mConfig.getTrapBatchSize(), mConfig.getTrapMaxWait(), mThreadpool);
            mQueryProcessor = new QueryProcessor(mConfig.getQueryBatchSize(), mConfig.getQueryMaxWait(), mThreadpool);
            mPMProcessor = new PMProcessor();
            
            // Activate JDMK
            final Thread trapThread = new Thread(mTrapAgent);
            trapThread.setName("Trap receiving thread");
            trapThread.setPriority(Thread.MAX_PRIORITY);
            trapThread.start();
            
            // Done
            mStarted = true;
        } catch (Throwable e) {
            sLog.log(Level.WARNING, "Exception in SNMP starting procedure; will now try to " 
                + "stop engine; this may cause additional errors. Error was: " + e, e);
            stop();
            throw new Exception("Failed to start SNMP engine: " + e, e);
        }
    }
    
    /**
     * Stops listening and processing
     */
    public void stop() {
        try {
            if (mSession != null) {
                mSession.destroySession();
            }
        } catch (Exception e) {
            sLog.log(Level.WARNING, "Exception while destroying session: " + e, e);
        }

        try {
            if (mTrapAgent != null) {
                mTrapAgent.close();
            }
        } catch (Exception e) {
            sLog.log(Level.WARNING, "Exception while stopping TrapAgent: " + e, e);
        }

        try {
            if (mTaskServer != null) {
                mTaskServer.terminate();
            }
        } catch (Exception e) {
            sLog.log(Level.WARNING, "Exception while stopping TaskServer: " + e, e);
        }

        try {
            if (mThreadpool != null) {
                mThreadpool.shutdownNow();
            }
        } catch (Exception e) {
            sLog.log(Level.WARNING, "Exception while stopping threadpool: " + e, e);
        }
    }

    /**
     * Processes the results of a metadata query; method returns immediately; processing
     * is done in a different thread
     * 
     * @param queryId identifies the query
     * @param results query results
     */
    public void replyMetadata(final String queryId, final Source results) {
        if (sLog.isLoggable(Level.FINE)) {
            sLog.fine("QueryProcessor: Reply received on queryId=" + queryId);
        }
        mThreadpool.execute(new Runnable() {
            public void run() {
                mQueryProcessor.replyMetaData(queryId, results);
            }
        });
    }

    /**
     * Called when a batch of traps has been delivered processed (used for throttling)
     * 
     * @param batchId identifies the batch
     * @param error an error occurred (ME reported FAULT)
     */
    public void replyTraps(String batchId, boolean error) {
        // TODO Auto-generated method stub
    }
    
    /**
     * Process request from PM, method returns immediately.
     */
    public void requestPM(final String msgExchangeId, final Source request) {
        if (sLog.isLoggable(Level.FINE)) {
            sLog.fine("received request from PM: msgExchangeId=" + msgExchangeId);
        }
        mThreadpool.execute(new Runnable() {
            public void run() {
                mPMProcessor.requestPM(msgExchangeId, request);
            }
        });
    }
    
    /**
     * @see java.lang.Object#toString()
     */
    public String toString() {
        return mConfig.toString();
    }
    
    private class PMProcessor {
        
        // associate msgExchangeId with each requestContext
        private ConcurrentMap<String, PMRequestContext> msgExchangeIdToContextMap;
        
        public PMProcessor() {
            msgExchangeIdToContextMap = 
                    new ConcurrentHashMap<String, PMRequestContext> ();
            
        }
        
        public void requestPM(String msgExchangeId, Source request) {
            GetRequests getRequests = null;
            try {
                // Unmarshall
                Object o  = mJAXBContextPMRequest.createUnmarshaller().unmarshal(request);
                getRequests = (GetRequests) (o instanceof JAXBElement 
                ? ((JAXBElement) o).getValue() : o);
            } catch (Exception e) {
                sLog.log(Level.SEVERE, "cannot unmarshal PM request", e);
                return;
            }
                
            try {
                // create and add requestContext to list for tracking
                PMRequestContext requestContext = new PMRequestContext(msgExchangeId, getRequests);
                PMRequestContext prevValue = 
                        msgExchangeIdToContextMap.putIfAbsent(requestContext.msgExchangeId, requestContext);
                if (prevValue != null) {
                    throw new Exception("duplicate requestContext exist for msgExchangeId=" +
                            msgExchangeId);
                }
                
                for (Iterator<GetRequest> iter = getRequests.getGetRequest().iterator(); iter.hasNext(); ) {
                    GetRequest getRequest = iter.next();
                    AsynchRequestHandler callback = 
                            new AsynchRequestHandler(msgExchangeId, getRequest.getRequestID());
                    SnmpPeer peer = new SnmpPeer(getRequest.getHost(), getRequest.getPort());
                    SnmpParameters params = new SnmpParameters(getRequest.getRdCommunity(), null);
                    peer.setParams(params);

                    SnmpVarBindList list = new SnmpVarBindList();
                    for (Iterator<String> iterVarBind = getRequest.getOIDs().iterator(); iterVarBind.hasNext(); ) {
                        list.addVarBind(iterVarBind.next());
                    }
                    
                    // jaxb does not bind default values, so have to check for null
                    if (getRequest.getRequestType() == null ||
                            getRequest.getRequestType() == RequestType.GET) {
                        mSession.snmpGetRequest(peer, callback, list);
                    } else if (getRequest.getRequestType() == RequestType.GET_NEXT){
                        mSession.snmpGetNextRequest(peer, callback, list);
                    }else if (getRequest.getRequestType() == RequestType.GET_BULK){
                        mSession.snmpGetBulkRequest(peer,callback, list, getRequest.getNonRepeat(), getRequest.getMaxRepeat());
                    }
                }
                
                
            } catch (Exception e) {
                sLog.log(Level.SEVERE, "cannot process PM request", e);
            }
        }
        
        public void callbackData(String msgExchangeId, String requestId, int errStatus, int errIndex, SnmpVarBindList snmpVarBindList) {
            GetResponse getResponse = new GetResponse();
            getResponse.setRequestID(requestId);
            getResponse.setResponseStatus(ResponseStatusType.OKAY);
            for (Enumeration e = snmpVarBindList.getVarBindList(); e.hasMoreElements(); ) {
                SnmpVarBind snmpVarBind = (SnmpVarBind) e.nextElement();
                ResponseVarBind responseVarBind = new ResponseVarBind();
                responseVarBind.setOID(snmpVarBind.getOid().toString());
                responseVarBind.setType(-1);
                if (snmpVarBind.getValueStatus() != SnmpVarBind.stValueOk) {
                    responseVarBind.setHasError(true);
                    responseVarBind.setErrorMsg(snmpVarBind.getValueStatusLegend());
                } else {
                    responseVarBind.setHasError(false);
                    responseVarBind.setValue(snmpVarBind.getStringValue());
                }
                getResponse.getResponseVarBindList().add(responseVarBind);
            }
            
            addResponse(msgExchangeId, getResponse);
        }
        
        public void callbackTimeout(String msgExchangeId, String requestId) {
            GetResponse getResponse = new GetResponse();
            getResponse.setRequestID(requestId);
            getResponse.setResponseStatus(ResponseStatusType.TIMEOUT);
            
            addResponse(msgExchangeId, getResponse);
        }
        
        public void callbackInternalError(String msgExchangeId, String requestId, String errmsg) {
            GetResponse getResponse = new GetResponse();
            getResponse.setRequestID(requestId);
            getResponse.setResponseStatus(ResponseStatusType.ERROR);
            getResponse.setErrorMsg(errmsg);
            
            addResponse(msgExchangeId, getResponse);
        }
        
        private void addResponse(String msgExchangeId, GetResponse getResponse) {
            PMRequestContext requestContext = msgExchangeIdToContextMap.get(msgExchangeId);
            if (requestContext == null) {
                sLog.log(Level.SEVERE, "unable to find requestContext in callback method," +
                        " msgExchangeId=" + msgExchangeId);
                return;
            }

            try {
                synchronized (requestContext) {
                    requestContext.getResponses.add(getResponse);
                    if (requestContext.gotAllResponses()) {
                        msgExchangeIdToContextMap.remove(msgExchangeId);
                        GetResponses getResponses = new GetResponses();
                        // put responses in the same order as requests with respect to requestID
                        for (GetRequest request : requestContext.getRequests.getGetRequest()) {
                            for (GetResponse response : requestContext.getResponses) {
                                if (request.getRequestID().equals(response.getRequestID())) {
                                    getResponses.getGetResponse().add(response);
                                    continue;
                                }
                            }
                        }
                        // Deliver
                        JAXBSource source = new JAXBSource(mJAXBContextPMReply, getResponses);
                        mCallback.replyPM(msgExchangeId, source);
                    }
                }
            } catch (Exception e) {
                sLog.log(Level.SEVERE, "error encounter when sending PM reply", e);
            }
        }
        
        private class PMRequestContext {
            String msgExchangeId;
            GetRequests getRequests;
            List<GetResponse> getResponses;
            
            public PMRequestContext(String msgExchangeId, GetRequests getRequests) {
                this.msgExchangeId = msgExchangeId;
                this.getRequests = getRequests;
                this.getResponses = new ArrayList<GetResponse> ();
            }
            
            public boolean gotAllResponses() {
                if (getRequests.getGetRequest().size() == getResponses.size()) {
                    return true;
                }
                return false;
            }
        }
    }
    
    private class AsynchRequestHandler implements SnmpRequestHandler {
        
        private String msgExchangeId;
        private String requestId;
        
        public AsynchRequestHandler(String msgExchangeId, String requestId) {
            this.msgExchangeId = msgExchangeId;
            this.requestId = requestId;
        }
        
        public void processSnmpPollData(SnmpRequest snmpRequest, int errStatus, int errIndex, SnmpVarBindList snmpVarBindList) {
            mPMProcessor.callbackData(msgExchangeId, requestId, errStatus, errIndex, snmpVarBindList);
        }

        public void processSnmpPollTimeout(SnmpRequest snmpRequest) {
            mPMProcessor.callbackTimeout(msgExchangeId, requestId);
        }

        public void processSnmpInternalError(SnmpRequest snmpRequest, String errmsg) {
            mPMProcessor.callbackInternalError(msgExchangeId, requestId, errmsg);
        }

    }
    
}
