/*
 * Created on Mar 1, 2007
 */
package com.sun.jbi.snmpengine.test;

import com.sun.jbi.snmpengine.SNMPCallback;
import com.sun.jbi.snmpengine.SNMPEngineFactory;
import com.sun.jbi.snmpengine.SNMPPacketInterceptor;
import com.sun.jbi.snmpengine.SNMPRA;
import com.sun.jbi.snmpengine.systest.TrapProducer;
import com.sun.soabi.snmpbc.metadataquery.MetaDataQuery;
import com.sun.soabi.snmpbc.metadataquery.NetworkElementIdent;
import com.sun.soabi.snmpbc.metadataresponse.MetaDataResponse;
import com.sun.soabi.snmpbc.metadataresponse.NetworkElementProperties;
import com.sun.soabi.snmpbc.traps.Trap;
import com.sun.soabi.snmpbc.traps.Traps;

import org.w3c.dom.Document;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.util.JAXBSource;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Source;

import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Tests trap behavior
 * 
 * @author fkieviet
 */
public class Trap1Test extends BaseTrapTestCase {
    private static final int NBUFFERS = 1000;
    private static final int NTRAPSPERBUFFER = 50;
    private static final int WAIT = 30000;
    
    
    /**
     * Sends 10000 V1 traps to the engine; forwards traps to a trap processor where
     * they are ignored. Uses internal loopback for network. Only primary synchronization
     * and checking
     * 
     * @throws Throwable
     */
    public void testV1TrapOneSided() throws Throwable {
        // Mechanism to intercept traps
        final Semaphore sem = new Semaphore(0);
        
        SNMPCallback cb = new EmptyCallback() {
        };
        SNMPPacketInterceptor interc = new SNMPPacketInterceptor() {
            public void packetReceived(Object o) {
                sem.release();
            }
        };
        
        // Start listening
        SNMPEngineFactory fact = new SNMPEngineFactory();
        SNMPRA ra = fact.create();
        ra.setPrimaryInterceptor(interc);
        destroyOnExit(ra);
        ra.setPort(pickPort());
        ra.setQueryInterceptor(new QuerySimulator());
        ra.start(cb);
        
        TrapProducer p = new TrapProducer(NBUFFERS, NTRAPSPERBUFFER, 1, WAIT, ra);
        p.sendWithSemaphore(sem);
        
        // Done
        ra.stop();
        p.done();
        p.dump("1/One-sided", -1);
    }

    /**
     * Sends 10000 V1 traps to the engine; forwards traps to a trap processor where
     * they are ignored. Uses internal loopback for network. Only primary synchronization
     * and checking
     * 
     * @throws Throwable
     */
    public void testV1TrapOneSidedSyncPoint() throws Throwable {
        SNMPCallback cb = new EmptyCallback() {
        };
        SNMPPacketInterceptor interc = new SNMPPacketInterceptor() {
            public void packetReceived(Object o) {
            }
        };
        
        // Start listening
        SNMPEngineFactory fact = new SNMPEngineFactory();
        SNMPRA ra = fact.create();
        ra.setPrimaryInterceptor(interc);
        destroyOnExit(ra);
        int snmpport = pickPort();
        ra.setPort(snmpport);
        ra.setQueryInterceptor(new QuerySimulator());
        ra.start(cb);
        
        TrapProducer p = new TrapProducer(NBUFFERS, NTRAPSPERBUFFER, 1, WAIT, ra);
        p.sendWithSyncPacket();
        
        // Done
        ra.stop();
        p.done();
        p.dump("1/One-sided-sync", -1);
    }

    /**
     * Sends 10000 V1 traps to the engine; forwards traps to a trap processor, so 
     * this includes JAXB creation. Uses internal network loopback. Primary 
     * synchronization and secondary synchronization with an interceptor
     * 
     * @throws Throwable
     */
    public void testV1TrapDoubleSided() throws Throwable {
        // Mechanism to intercept traps
        final Semaphore primary = new Semaphore(0);
        final Semaphore end = new Semaphore(0);
        
        SNMPPacketInterceptor interc1 = new SNMPPacketInterceptor() {
            public void packetReceived(Object o) {
                primary.release();
            }
        };
        SNMPPacketInterceptor interc2 = new SNMPPacketInterceptor() {
            public void packetReceived(Object o) {
                end.release();
            }
        };
        SNMPCallback cb = new EmptyCallback();
        
        // Start listening
        SNMPEngineFactory fact = new SNMPEngineFactory();
        SNMPRA ra = fact.create();
        ra.setPrimaryInterceptor(interc1);
        ra.setSecondayInterceptor(interc2);
        destroyOnExit(ra);
        int snmpport = pickPort();
        ra.setPort(snmpport);
        ra.setQueryInterceptor(new QuerySimulator());
        ra.start(cb);
        

        TrapProducer p = new TrapProducer(NBUFFERS, NTRAPSPERBUFFER, 1, WAIT, ra);
        p.sendWithSemaphore(primary);

        // Wait for all traps to be processed 
        assertTrue(end.tryAcquire(p.getNK(), 30, TimeUnit.SECONDS));
        long t2 = System.nanoTime();
        
        // Done
        ra.stop();
        p.done();
        p.dump("1/double-sided", t2);
    }

    /**
     * Sends V1 traps to the engine; forwards traps to a trap processor, so 
     * this includes JAXB creation. Also does JAXB decoding. 
     * Uses internal network loopback.
     * 
     * @throws Throwable
     */
    public void testV1TrapDOMProc() throws Throwable {
        // Mechanism to intercept traps
        final Semaphore primary = new Semaphore(0);
        final Semaphore end = new Semaphore(0);
        
        SNMPPacketInterceptor interc1 = new SNMPPacketInterceptor() {
            public void packetReceived(Object o) {
                primary.release();
            }
        };
        final JAXBContext jaxbContext = JAXBContext.newInstance("com.sun.soabi.snmpbc.traps", this.getClass().getClassLoader());
        SNMPCallback cb = new EmptyCallback() {
            @Override
            public void deliverTraps(String batchId, String trapProcessorID, Source trapsobj) {
                try {
                    Traps traps = (Traps) jaxbContext.createUnmarshaller().unmarshal(trapsobj);
                    end.release(traps.getTrap().size());
                } catch (JAXBException e) {
                    e.printStackTrace();
                }
            }
        };
        
        // Start listening
        SNMPRA ra = new SNMPEngineFactory().create();
        ra.setPrimaryInterceptor(interc1);
        destroyOnExit(ra);
        ra.setPort(pickPort());
        ra.setBatchSize(100);
        ra.setMaxWait(100);
        ra.setNThreads(1);
        ra.setQueryInterceptor(new QuerySimulator());
        ra.start(cb);
        

        TrapProducer p = new TrapProducer(NBUFFERS, NTRAPSPERBUFFER, 1, WAIT, ra);
        p.sendWithSemaphore(primary);

        // Wait for all traps to be processed 
        assertTrue(end.tryAcquire(p.getNK(), 30, TimeUnit.SECONDS));
        long t2 = System.nanoTime();
        
        // Done
        ra.stop();
        p.done();
        p.dump("1/double-sided/w.jaxb", t2);
    }

    /**
     * Sends V1 traps to the engine; forwards traps to a trap processor, so 
     * this includes JAXB creation. Also does JAXB decoding. 
     * Uses internal network loopback. Primary and secondary semaphore sync
     * 
     * @throws Throwable
     */
    public void testV1TrapWithQuery() throws Throwable {
        // Mechanism to intercept traps
        final Semaphore primary = new Semaphore(0);
        final Semaphore end = new Semaphore(0);
        
        // LOGGER
        //setLogLevel(SNMPRAImpl.class.getName(), Level.FINE);
        
        SNMPPacketInterceptor interc1 = new SNMPPacketInterceptor() {
            public void packetReceived(Object o) {
                primary.release();
            }
        };
        final ScheduledThreadPoolExecutor threadpool = new ScheduledThreadPoolExecutor(10);
        final JAXBContext jaxbContext = JAXBContext.newInstance("com.sun.soabi.snmpbc.traps", this.getClass().getClassLoader());
        final JAXBContext jaxbContextRequest = JAXBContext.newInstance("com.sun.soabi.snmpbc.metadataquery", this.getClass().getClassLoader());
        final JAXBContext jaxbContextReply = JAXBContext.newInstance("com.sun.soabi.snmpbc.metadataresponse", this.getClass().getClassLoader());
        final SNMPRA ra = new SNMPEngineFactory().create();
        final AtomicInteger queriedAddresses = new AtomicInteger();
        SNMPCallback cb = new EmptyCallback() {
            @Override
            public void deliverTraps(String batchId, String trapProcessorID, Source trapsobj) {
                try {
                    Traps traps = (Traps) jaxbContext.createUnmarshaller().unmarshal(trapsobj);
                    end.release(traps.getTrap().size());
                } catch (JAXBException e) {
                    e.printStackTrace();
                }
            }

            @Override
            public void getMetaData(final String queryId, Source query) {
                try {
                    Object o  = jaxbContextRequest.createUnmarshaller().unmarshal(query);
                    final MetaDataQuery q = (MetaDataQuery) (o instanceof JAXBElement 
                    ? ((JAXBElement) o).getValue() : (MetaDataQuery) o);
                    
                    threadpool.execute(new Runnable() {
                        public void run() {
                            MetaDataResponse resp = new MetaDataResponse();
                            queriedAddresses.addAndGet(q.getNetworkElementIdents().size());
                            for (NetworkElementIdent id : q.getNetworkElementIdents()) {
                                NetworkElementProperties el = new NetworkElementProperties();
                                el.getProcessorID().add("A");
                                el.setReplyID(id.getReplyID());
                                resp.getNetworkElements().add(el);
                            }
                            try {
                                com.sun.soabi.snmpbc.metadataresponse.ObjectFactory objectFactory = new com.sun.soabi.snmpbc.metadataresponse.ObjectFactory();
                                JAXBElement<MetaDataResponse> jaxbElement = objectFactory.createMetaDataResponse(resp);
                                JAXBSource source = new JAXBSource(jaxbContextReply, jaxbElement);
                                ra.replyMetadata(queryId, source);
                            } catch (JAXBException e) {
                                e.printStackTrace();
                            }
                        }
                    });
                } catch (JAXBException e1) {
                    e1.printStackTrace();
                }
            }
        };
        
        // Start listening
        ra.setPrimaryInterceptor(interc1);
        destroyOnExit(ra);
        ra.setPort(pickPort());
        ra.setBatchSize(10000);
        ra.setMaxWait(1000);
        ra.setNThreads(1);
        ra.start(cb);
        
        TrapProducer p = new TrapProducer(NBUFFERS, NTRAPSPERBUFFER, 1, WAIT, ra);
        p.sendWithSemaphore(primary);

        // Wait for all traps to be processed 
        assertTrue(end.tryAcquire(p.getNK(), 30, TimeUnit.SECONDS));
        long t2 = System.nanoTime();
        
        // Done
        ra.stop();
        p.done();
        p.dump("double-sided/w.query", t2);
        
        assertTrue(queriedAddresses.get() == 1);
    }

    /**
     * Sends V1 traps to the engine; forwards traps to a trap processor, so 
     * this includes JAXB creation. Also does JAXB decoding. 
     * Uses internal network loopback
     * 
     * @throws Throwable
     */
    public void testV1TrapWithMultipleQuery() throws Throwable {
        // Mechanism to intercept traps
        final Semaphore primary = new Semaphore(0);
        final Semaphore end = new Semaphore(0);
        
        // LOGGER
        //setLogLevel(SNMPRAImpl.class.getName(), Level.FINE);
        
        SNMPPacketInterceptor interc1 = new SNMPPacketInterceptor() {
            public void packetReceived(Object o) {
                primary.release();
            }
        };
        final ScheduledThreadPoolExecutor threadpool = new ScheduledThreadPoolExecutor(10);
        final Coder c = new Coder();
        final SNMPRA ra = new SNMPEngineFactory().create();
        final AtomicInteger queriedAddresses = new AtomicInteger();
        SNMPCallback cb = new EmptyCallback() {
            @Override
            public void deliverTraps(String batchId, String trapProcessorID, Source trapsobj) {
                try {
                    Traps traps = (Traps) c.jaxbContext.createUnmarshaller().unmarshal(trapsobj);
                    end.release(traps.getTrap().size());
                } catch (JAXBException e) {
                    e.printStackTrace();
                }
            }

            @Override
            public void getMetaData(final String queryId, Source query) {
                try {
                    Object o  = c.jaxbContextRequest.createUnmarshaller().unmarshal(query);
                    final MetaDataQuery q = (MetaDataQuery) (o instanceof JAXBElement 
                    ? ((JAXBElement) o).getValue() : (MetaDataQuery) o);
                    
                    threadpool.execute(new Runnable() {
                        public void run() {
                            MetaDataResponse resp = new MetaDataResponse();
                            queriedAddresses.addAndGet(q.getNetworkElementIdents().size());
                            for (NetworkElementIdent id : q.getNetworkElementIdents()) {
                                NetworkElementProperties el = new NetworkElementProperties();
                                el.getProcessorID().add("A");
                                el.setReplyID(id.getReplyID());
                                resp.getNetworkElements().add(el);
                            }
                            try {
                                com.sun.soabi.snmpbc.metadataresponse.ObjectFactory objectFactory = new com.sun.soabi.snmpbc.metadataresponse.ObjectFactory();
                                JAXBElement<MetaDataResponse> jaxbElement = objectFactory.createMetaDataResponse(resp);
                                JAXBSource source = new JAXBSource(c.jaxbContextReply, jaxbElement);
                                ra.replyMetadata(queryId, source);
                            } catch (JAXBException e) {
                                e.printStackTrace();
                            }
                        }
                    });
                } catch (JAXBException e1) {
                    e1.printStackTrace();
                }
            }
        };
        
        // Start listening
        ra.setPrimaryInterceptor(interc1);
        destroyOnExit(ra);
        ra.setPort(pickPort());
        ra.setBatchSize(10000);
        ra.setMaxWait(1000);
        ra.setNThreads(1);
        ra.start(cb);
        
        TrapProducer p = new TrapProducer(NBUFFERS, NTRAPSPERBUFFER, 1, WAIT, ra);
        
        p.sendWithSemaphore(primary);

        // Wait for all traps to be processed 
        assertTrue(end.tryAcquire(p.getNK(), 300, TimeUnit.SECONDS)); ///////////////// 300
        long t2 = System.nanoTime();
        
        p.dump("double-sided/w.query", t2);
        
        assertTrue(queriedAddresses.get() == 1);
        
        // Done
        ra.stop();
    }
    
    /**
     * Sends V1 traps to the engine; forwards traps to a trap processor, so 
     * this includes JAXB creation. Also does JAXB decoding. 
     * Uses internal network loopback
     * 
     * @throws Throwable
     */
    public void testV1TrapWithMultipleQuerySyncPoint() throws Throwable {
        // Mechanism to intercept traps
        final Semaphore end = new Semaphore(0);
        
        // LOGGER
        //setLogLevel(SNMPRAImpl.class.getName(), Level.FINE);
        
        final ScheduledThreadPoolExecutor threadpool = new ScheduledThreadPoolExecutor(10);
        final Coder c = new Coder();
        final SNMPRA ra = new SNMPEngineFactory().create();
        final AtomicInteger queriedAddresses = new AtomicInteger();
        SNMPCallback cb = new EmptyCallback() {
            @Override
            public void deliverTraps(String batchId, String trapProcessorID, Source trapsobj) {
                try {
                    Traps traps = (Traps) c.jaxbContext.createUnmarshaller().unmarshal(trapsobj);
                    end.release(traps.getTrap().size());
                } catch (JAXBException e) {
                    e.printStackTrace();
                }
            }

            @Override
            public void getMetaData(final String queryId, Source query) {
                try {
                    Object o  = c.jaxbContextRequest.createUnmarshaller().unmarshal(query);
                    final MetaDataQuery q = (MetaDataQuery) (o instanceof JAXBElement 
                    ? ((JAXBElement) o).getValue() : (MetaDataQuery) o);
                    
                    threadpool.execute(new Runnable() {
                        public void run() {
                            MetaDataResponse resp = new MetaDataResponse();
                            queriedAddresses.addAndGet(q.getNetworkElementIdents().size());
                            for (NetworkElementIdent id : q.getNetworkElementIdents()) {
                                NetworkElementProperties el = new NetworkElementProperties();
                                el.getProcessorID().add("A");
                                el.setReplyID(id.getReplyID());
                                resp.getNetworkElements().add(el);
                            }
                            try {
                                com.sun.soabi.snmpbc.metadataresponse.ObjectFactory objectFactory = new com.sun.soabi.snmpbc.metadataresponse.ObjectFactory();
                                JAXBElement<MetaDataResponse> jaxbElement = objectFactory.createMetaDataResponse(resp);
                                JAXBSource source = new JAXBSource(c.jaxbContextReply, jaxbElement);
                                ra.replyMetadata(queryId, source);
                            } catch (JAXBException e) {
                                e.printStackTrace();
                            }
                        }
                    });
                } catch (JAXBException e1) {
                    e1.printStackTrace();
                }
            }
        };
        
        // Start listening
        destroyOnExit(ra);
        ra.setPort(pickPort());
        ra.setBatchSize(100);
        ra.setMaxWait(100);
        ra.setNThreads(4);
        ra.start(cb);

        // Test with multiple sockets
        {
            int nSockets = 100;
            TrapProducer p = new TrapProducer(NBUFFERS, NTRAPSPERBUFFER, nSockets, WAIT, ra);
            p.sendWithSyncPacket();
            assertTrue(end.tryAcquire(p.getNK(), 300, TimeUnit.SECONDS)); ///////////////// 300
            p.done();
            assertTrue(queriedAddresses.get() == Math.min(p.getNK(), nSockets));
            p.dump("double-sided-sync/w.query," + nSockets + "sockets", System.nanoTime());
        }

        // Done
        ra.stop();
    }

    /**
     * Sends V1 traps to the engine; forwards traps to a trap processor, so 
     * this includes JAXB creation. Also does JAXB decoding. 
     * Uses internal network loopback
     * 
     * @throws Throwable
     */
    public void testV1TrapWithMultipleQuerySyncPoint2() throws Throwable {
        // Mechanism to intercept traps
        final Semaphore end = new Semaphore(0);
        
        // LOGGER
        //setLogLevel(SNMPRAImpl.class.getName(), Level.FINE);
        
        final ScheduledThreadPoolExecutor threadpool = new ScheduledThreadPoolExecutor(10);
        final Coder c = new Coder();
        final SNMPRA ra = new SNMPEngineFactory().create();
        final AtomicInteger queriedAddresses = new AtomicInteger();
        SNMPCallback cb = new EmptyCallback() {
            @Override
            public void deliverTraps(String batchId, String trapProcessorID, final Source trapsobj) {
                threadpool.execute(new Runnable() {
                    public void run() {
                        try {
                            Traps traps = (Traps) c.jaxbContext.createUnmarshaller().unmarshal(trapsobj);
                            end.release(traps.getTrap().size());
                        } catch (JAXBException e) {
                            e.printStackTrace();
                        }
                    }
                });
            }

            @Override
            public void getMetaData(final String queryId, Source query) {
                try {
                    Object o  = c.jaxbContextRequest.createUnmarshaller().unmarshal(query);
                    final MetaDataQuery q = (MetaDataQuery) (o instanceof JAXBElement 
                    ? ((JAXBElement) o).getValue() : (MetaDataQuery) o);
                    
                    threadpool.execute(new Runnable() {
                        public void run() {
                            MetaDataResponse resp = new MetaDataResponse();
                            queriedAddresses.addAndGet(q.getNetworkElementIdents().size());
                            for (NetworkElementIdent id : q.getNetworkElementIdents()) {
                                NetworkElementProperties el = new NetworkElementProperties();
                                el.getProcessorID().add("A");
                                el.setReplyID(id.getReplyID());
                                resp.getNetworkElements().add(el);
                            }
                            try {
                                com.sun.soabi.snmpbc.metadataresponse.ObjectFactory objectFactory = new com.sun.soabi.snmpbc.metadataresponse.ObjectFactory();
                                JAXBElement<MetaDataResponse> jaxbElement = objectFactory.createMetaDataResponse(resp);
                                JAXBSource source = new JAXBSource(c.jaxbContextReply, jaxbElement);
                                ra.replyMetadata(queryId, source);
                            } catch (JAXBException e) {
                                e.printStackTrace();
                            }
                        }
                    });
                } catch (JAXBException e1) {
                    e1.printStackTrace();
                }
            }
        };
        
        // Start listening
        destroyOnExit(ra);
        ra.setPort(pickPort());
        ra.setBatchSize(10);
        ra.setMaxWait(100);
        ra.setNThreads(4);
        ra.start(cb);

        // Test with multiple sockets
        {
            int nSockets = 100;
            TrapProducer p = new TrapProducer(NBUFFERS, NTRAPSPERBUFFER, nSockets, WAIT, ra);
            p.sendWithSyncPacket();
            assertTrue(end.tryAcquire(p.getNK(), 300, TimeUnit.SECONDS)); ///////////////// 300
            p.done();
            assertTrue(queriedAddresses.get() == Math.min(p.getNK(), nSockets));
            p.dump("double-sided-sync/w.query," + nSockets + "sockets", System.nanoTime());
        }

        // Done
        ra.stop();
    }

    /**
     * Marshals a Trap to a Source 
     * 
     * @throws Throwable
     */
    public void testCreateTrap() throws Throwable {
        try {
            // Create a trap
            Traps t = new Traps();
            Trap trap = new Trap();
            trap.setUDPPort(-1);
            t.getTrap().add(trap);
            
            JAXBContext jaxbContext = JAXBContext.newInstance("com.sun.soabi.snmpbc.traps",
                this.getClass().getClassLoader());
            
            // Marshal to a DOM
            DocumentBuilderFactory fact = DocumentBuilderFactory.newInstance();
            fact.setNamespaceAware(true);
            DocumentBuilder builder = fact.newDocumentBuilder();
            Document d = builder.newDocument();
            Marshaller marshaller = jaxbContext.createMarshaller();
            marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, new Boolean(true));
            marshaller.marshal(t, d);
            
            // Visually verify
            // printDOM(d);
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }
    
    public void runTest() throws Throwable {
        super.runTest();
    }
    
    /**
     * 
     * 
     * @throws Throwable
     */
    public void xtestListen() throws Throwable {
        // Mechanism to intercept traps
        final Semaphore end = new Semaphore(0);
        
        // LOGGER
        //setLogLevel(SNMPRAImpl.class.getName(), Level.FINE);
        
        final ScheduledThreadPoolExecutor threadpool = new ScheduledThreadPoolExecutor(10);
        final Coder c = new Coder();
        final SNMPRA ra = new SNMPEngineFactory().create();
        final AtomicInteger queriedAddresses = new AtomicInteger();
        SNMPCallback cb = new EmptyCallback() {
            @Override
            public void deliverTraps(String batchId, String trapProcessorID, final Source trapsobj) {
                threadpool.execute(new Runnable() {
                    public void run() {
                        try {
                            Traps traps = (Traps) c.jaxbContext.createUnmarshaller().unmarshal(trapsobj);
                            end.release(traps.getTrap().size());
                        } catch (JAXBException e) {
                            e.printStackTrace();
                        }
                    }
                });
            }

            @Override
            public void getMetaData(final String queryId, Source query) {
                try {
                    Object o  = c.jaxbContextRequest.createUnmarshaller().unmarshal(query);
                    final MetaDataQuery q = (MetaDataQuery) (o instanceof JAXBElement 
                    ? ((JAXBElement) o).getValue() : (MetaDataQuery) o);
                    
                    threadpool.execute(new Runnable() {
                        public void run() {
                            MetaDataResponse resp = new MetaDataResponse();
                            queriedAddresses.addAndGet(q.getNetworkElementIdents().size());
                            for (NetworkElementIdent id : q.getNetworkElementIdents()) {
                                NetworkElementProperties el = new NetworkElementProperties();
                                el.getProcessorID().add("A");
                                el.setReplyID(id.getReplyID());
                                resp.getNetworkElements().add(el);
                            }
                            try {
                                com.sun.soabi.snmpbc.metadataresponse.ObjectFactory objectFactory = new com.sun.soabi.snmpbc.metadataresponse.ObjectFactory();
                                JAXBElement<MetaDataResponse> jaxbElement = objectFactory.createMetaDataResponse(resp);
                                JAXBSource source = new JAXBSource(c.jaxbContextReply, jaxbElement);
                                ra.replyMetadata(queryId, source);
                            } catch (JAXBException e) {
                                e.printStackTrace();
                            }
                        }
                    });
                } catch (JAXBException e1) {
                    e1.printStackTrace();
                }
            }
        };
        
        // Start listening
        destroyOnExit(ra);
        ra.setPort(5541);
        ra.setBatchSize(1);
        ra.setMaxWait(100);
        ra.setNThreads(1);
        ra.start(cb);
        
        Thread.sleep(3600000L);
        

        // Done
        ra.stop();
    }
    
}
