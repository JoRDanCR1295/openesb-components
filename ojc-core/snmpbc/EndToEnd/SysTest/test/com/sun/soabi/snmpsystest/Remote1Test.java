/*
 * Remote1Test.java
 * JUnit based test
 *
 * Created on April 3, 2007, 12:27 PM
 */

package com.sun.soabi.snmpsystest;

import com.sun.soabi.snmpmonitors.ProcEng1;
import com.sun.soabi.snmpmonitor1.Stats;
import com.sun.jbi.snmpengine.systest.TrapProducer;
import com.sun.jbi.snmpengine.SNMPEngineFactory;
import com.sun.jbi.snmpengine.SNMPRA;
import java.util.*;
import junit.framework.*;

/**
 *
 * @author fkieviet
 */
public class Remote1Test extends TestCase {
    private List<SNMPRA> mToDestroy = new ArrayList<SNMPRA>();
    private List<TrapProducer> mTrapProducersToClose = new ArrayList<TrapProducer>();
    
    public Remote1Test(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
        for (SNMPRA ra: mToDestroy) {
            ra.stop();
        }
        mToDestroy.clear();

        for (TrapProducer s: mTrapProducersToClose) {
            s.done();
        }
        mTrapProducersToClose.clear();
    }
    protected void destroyOnExit(SNMPRA ra) {
        mToDestroy.add(ra);
    }

    protected void closeOnExit(TrapProducer p) {
        mTrapProducersToClose.add(p);
    }
    
    private static final int NBUFFERS = 1000;
    private static final int NTRAPSPERBUFFER = 50;
    private static final int WAIT = 30000;
    
    
    /**
     * Sends V1 traps to the engine; forwards traps to a trap processor, so 
     * this includes JAXB creation. Also does JAXB decoding. 
     * Uses internal network loopback
     * 
     * @throws Throwable
     */
    public void testV1TrapWithMultipleQuerySyncPoint() throws Throwable {
        // LOGGER
        //setLogLevel(SNMPRAImpl.class.getName(), Level.FINE);
        
        final SNMPRA ra = new SNMPEngineFactory().create();
        
        // Start listening
        destroyOnExit(ra);
        ra.setPort(5541);
        ra.setBatchSize(1);
        ra.setMaxWait(0);
        ra.setNThreads(1);
        
        Stats before = ProcEng1.getStats();

        // Test with multiple sockets
        {
            int nSockets = 100;
            TrapProducer p = new TrapProducer(NBUFFERS, NTRAPSPERBUFFER, nSockets, WAIT, ra, 10000, 2);
            p.sendWithSyncPacket();

            long t2 = -1;
            for (;;) {
                Thread.sleep(1000);
                Stats current =  ProcEng1.getStats();
                System.out.println("Invokes=" + (current.getInvokes() - before.getInvokes()) + ", items=" + (current.getItems() - before.getItems()));
                if (current.getItems() - before.getItems() >= p.getNK()) {
                    assertTrue(current.getItems() - before.getItems() == p.getNK());
                    t2 = current.getLast();
                    System.out.println("Invokes=" + (current.getInvokes() - before.getInvokes()) + ", items=" + (current.getItems() - before.getItems()));
                    break;
                }
            }

            p.done();
            p.dump("injbi" + nSockets + "sockets", t2);
        }
        
        
        // Done
        ra.stop();
    }
    
}
