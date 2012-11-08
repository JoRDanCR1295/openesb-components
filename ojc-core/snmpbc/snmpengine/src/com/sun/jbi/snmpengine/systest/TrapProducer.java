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
 * @(#)TrapProducer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpengine.systest;

import com.sun.jbi.snmpengine.SNMPRA;
import com.sun.jbi.snmpengine.impl.ChangedSnmpEventReportDispatcher;
import com.sun.org.apache.xerces.internal.impl.dv.util.Base64;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

/**
 * Produces large number of tests for testing
 * 
 * @author fkieviet
 */
public class TrapProducer {
    /**
     * Generated through snmptrap  -d -v  1  -c  public  localhost:5541 1.3.6.1.4.1.3.1.1 
     *   localhost 3 0 '' .1.3.6.1.2.1.1.1.0 s hello
     */
    protected static String V1TRAP = "MDsCAQAEBnB1YmxpY6QuBggrBgEEAQMBAUAEfwAAAQIBAwIBAEMB" +
                "ADATMBEGCCsGAQIBAQEABAVoZWxsbw==";

    private int nTotalBuffers;
    private int nTrapsPerBuffer;
    private int nSockets;
    private int WAIT;
    private int PORT;
    private long t0;
    private long t1;
    private DatagramSocket[] sockets;
    private byte[] buf = Base64.decode(V1TRAP);
    private InetAddress LOCALADDR = InetAddress.getByName("localhost");
    private SNMPRA ra;

    /**
     * Constructor
     * 
     * @param nTotalBuffers N to send
     * @param nTrapsPerBuffer K to send; total traps = N * K
     * @param nSockets how many sockets to use
     * @param waitHowLong wait how long for a sync reply
     * @param ra config
     * @throws Exception on failure
     */
    public TrapProducer(int nTotalBuffers, int nTrapsPerBuffer, int nSockets, 
        int waitHowLong, SNMPRA ra) throws Exception {
        this.nTotalBuffers = nTotalBuffers;
        this.nTrapsPerBuffer = nTrapsPerBuffer;
        this.WAIT = waitHowLong;
        this.PORT = ra.getPort();
        this.nSockets = nSockets;
        this.sockets = new DatagramSocket[nSockets];
        this.ra = ra;

        // Init sockets
        for (int i = 0; i < nSockets; i++) {
            sockets[i] = new DatagramSocket(); 
        }
        sockets[0].setSoTimeout(WAIT);
    }
    
    /*
     * constructor used to create sockets based on startPort and multiple increment
     * so that sockets bind to port with a specific pattern
     */
    public TrapProducer(int nTotalBuffers, int nTrapsPerBuffer, int nSockets, 
        int waitHowLong, SNMPRA ra, int startPort, int multiple) throws Exception {
        this.nTotalBuffers = nTotalBuffers;
        this.nTrapsPerBuffer = nTrapsPerBuffer;
        this.WAIT = waitHowLong;
        this.PORT = ra.getPort();
        this.nSockets = nSockets;
        this.sockets = new DatagramSocket[nSockets];
        this.ra = ra;

        // Init sockets
        int numSuccess = 0;
        int curPort = startPort;
        while (numSuccess < nSockets) {
            try {
                DatagramSocket socket = new DatagramSocket(curPort);
                sockets[numSuccess] = socket;
                sockets[numSuccess].setSoTimeout(WAIT);
            } catch (SocketException se) {
                continue;
            } finally {
                curPort = curPort + multiple;
            }
            numSuccess++;
        }
    }
    
    /**
     * Send traps; use a semaphore for syncing
     * 
     * @param primary semaphore
     * @throws Exception on failure
     */
    public void sendWithSemaphore(Semaphore primary) throws Exception {
        DatagramPacket request = new DatagramPacket(buf, buf.length, LOCALADDR, PORT);

        t0 = System.nanoTime();
        int k = 0;
        for (int i = 0; i < nTotalBuffers; i++) {
            // Send a buffer of traps
            for (int j = 0; j < nTrapsPerBuffer; j++) {
                if (k == nSockets) {
                    k = 0;
                }
                sockets[k].send(request);
                k++;
            }
            
            if (!primary.tryAcquire(nTrapsPerBuffer, WAIT, TimeUnit.MILLISECONDS)) {
                throw new Exception("Timeout");
            }
        }
        t1 = System.nanoTime();
    }

    /**
     * Sends traps; uses a sync packet to sync
     * 
     * @throws Exception on failure
     */
    public void sendWithSyncPacket() throws Exception {
        DatagramPacket request = new DatagramPacket(buf, buf.length, LOCALADDR, PORT);

        DatagramPacket syncpoint = new DatagramPacket(
            ChangedSnmpEventReportDispatcher.SYNCPOINTBUF, 
            ChangedSnmpEventReportDispatcher.SYNCPOINTBUF.length,
            LOCALADDR, 
            PORT);
        DatagramPacket syncpointReadback = new DatagramPacket(
            ChangedSnmpEventReportDispatcher.SYNCPOINTBUF, 
            ChangedSnmpEventReportDispatcher.SYNCPOINTBUF.length,
            LOCALADDR, 
            PORT);

        t0 = System.nanoTime();
        int k = 0;
        for (int i = 0; i < nTotalBuffers; i++) {
            // Send a buffer of traps
            for (int j = 0; j < nTrapsPerBuffer; j++) {
                if (k == nSockets) {
                    k = 0;
                }
                sockets[k].send(request);
                k++;
            }
            
            // Send sync packet
            sockets[0].send(syncpoint);
            
            // Wait for sync reply
            sockets[0].receive(syncpointReadback);
        }
        t1 = System.nanoTime();
    }
    
    /**
     * Frees up used resources
     */
    public void done() {
        for (DatagramSocket s : sockets) {
            s.close();
        }
    }
    
    /**
     * @return total number of traps to be sent
     */
    public int getNK() {
        return nTotalBuffers * nTrapsPerBuffer;
    }
    
    /**
     * Writes performance metrics to system.out
     * 
     * @param prefix test annotation
     * @param t2 end time of processing (obtained through System.nanotime())
     */
    public void dump(String prefix, long t2) {
        double rate1 = (double) nTotalBuffers * nTrapsPerBuffer / (double) (t1 - t0) * 1E9D;
        System.out.printf("%s\trecv\t\t%d buffers of %d packets (%d total) of %d bytes " +
                        "in %f sec = %f packets / sec = %g bytes/sec %s\n"
            , prefix, nTotalBuffers, nTrapsPerBuffer, nTotalBuffers * nTrapsPerBuffer, buf.length
            , ((double) (t1 - t0) / 1E9D), rate1, rate1 * buf.length, ra.toString());

        if (t2 > 0) {
            double rate2 = (double) nTotalBuffers * nTrapsPerBuffer / (double) (t2 - t0) * 1E9D;
            System.out.printf("%s\tproc\t\t%d buffers of %d packets (%d total) of %d bytes " +
                        "in %f sec = %f packets / sec = %g bytes/sec %s\n"
                , prefix, nTotalBuffers, nTrapsPerBuffer, nTotalBuffers * nTrapsPerBuffer, buf.length
                , ((double) (t2 - t0) / 1E9D), rate2, rate2 * buf.length, ra.toString());
        }
    }
}
