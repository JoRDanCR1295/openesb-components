
package com.sun.jbi.snmpbc.engine.test;

import com.sun.jdmk.tasks.DaemonTaskServer;
import com.sun.management.internal.snmp.SnmpAgentEngineFactory;
import com.sun.management.internal.snmp.SnmpLcd;
import com.sun.management.internal.snmp.SnmpSecuritySubSystem;
import com.sun.management.snmp.SnmpEngine;
import com.sun.management.snmp.SnmpEngineId;
import com.sun.management.snmp.SnmpEngineParameters;
import com.sun.management.snmp.SnmpEventReportDispatcher;
import com.sun.management.snmp.SnmpPduRequest;
import com.sun.management.snmp.SnmpPduTrap;
import com.sun.management.snmp.SnmpScopedPduRequest;
import com.sun.management.snmp.manager.SnmpSession;
import com.sun.management.snmp.manager.SnmpTrapListener;
import com.sun.management.snmp.usm.SnmpUsmAlgorithmManager;
import com.sun.management.snmp.usm.SnmpUsmAuthAlgorithmException;
import com.sun.management.snmp.usm.SnmpUsmAuthPair;
import com.sun.management.snmp.usm.SnmpUsmEngineIdException;
import com.sun.management.snmp.usm.SnmpUsmException;
import com.sun.management.snmp.usm.SnmpUsmLcd;
import com.sun.management.snmp.usm.SnmpUsmMibTable;
import com.sun.management.snmp.usm.SnmpUsmPasswordLcd;
import com.sun.management.snmp.usm.SnmpUsmPrivAlgorithmException;
import com.sun.management.snmp.usm.SnmpUsmPrivPair;
import com.sun.management.snmp.usm.SnmpUsmSecureUser;
import com.sun.management.snmp.usm.SnmpUsmUserNameException;
import com.sun.org.apache.xerces.internal.impl.dv.util.Base64;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.util.Enumeration;
import java.util.concurrent.Semaphore;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The class shows how to: <br> - use the OidTable generated by mibgen <br> -
 * write and use an SnmpRequestHandler <br> - deal with asynchronous requests
 * <br> - listen to traps sent by an agent
 */

public class Aync2 {
    static String TESTTRAP = "MDsCAQAEBnB1YmxpY6QuBggrBgEEAQMBAUAEfwAAAQIBAwIBAEMBADATMBEGCCsGAQIBAQEABAVoZWxsbw==";

    public class TrapListenerImpl implements SnmpTrapListener {

        public void processSnmpTrapV1(SnmpPduTrap trap) {
//            for (int i = 0; i < trap.varBindList.length; i++) {
//                SnmpVarBind b = trap.varBindList[i];
//            }
            synchronized (Aync2.this) {
                mNReceived++;
            }
            
            mRec.release();
        }

        public void processSnmpTrapV2(SnmpPduRequest trap) {
            println("NOTE: Trap V2 not of interest !!!");
        }

        public void processSnmpTrapV3(SnmpScopedPduRequest trap) {
            println("NOTE: TrapListenerImpl received trap V3:");
            println("\tContextEngineId : "
                + SnmpEngineId.createEngineId(trap.contextEngineId));
            println("\tContextName : " + new String(trap.contextName));
            println("\tVarBind list :");
            for (int i = 0; i < trap.varBindList.length; i++) {
                println("oid : " + trap.varBindList[i].getOid() + " val : "
                    + trap.varBindList[i].getSnmpValue());
            }
        }

        private final void println(String msg) {
            java.lang.System.out.println(msg);
            System.out.flush();
        }
    }
    
    private SnmpSession session;
    private SnmpEventReportDispatcher trapAgent ;
    private DaemonTaskServer taskServer;
    private int mNReceived;
    
    private Semaphore mRec = new Semaphore(0);
    
    final static int PORT = 5541;
    
    private class XSnmpUsmLcd implements SnmpUsmLcd {
        private SnmpUsmLcd mDelegate;

        XSnmpUsmLcd(SnmpEngine engine, SnmpLcd lcd,
            SnmpSecuritySubSystem securSys, String file) throws IllegalArgumentException {
            mDelegate = new SnmpUsmPasswordLcd(engine, securSys, lcd, file);
        }

        public void addEngine(SnmpEngineId engineId) {
            mDelegate.addEngine(engineId);
        }

        public void addUser(SnmpEngineId engineId, String userName, String securityName, String authProtocol, String authPassword, String privProtocol, String privKey, int storage, boolean template) throws SnmpUsmException {
            mDelegate.addUser(engineId, userName, securityName, authProtocol, authPassword, privProtocol, privKey, storage, template);
        }

        public void addUser(SnmpUsmSecureUser user, boolean notifyMIB) throws SnmpUsmException {
            mDelegate.addUser(user, notifyMIB);
        }

        public SnmpUsmSecureUser createNewUser(byte[] engineId, String userName) {
            return mDelegate.createNewUser(engineId, userName);
        }

        public SnmpUsmAlgorithmManager getAlgorithmManager() {
            return mDelegate.getAlgorithmManager();
        }

        public Enumeration getAllUsers() {
            return mDelegate.getAllUsers();
        }

        public int getStorageType() {
            return mDelegate.getStorageType();
        }

        public SnmpUsmSecureUser getUser(SnmpEngineId engineId, String userName) throws SnmpUsmEngineIdException, SnmpUsmUserNameException {
            return mDelegate.getUser(engineId, "kschmidt");
        }

        public SnmpUsmAuthPair getUserAuthPair(SnmpEngineId engineId, String userName) throws SnmpUsmAuthAlgorithmException, SnmpUsmEngineIdException, SnmpUsmUserNameException {
            return mDelegate.getUserAuthPair(engineId, "kschmidt");
        }

        public SnmpUsmPrivPair getUserPrivPair(SnmpEngineId engineId, String userName) throws SnmpUsmPrivAlgorithmException, SnmpUsmEngineIdException, SnmpUsmUserNameException {
            return mDelegate.getUserPrivPair(engineId, "kschmidt");
        }

        public void removeUser(SnmpEngineId engineId, String userName, boolean notifyMIB) {
            mDelegate.removeUser(engineId, userName, notifyMIB);
        }

        public void setAlgorithmManager(SnmpUsmAlgorithmManager manager) {
            mDelegate.setAlgorithmManager(manager);
        }

        public void setMibTable(SnmpUsmMibTable table) {
            mDelegate.setMibTable(table);
        }

        public void setUserAuthKeyChange(SnmpUsmSecureUser user, byte[] keyChange) {
            mDelegate.setUserAuthKeyChange(user, keyChange);
        }

        public void setUserPrivKeyChange(SnmpUsmSecureUser user, byte[] keyChange) {
            mDelegate.setUserPrivKeyChange(user, keyChange);
        }

        public void syncDataSource() {
            mDelegate.syncDataSource();
        }
    };

    
    public void start() {
        try {

            
            
            SnmpAgentEngineFactory fact = new SnmpAgentEngineFactory() {
                protected SnmpUsmLcd createUsmLcd(SnmpEngine engine, SnmpLcd lcd,
                    SnmpSecuritySubSystem securSys, String file) throws IllegalArgumentException {
                    return new XSnmpUsmLcd(engine, lcd, securSys, file);
                }
            };
            
            
            
            
            SnmpEngineParameters parameters = new SnmpEngineParameters();
            parameters.activateEncryption();
            session = new SnmpSession(parameters, fact, "SyncV3Manager session", null);
            //session = new SnmpSession("AsyncManager session");
            taskServer = new DaemonTaskServer();
            taskServer.start(Thread.NORM_PRIORITY);
//            trapAgent = new SnmpEventReportDispatcher(PORT, null, taskServer, null);
            trapAgent = new SnmpEventReportDispatcher(session.getEngine(), PORT, taskServer, null);
            trapAgent.addTrapListener(new TrapListenerImpl());
            final Thread trapThread = new Thread(trapAgent);
            trapThread.setPriority(Thread.MAX_PRIORITY);
            trapThread.start();
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }
    
    public void stop() {
        session.destroySession();

        try {
            trapAgent.close();
        } catch (Exception e) {
            e.printStackTrace();
        }
        try {
            taskServer.terminate();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    public void test100() throws Exception {
        byte[] buf = Base64.decode(TESTTRAP);
        
        final InetAddress REMOTEADDR = InetAddress.getByName("blue");
        final InetAddress LOCALADDR = InetAddress.getByName("bee");

        // SEND 
        DatagramPacket request = new DatagramPacket(buf, buf.length, REMOTEADDR, 60001);
        DatagramSocket socket = new DatagramSocket(PORT + 10, LOCALADDR);
        
        int N = 1000;
        int K = 10;
        
        long t0 = System.nanoTime();
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < K; j++) {
                socket.send(request);
            }
            mRec.acquire(K);
        }
        long t1 = System.nanoTime();
        
        socket.close();
        
        double rate = (double) N * K / (double) (t1 - t0) * 1E9D;
        System.out.printf("port %d: %d buffers of %d bytes in %f sec = %f packets / sec = %g bytes/sec\n"
            ,-1, N, buf.length, ((double) (t1 - t0) / 1E9D), rate, rate * buf.length);
    }

    public static void main(String argv[]) {

        Logger.getLogger("").setLevel(Level.ALL);
        Logger.getLogger("com.sun.management.snmp.manager.SnmpEventReportDispatcher").setLevel(Level.FINEST);
        Handler h = new ConsoleHandler();
        h.setLevel(Level.ALL);
        Logger.getLogger("").addHandler(h);
        
        try {
            Aync2 a = new Aync2();
            a.start();
            
//            a.test100();
            
            System.out.println("\n>> Press <Enter> if you want to stop" + " this SNMP manager.\n");
            java.lang.System.in.read();
            a.stop();
            
            System.out.println("Received: " + a.mNReceived);
            
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }
}
