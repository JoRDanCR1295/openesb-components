/*
 * SnmpGetTest.java
 * JUnit 4.x based test
 *
 * Created on April 19, 2007, 6:20 PM
 */

package com.sun.soabi.snmpsystest;

import com.sun.soabi.snmpbc.getrequests.*;
import com.sun.soabi.snmpbc.getresponses.*;
import com.sun.soabi.snmpagent.JUnitAgent;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import junit.framework.*;

/**
 *
 * @author echou
 */
public class SnmpGetTest extends TestCase {
    
    // MIB-2 sample OIDs
    public static final String OID_1 = "1.3.6.1.2.1.1.1.0";
    public static final String OID_2 = "1.3.6.1.2.1.1.2.0";
    public static final String OID_3 = "1.3.6.1.2.1.1.3.0";
    public static final String OID_4 = "1.3.6.1.2.1.1.4.0";
    public static final String OID_5 = "1.3.6.1.2.1.1.5.0";
    public static final String OID_6 = "1.3.6.1.2.1.1.6.0";
    public static final String OID_7 = "1.3.6.1.2.1.1.7.0";
    public static final String OID_8 = "1.3.6.1.2.1.1.8.0";
    
    private String snmpHost = "localhost";
    private int snmpPort = 8085;
    private String readCommunity = "public";
    
    private JUnitAgent snmpAgent;
    
    // setup webservice calls
    org.netbeans.j2ee.wsdl.junitwsdl.JunitWSDLService service = new org.netbeans.j2ee.wsdl.junitwsdl.JunitWSDLService();
    org.netbeans.j2ee.wsdl.junitwsdl.JunitWSDLPortType port = service.getJunitWSDLPort();
    
    /// setup stress test
    private final ExecutorService pool = Executors.newFixedThreadPool(10);
    private Semaphore sem1 = new Semaphore(0);
    private int numRequests = 20;
    private int numRepeat = 50;
    
    
    public SnmpGetTest(String testName) {
        super(testName);
    }

    @Override
    protected void setUp() throws Exception {
        // start test agent
        snmpAgent = new JUnitAgent(snmpPort);
        snmpAgent.init();
        
    }

    @Override
    protected void tearDown() throws Exception {
        snmpAgent.stop();
    }

    
    public void testSnmpV2GetSingleRequestSingleOID() throws Throwable {
        ArrayList< ArrayList<String> > data = new ArrayList< ArrayList<String> > ();
        ArrayList<String> oids = new ArrayList<String> ();
        oids.add(OID_1);
        data.add(oids);
        
        GetRequests requests = createGetRequestsObject(data);
        GetResponses responses = port.junitWSDLOperation(requests);
        
        verifyRequestsResponsesResult(requests, responses);
    }

    public void testSnmpV2GetSingleRequestMultipleOID() throws Throwable {
        ArrayList< ArrayList<String> > data = new ArrayList< ArrayList<String> > ();
        ArrayList<String> oids = new ArrayList<String> ();
        oids.add(OID_1);
        oids.add(OID_2);
        oids.add(OID_3);
        oids.add(OID_4);
        oids.add(OID_5);
        oids.add(OID_6);
        oids.add(OID_7);
        oids.add(OID_8);
        data.add(oids);
        
        GetRequests requests = createGetRequestsObject(data);
        GetResponses responses = port.junitWSDLOperation(requests);
        
        verifyRequestsResponsesResult(requests, responses);
    }
    
    public void testSnmpV2GetMultipleRequestSingleOID() throws Throwable {
        ArrayList< ArrayList<String> > data = new ArrayList< ArrayList<String> > ();
        ArrayList<String> oids1 = new ArrayList<String> ();
        oids1.add(OID_1);
        ArrayList<String> oids2 = new ArrayList<String> ();
        oids2.add(OID_2);
        ArrayList<String> oids3 = new ArrayList<String> ();
        oids3.add(OID_3);
        ArrayList<String> oids4 = new ArrayList<String> ();
        oids4.add(OID_4);
        ArrayList<String> oids5 = new ArrayList<String> ();
        oids5.add(OID_5);
        ArrayList<String> oids6 = new ArrayList<String> ();
        oids6.add(OID_6);
        ArrayList<String> oids7 = new ArrayList<String> ();
        oids7.add(OID_7);
        ArrayList<String> oids8 = new ArrayList<String> ();
        oids8.add(OID_8);
        
        data.add(oids1);
        data.add(oids2);
        data.add(oids3);
        data.add(oids4);
        data.add(oids5);
        data.add(oids6);
        data.add(oids7);
        data.add(oids8);
        
        GetRequests requests = createGetRequestsObject(data);
        GetResponses responses = port.junitWSDLOperation(requests);
        
        verifyRequestsResponsesResult(requests, responses);
    }

    public void testSnmpV2GetMultipleRequestMultipleOID() throws Throwable {
        ArrayList< ArrayList<String> > data = new ArrayList< ArrayList<String> > ();
        ArrayList<String> oids1 = new ArrayList<String> ();
        oids1.add(OID_1);
        oids1.add(OID_2);
        ArrayList<String> oids2 = new ArrayList<String> ();
        oids2.add(OID_3);
        oids2.add(OID_4);
        ArrayList<String> oids3 = new ArrayList<String> ();
        oids3.add(OID_5);
        oids3.add(OID_6);
        ArrayList<String> oids4 = new ArrayList<String> ();
        oids4.add(OID_7);
        oids4.add(OID_8);
        
        data.add(oids1);
        data.add(oids2);
        data.add(oids3);
        data.add(oids4);
        
        GetRequests requests = createGetRequestsObject(data);
        GetResponses responses = port.junitWSDLOperation(requests);
        
        verifyRequestsResponsesResult(requests, responses);
    }
    
    public void testSnmpGetStressTest() throws Throwable {
        ArrayList< ArrayList<String> > data = new ArrayList< ArrayList<String> > ();
        ArrayList<String> oids1 = new ArrayList<String> ();
        oids1.add(OID_1);
        oids1.add(OID_2);
        ArrayList<String> oids2 = new ArrayList<String> ();
        oids2.add(OID_3);
        oids2.add(OID_4);
        ArrayList<String> oids3 = new ArrayList<String> ();
        oids3.add(OID_5);
        oids3.add(OID_6);
        ArrayList<String> oids4 = new ArrayList<String> ();
        oids4.add(OID_7);
        oids4.add(OID_8);
        
        data.add(oids1);
        data.add(oids2);
        data.add(oids3);
        data.add(oids4);
        
        List<Future<GetResponses>> futures = new ArrayList<Future<GetResponses>> ();
        final GetRequests requests = createGetRequestsObject(data);
        
        System.out.println("starting stress test ...");
        
        long t0 = System.nanoTime();
        for (int i = 0; i < numRepeat; i++) {
            for (int j = 0; j < numRequests; j++) {
                Future<GetResponses> future = pool.submit(new Callable<GetResponses> () {
                    public GetResponses call() throws Exception {
                        GetResponses responses = port.junitWSDLOperation(requests);
                        sem1.release();
                        return responses;
                    }
                });
                futures.add(future);
            }
            sem1.tryAcquire(numRequests, 60, TimeUnit.SECONDS);
            System.out.println("done with " + numRequests + " requests ...");
        }
        long t1 = System.nanoTime();
        
        long n = numRepeat * numRequests;
        double rate = (double) n / (double) (t1 - t0) * 1E9D;
        System.out.println("finished stress test:");
        System.out.println("time = " + ((double) (t1 - t0) / 1E9D) + " seconds");
        System.out.println("rate = " + rate + " requests / sec");
        
        System.out.println("verifying results ...");
        for (Future<GetResponses> future : futures) {
            verifyRequestsResponsesResult(requests, future.get());
        }
        System.out.println("finished verifying results ...");
        
    }
    
    
    // verify that the response object is indeed successful result of requests
    private void verifyRequestsResponsesResult(GetRequests requests, GetResponses responses) {
        assertEquals(requests.getGetRequest().size(), responses.getGetResponse().size());
        
        for (int i = 0; i < requests.getGetRequest().size(); i++) {
            GetRequest request = requests.getGetRequest().get(i);
            GetResponse response = responses.getGetResponse().get(i);

            assertEquals(request.getRequestID(), response.getRequestID());
            assertEquals(response.getResponseStatus(), ResponseStatusType.OKAY);
            assertTrue(response.getErrorMsg() == null);
            assertEquals(request.getOIDs().size(), response.getResponseVarBindList().size());
            
            for (int j = 0; j < request.getOIDs().size(); j++) {
                String requestOID = request.getOIDs().get(j);
                ResponseVarBind varBind = response.getResponseVarBindList().get(j);

                assertTrue(varBind.getErrorMsg() == null);
                assertEquals(requestOID, varBind.getOID());
                assertTrue(varBind.getValue() != null);

                //System.out.println(requestOID + " : " + varBind.getValue());
            }
        }
    }
    
    
    // construct GetRequests object from input data, structs looks like:
    // ArrayList< ArrayList<OID> >
    private GetRequests createGetRequestsObject(ArrayList< ArrayList<String> > data) {
        GetRequests requests = new GetRequests();
        int i = 1;
        for (ArrayList<String> oids : data) {
            GetRequest request = new GetRequest();
            request.setRequestType(RequestType.GET);
            request.setRequestID(Integer.toString(i));
            request.setHost(snmpHost);
            request.setPort(snmpPort);
            request.setRdCommunity(readCommunity);
            request.getOIDs().addAll(oids);
            
            requests.getGetRequest().add(request);
            i++;
        }
        
        return requests;
    }
    
}
