/*
 * SnmpGetBulkTest.java
 * JUnit based test
 *
 * Created on August 23, 2007, 3:56 PM
 */

package com.sun.soabi.snmpsystest;
import com.sun.soabi.snmpbc.getrequests.*;
import com.sun.soabi.snmpbc.getresponses.*;
import com.sun.soabi.snmpagent.JUnitAgent;
import junit.framework.TestCase;
import java.util.ArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import junit.framework.*;
/**
 *
 * @author sun microsystems
 */
public class SnmpGetBulkTest extends TestCase {
    
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
    private int nonRepeatNum = 8;
    private int maxRepeatNum = 0;
    
    private JUnitAgent snmpAgent;
    
    // setup webservice calls
    org.netbeans.j2ee.wsdl.junitwsdl.JunitWSDLService service = new org.netbeans.j2ee.wsdl.junitwsdl.JunitWSDLService();
    org.netbeans.j2ee.wsdl.junitwsdl.JunitWSDLPortType port = service.getJunitWSDLPort();
    
  
    
    public SnmpGetBulkTest(String testName) {
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
    
    public void testSnmpv3GetBulkRequest() throws Throwable {
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
    
     // verify that the response object is indeed successful result of requests
    private void verifyRequestsResponsesResult(GetRequests requests, GetResponses responses) {
        assertEquals(requests.getGetRequest().size(), responses.getGetResponse().size());
        
        for (int i = 0; i < requests.getGetRequest().size(); i++) {
            GetRequest request = requests.getGetRequest().get(i);
            GetResponse response = responses.getGetResponse().get(i);
            System.out.println("My Test....");
            System.out.println("Request ID  "+request.getRequestID());
            System.out.println("Response ID  "+response.getRequestID());
            
            System.out.println("Resonse status "+response.getResponseStatus());
            System.out.println("Response Type  "+ ResponseStatusType.OKAY);
            
            System.out.println("Request OIDs  "+request.getOIDs().size());
            System.out.println("Response OIDs  "+ response.getResponseVarBindList().size());
            
            assertEquals(request.getRequestID(), response.getRequestID());
            assertEquals(response.getResponseStatus(), ResponseStatusType.OKAY);
            assertTrue(response.getErrorMsg() == null);
            assertEquals(request.getOIDs().size(), response.getResponseVarBindList().size());
            
            for (int j = 0; j < request.getOIDs().size(); j++) {
                String requestOID = request.getOIDs().get(j);
                ResponseVarBind varBind = response.getResponseVarBindList().get(j);
           
                System.out.println(requestOID + " : " + varBind.getOID());
                assertTrue(varBind.getErrorMsg() == null);
                //assertEquals(requestOID, varBind.getOID());
                assertTrue(varBind.getValue() != null);

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
            request.setRequestType(RequestType.GET_BULK);
            request.setRequestID(Integer.toString(i));
            request.setHost(snmpHost);
            request.setPort(snmpPort);
            request.setNonRepeat(nonRepeatNum);
            request.setMaxRepeat(maxRepeatNum);
            request.setRdCommunity(readCommunity);
            request.getOIDs().addAll(oids);
            
            requests.getGetRequest().add(request);
            i++;
        }
        
        return requests;
    }
}
