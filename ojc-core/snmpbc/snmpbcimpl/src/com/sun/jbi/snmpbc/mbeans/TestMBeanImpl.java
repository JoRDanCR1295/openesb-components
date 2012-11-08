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
 * @(#)TestMBeanImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.snmpbc.mbeans;

import com.sun.jbi.snmpengine.SNMPCallback;
import com.sun.soabi.snmpbc.metadataquery.*;
import com.sun.soabi.snmpbc.metadataresponse.*;
import com.sun.soabi.snmpbc.traps.*;
import com.sun.soabi.snmpbc.getrequests.*;
import com.sun.soabi.snmpbc.getresponses.*;
import java.util.Iterator;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.util.JAXBSource;
import javax.xml.transform.Source;

/**
 * MBean used for testing purpose
 * 
 * @author echou
 */
public class TestMBeanImpl implements TestMBeanIntf {
    
    private SNMPCallback callback;
    private JAXBContext jaxbContextMetaResponse;
    private JAXBContext jaxbContextGetRequest;
    private JAXBContext jaxbContextGetResponse;
    private Semaphore sem1 = new Semaphore(0);
    private Semaphore sem2 = new Semaphore(0);
    private final ExecutorService pool = Executors.newFixedThreadPool(5);
    
    /** Creates a new instance of TestMBeanImpl 
     * @param callback 
     * @throws java.lang.Exception 
     */
    public TestMBeanImpl(SNMPCallback callback) throws Exception {
        this.callback = callback;
        jaxbContextMetaResponse = JAXBContext.newInstance("com.sun.soabi.snmpbc.metadataresponse",
                this.getClass().getClassLoader());
        jaxbContextGetRequest = JAXBContext.newInstance("com.sun.soabi.snmpbc.getrequests",
                this.getClass().getClassLoader());
        jaxbContextGetResponse = JAXBContext.newInstance("com.sun.soabi.snmpbc.getresponses",
                this.getClass().getClassLoader());
    }

    public void testDeliverTrapsSingle() throws Exception {
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance("com.sun.soabi.snmpbc.traps", 
                    this.getClass().getClassLoader());
            
            pool.execute(new DeliverTrap(jaxbContext));
            
            boolean success = sem1.tryAcquire(1, 100, TimeUnit.SECONDS);
            
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }

    public void testDeliverTrapsMultiple(int n) throws Exception {
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance("com.sun.soabi.snmpbc.traps", 
                    this.getClass().getClassLoader());
            
            long t0 = System.nanoTime();
            for (int i = 0; i < n; i++) {
                pool.execute(new DeliverTrap(jaxbContext));
            }
            boolean success = sem1.tryAcquire(n, 100, TimeUnit.SECONDS);
            long t1 = System.nanoTime();
            if (!success) {
                System.out.println("timed-out waiting for " + n + " trap replies");
            }
            
            double rate = (double) n / (double) (t1 - t0) * 1E9D;
            System.out.println("time = " + ((double) (t1 - t0) / 1E9D) + " seconds");
            System.out.println("rate = " + rate + " InOnly msgs / sec");
            
            
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }
    
    
    public void trapReply(boolean success) {
        if (!success) {
            System.out.println("got error on trap reply");
        }
        sem1.release();
        //System.out.println("got trap reply: " + success);
    }

    public void testGetMetaDataSingle() throws Exception {
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance("com.sun.soabi.snmpbc.metadataquery", 
                    this.getClass().getClassLoader());
            
            pool.execute(new GetMetaData(jaxbContext));
            
            boolean success = sem2.tryAcquire(1, 100, TimeUnit.SECONDS);
            
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }

    public void testGetMetaDataMultiple(int n) throws Exception {
        try {
            JAXBContext jaxbContext = JAXBContext.newInstance("com.sun.soabi.snmpbc.metadataquery", 
                    this.getClass().getClassLoader());
            
            long t0 = System.nanoTime();
            for (int i = 0; i < n; i++) {
                pool.execute(new GetMetaData(jaxbContext));
            }
            boolean success = sem2.tryAcquire(n, 100, TimeUnit.SECONDS);
            long t1 = System.nanoTime();
            if (!success) {
                System.out.println("timed-out waiting for " + n + " metadata replies");
            }
            
            double rate = (double) n / (double) (t1 - t0) * 1E9D;
            System.out.println("time = " + ((double) (t1 - t0) / 1E9D) + " seconds");
            System.out.println("rate = " + rate + " InOut msgs / sec");
            
            
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }
    
    public void replyMetadata(String queryId, Source source, boolean success) {
        //System.out.println("got query reply: queryId=" + queryId + ", " + success);
        try {
            Unmarshaller unmarshaller = null;
            synchronized (jaxbContextMetaResponse) {
                unmarshaller = jaxbContextMetaResponse.createUnmarshaller();
            }
            
            MetaDataResponse response =
                    unmarshaller.unmarshal(source, MetaDataResponse.class).getValue();
            String realQueryId = response.getQuereyID();
            
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            sem2.release();
        }
        
    }
    
    public void requestPM(String requestId, Source request) {
        try {
            Unmarshaller unmarshaller = null;
            synchronized (jaxbContextGetRequest) {
                unmarshaller = jaxbContextGetRequest.createUnmarshaller();
            }
            
            GetRequests getRequests = 
                    unmarshaller.unmarshal(request, GetRequests.class).getValue();
            GetResponses getResponses = new GetResponses();
            for (Iterator<GetRequest> iter = getRequests.getGetRequest().iterator(); iter.hasNext(); ) {
                GetRequest getRequest = iter.next();
                GetResponse getResponse = new GetResponse();
                getResponse.setRequestID(getRequest.getRequestID());
                getResponse.setResponseStatus(ResponseStatusType.OKAY);
                for (Iterator<String> oidIter = getRequest.getOIDs().iterator(); oidIter.hasNext(); ) {
                    String oid = oidIter.next();
                    ResponseVarBind varBind = new ResponseVarBind();
                    varBind.setOID(oid);
                    varBind.setType(0);
                    varBind.setValue("Hello Edward");
                    getResponse.getResponseVarBindList().add(varBind);
                }
                getResponses.getGetResponse().add(getResponse);
            }
            
            JAXBSource response = new JAXBSource(jaxbContextGetResponse, getResponses);
            
            callback.replyPM(requestId, response);
            
        } catch (Exception e) {
            e.printStackTrace();
        }
        
    }
    
    static int testBatchCounter;
    
    class DeliverTrap implements Runnable {
        JAXBContext jaxbContext;
        
        DeliverTrap(JAXBContext jaxbContext) {
            this.jaxbContext = jaxbContext;
        }
        
        public void run() {
            try {
                Traps traps = new Traps();

                // Convert into Traps object
                Trap t = new Trap();
                t.setRequestID(123456);
                t.setUDPSource("localhost");
                t.setUDPPort(5541);
                t.setType(1);
                t.setVersion(1);
                for (int i = 0; i < 2; i++) {
                    t.getValues().add(new VarBinding());
                    t.getValues().get(i).setOID("1.2.3.4.5");
                    t.getValues().get(i).setType(-1);
                    t.getValues().get(i).setValue("hello world");
                }
                traps.getTrap().add(t);

                JAXBSource source1 = new JAXBSource(jaxbContext, traps);

                // Deliver
                callback.deliverTraps(Integer.toString(testBatchCounter++), "Adaptation1", source1);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        
    }

    class GetMetaData implements Runnable {
        JAXBContext jaxbContext;
        
        GetMetaData(JAXBContext jaxbContext) {
            this.jaxbContext = jaxbContext;
        }
        
        public void run() {
            try {
                String queryId = "query1";
                MetaDataQuery query = new MetaDataQuery();

                NetworkElementIdent ident = new NetworkElementIdent();
                ident.setAgentID("agent1");
                ident.setIPAddress("localhost");
                ident.setPort(5541);
                ident.setReplyID("reply1");
                ident.setUID("12345");

                query.setQueryID(queryId);
                query.getNetworkElementIdents().add(ident);

                com.sun.soabi.snmpbc.metadataquery.ObjectFactory objectFactory =
                    new com.sun.soabi.snmpbc.metadataquery.ObjectFactory();
                JAXBElement<MetaDataQuery> jaxbElement = objectFactory.createMetaDataQuery(query);
                JAXBSource source1 = new JAXBSource(jaxbContext, jaxbElement);

                // Deliver
                callback.getMetaData(queryId, source1);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        
    }

}
