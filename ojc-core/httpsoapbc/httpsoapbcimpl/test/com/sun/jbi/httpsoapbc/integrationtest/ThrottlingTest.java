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
 * @(#)ThrottlingTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.integrationtest;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.ArrayList;

import javax.xml.namespace.QName;
import javax.xml.soap.DetailEntry;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPBody;
import javax.xml.soap.SOAPConnection;
import javax.xml.soap.SOAPConnectionFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPMessage;

import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import javax.xml.ws.Dispatch;
import javax.xml.ws.http.HTTPBinding;
import javax.xml.ws.Service;
import javax.xml.ws.WebServiceException;
import javax.xml.ws.handler.MessageContext;
import javax.xml.ws.soap.SOAPFaultException;

import com.sun.jbi.component.test.framework.OpenESBIntegrationTestBase;

/**
 *
 * Junit test cases for performance measurement implementation
 */
public class ThrottlingTest extends OpenESBIntegrationTestBase {
    
    public ThrottlingTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {        
    }

    protected void tearDown() throws Exception {
    }

    class RequestProcessor implements Callable<String> {
        private Dispatch<Source> dispatch;
        private String request;
        
        public RequestProcessor (Dispatch<Source> dispatch, String request) {
            this.dispatch = dispatch;
            this.request = request;
        }
                
        public void run() {
        }

        public String call() throws Exception {
            String resultStr = null;
            try {
                ByteArrayInputStream bais = new ByteArrayInputStream(request.getBytes());
                Source input = new StreamSource(bais);
                Source output = dispatch.invoke(input);
                System.out.println ("After invoke");
                if (output != null) {
                    StreamResult result = new StreamResult(new ByteArrayOutputStream());
                    Transformer trans = TransformerFactory.newInstance().newTransformer();
                    trans.transform(output, result);
                    ByteArrayOutputStream baos = (ByteArrayOutputStream) result.getOutputStream();
                    // Check the response content.
                    resultStr = new String(baos.toByteArray());
                }
                return resultStr;
            } catch (Throwable t) {
                throw new Exception ("error occurred", t);
            } 
        }
    }
    
    private ArrayList<String> runTest (int threads, int messages, String request) throws Exception {
        ArrayList<String> result = new ArrayList(messages);
        Service service = Service.create(new QName("http://j2ee.netbeans.org/wsdl/PassThru", "PassThruService"));
        QName echoPort = new QName("http://j2ee.netbeans.org/wsdl/PassThru", "PassThruPort");
        String url = "http://localhost:18181/PassThruService/PassThruPort";
        service.addPort(echoPort, null, url); 

        ExecutorService pool = Executors.newFixedThreadPool(threads);
        long startTime = System.currentTimeMillis();
        ArrayList<Future<String>> futures = new ArrayList<Future<String>> (threads);

        for (int i=0; i < messages; i++) {
            Dispatch<Source> dispatch = service.createDispatch(echoPort,
                                                               Source.class,
                                                               Service.Mode.MESSAGE);
            futures.add(pool.submit(new RequestProcessor(dispatch,request)));
        }        
        
        for (int i=0; i < messages; i++) {
            String resultStr = futures.get(i).get();
            result.add(resultStr);
        }        
        return result;
    }
    
    public void testThrottleSize1() throws Exception {
        final String SAT1  = "C:/ojc/open-jbi-components/ojc-core/httpsoapbc/httpsoapbcimpl/test/com/sun/jbi/httpsoapbc/integrationtest/caps/ThrottleSize1CAPS.zip";
        final int throttleSize = 1;
        final int serviceProcessTime = 10 * 1000; // ten seconds - i.e. bpel wait
        
        String saname = getAdministrationService().deployServiceAssembly(SAT1);
        getAdministrationService().startServiceAssembly(saname);
        
        // Build the message.
        String request = 
        "<soapenv:Envelope xsi:schemaLocation=\"http://schemas.xmlsoap.org/soap/envelope/ http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:pas=\"http://j2ee.netbeans.org/wsdl/PassThru\">" +
          "<soapenv:Body>" +
            "<pas:PassThruOperation>" +
              "<part1>throttle test size 1</part1>" +
            "</pas:PassThruOperation>" +
          "</soapenv:Body>" +
        "</soapenv:Envelope>";
        
        final String expectedResponse = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://schemas.xmlsoap.org/soap/envelope/ http://schemas.xmlsoap.org/soap/envelope/\">" +
          "<SOAP-ENV:Body>" +
            "<m:PassThruOperationResponse xmlns:m=\"http://j2ee.netbeans.org/wsdl/PassThru\">" +
              "<part1 xmlns:msgns=\"http://j2ee.netbeans.org/wsdl/PassThru\" xmlns:pas=\"http://j2ee.netbeans.org/wsdl/PassThru\" xmlns=\"\">throttle test size 1</part1>" +
            "</m:PassThruOperationResponse>" +
          "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";
        
        try {            
            int messages = throttleSize * 2;
            int threads = messages;            
            long startTime = System.currentTimeMillis();
            ArrayList<String> results = runTest(threads, messages, request);
            long endTime = System.currentTimeMillis();
            assert(results.size()==messages);            
            for (int i=0; i < threads; i++) {
                String result = results.get(i);
                assert (result.equals(expectedResponse));
            }
            long delta = endTime - startTime;
            long expectedTime = (messages * serviceProcessTime) / throttleSize;
            System.out.println ("Expected time to finish is " + expectedTime + " [ms].  Actual time is " + delta + " [ms].");
            assert (delta >= expectedTime);            
        } catch (Throwable t) {
            fail (t.getMessage());
        } finally {
            getAdministrationService().stopServiceAssembly(saname);
            getAdministrationService().shutdownServiceAssembly(saname);
            getAdministrationService().undeployServiceAssembly(saname);            
        }
    }
    
    public void testThrottleSize13() throws Exception {
        final String SAT13 = "C:/ojc/open-jbi-components/ojc-core/httpsoapbc/httpsoapbcimpl/test/com/sun/jbi/httpsoapbc/integrationtest/caps/ThrottleSize13CAPS.zip";
        final int throttleSize = 13;
        final int serviceProcessTime = 10 * 1000; // ten seconds - i.e. bpel wait
        
        String saname = getAdministrationService().deployServiceAssembly(SAT13);
        getAdministrationService().startServiceAssembly(saname);
        
        // Build the message.
        String request = 
        "<soapenv:Envelope xsi:schemaLocation=\"http://schemas.xmlsoap.org/soap/envelope/ http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:pas=\"http://j2ee.netbeans.org/wsdl/PassThru\">" +
          "<soapenv:Body>" +
            "<pas:PassThruOperation>" +
              "<part1>throttle test size 13</part1>" +
            "</pas:PassThruOperation>" +
          "</soapenv:Body>" +
        "</soapenv:Envelope>";

        final String expectedResponse = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://schemas.xmlsoap.org/soap/envelope/ http://schemas.xmlsoap.org/soap/envelope/\">" +
          "<SOAP-ENV:Body>" +
            "<m:PassThruOperationResponse xmlns:m=\"http://j2ee.netbeans.org/wsdl/PassThru\">" +
              "<part1 xmlns:msgns=\"http://j2ee.netbeans.org/wsdl/PassThru\" xmlns:pas=\"http://j2ee.netbeans.org/wsdl/PassThru\" xmlns=\"\">throttle test size 13</part1>" +
            "</m:PassThruOperationResponse>" +
          "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";
        
        try {            
            int messages = throttleSize * 2;
            int threads = messages;            
            long startTime = System.currentTimeMillis();
            ArrayList<String> results = runTest(threads, messages, request);
            long endTime = System.currentTimeMillis();
            assert(results.size()==messages);            
            for (int i=0; i < threads; i++) {
                String result = results.get(i);
                assert (result.equals(expectedResponse));
            }
            long delta = endTime - startTime;
            long expectedTime = (messages * serviceProcessTime) / throttleSize;
            System.out.println ("Expected time to finish is " + expectedTime + " [ms].  Actual time is " + delta + " [ms].");
            assert (delta >= expectedTime);            
        } catch (Throwable t) {
            fail (t.getMessage());
        } finally {
            getAdministrationService().stopServiceAssembly(saname);
            getAdministrationService().shutdownServiceAssembly(saname);
            getAdministrationService().undeployServiceAssembly(saname);            
        }
    }      
}
