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
 * @(#)MonitorManagementTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.integrationtest;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import java.util.Properties;
import java.util.Collection;
import java.util.Iterator;

import javax.management.ObjectName;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularData;

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
import com.sun.jbi.component.test.framework.OpenESBMBeanHelper;

/**
 *
 * Junit test cases for performance measurement implementation
 */
public class MonitorManagementTest extends OpenESBIntegrationTestBase {

    private static final String SUN_HTTP_BINDING = "sun-http-binding";

    // Simple echo SA
    // 0 provisioning endpoint
    // 1 consuming endoint
    private static final String SA = "C:/ojc/open-jbi-components/ojc-core/httpsoapbc/httpsoapbcimpl/test/com/sun/jbi/httpsoapbc/integrationtest/caps/HttpEchoCAPS.zip";
    private String saname;
    
    public MonitorManagementTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        saname = getAdministrationService().deployServiceAssembly(SA);
        getAdministrationService().startServiceAssembly(saname);
    }

    protected void tearDown() throws Exception {
        getAdministrationService().stopServiceAssembly(saname);
        getAdministrationService().shutdownServiceAssembly(saname);
        getAdministrationService().undeployServiceAssembly(saname);
    }

    /**
     * Test of getting performance instrumentation measurement categories
     */
    public void testGetPerformanceMeasurementCategories() throws Exception {
        ObjectName statusProviderMBean = OpenESBMBeanHelper.getBindingStatusMBeanObjectName(SUN_HTTP_BINDING);
        String operation = "getPerformanceMeasurementCategories";
        Object [] paramVals  = null;
        String [] paramTypes = null;
        Properties jmxConnProps = getConnectionProperties();
        String [] categories = (String[])OpenESBMBeanHelper.invokeMBeanOperation(statusProviderMBean, operation, paramVals, paramTypes, jmxConnProps);
        assert (categories.length == 2);
        assert (categories[0].equals("Denormalization") || categories[1].equals("Normalization"));
        assert (categories[1].equals("Denormalization") || categories[1].equals("Normalization"));
    }

    /**
     * Test of getting provisioning endpoints
     */
    public void testGetProvisioningEndpoints() throws Exception {
        ObjectName statusProviderMBean = OpenESBMBeanHelper.getBindingStatusMBeanObjectName(SUN_HTTP_BINDING);
        String attribute = "ProvisioningEndpoints";
        Properties jmxConnProps = getConnectionProperties();
        String [] endpoints = (String[])OpenESBMBeanHelper.getMBeanAttribute(statusProviderMBean, attribute, jmxConnProps);
        assert (endpoints.length == 0);
    }

    /**
     * Test of getting consuming endpoints
     */
    public void testGetConsumingEndpoints() throws Exception {
        String expectedEndpoint = "http://j2ee.netbeans.org/wsdl/echo,echoService,echoPort,Consumer";
        ObjectName statusProviderMBean = OpenESBMBeanHelper.getBindingStatusMBeanObjectName(SUN_HTTP_BINDING);
        String attribute = "ConsumingEndpoints";
        Properties jmxConnProps = getConnectionProperties();
        String [] endpoints = (String[])OpenESBMBeanHelper.getMBeanAttribute(statusProviderMBean, attribute, jmxConnProps);
        assert (endpoints.length == 1);
        assert (endpoints[0].equals(expectedEndpoint));
    }
    
    /**
     * Test of getting the main WSDL for the endpoint
     */
    public void testGetWSDLDefinition() throws Exception {
        String endpoint = "http://j2ee.netbeans.org/wsdl/echo,echoService,echoPort,Consumer";
        String operation = "getWSDLDefinition";
        ObjectName statusProviderMBean = OpenESBMBeanHelper.getBindingStatusMBeanObjectName(SUN_HTTP_BINDING);
        Object [] paramVals  = new Object [] {endpoint};
        String [] paramTypes = new String [] {endpoint.getClass().getName()};
        Properties jmxConnProps = getConnectionProperties();
        String wsdl = (String)OpenESBMBeanHelper.invokeMBeanOperation(statusProviderMBean, operation, paramVals, paramTypes, jmxConnProps);
        System.out.println ("========== START wsdl\n" + wsdl + "\n============ END wsdl\n");
    }

    public void testGetPerformanceMeasurement() throws Exception {
        try {
            // Start by invoking the operation on endpoint to get some performance numbers
            Service service = Service.create(new QName("http://j2ee.netbeans.org/wsdl/echo", "echoService"));
            QName echoPort = new QName("http://j2ee.netbeans.org/wsdl/echo", "echoPort");
            String url = "http://localhost:18181/echoService/echoPort";
            service.addPort(echoPort, null, url); 
            Dispatch<Source> dispatch = service.createDispatch(echoPort,
                                                               Source.class,
                                                               Service.Mode.PAYLOAD);

            SOAPMessage outSoapMsg = MessageFactory.newInstance().createMessage();
            
            // Build the message.
            String request = 
                    "<echo:echoOperation xmlns:echo=\"http://j2ee.netbeans.org/wsdl/echo\">" +                    
                    "<part1>Hello World</part1>" +
                    "</echo:echoOperation>";
            
            final String expectedResponse = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +
                                            "<m:echoOperationResponse xmlns:m=\"http://j2ee.netbeans.org/wsdl/echo\">" +
                                            "<part1>Hello World</part1></m:echoOperationResponse>";
            
            ByteArrayInputStream bais = new ByteArrayInputStream(request.getBytes());
            Source input = new StreamSource(bais);

            // Invoke the operation n times
            int nTimes = 10;
            Source output = null;
            for (int i=0; i < nTimes; i++) {
                output = dispatch.invoke(input);
                bais.reset();
            }

            assert (output != null);
            
            System.out.println ("After invokes");
            
            // Process the last response.
            StreamResult result = new StreamResult(new ByteArrayOutputStream());
            Transformer trans = TransformerFactory.newInstance().newTransformer();
            trans.transform(output, result);
            ByteArrayOutputStream baos = (ByteArrayOutputStream) result.getOutputStream();

            // Check the response content.
            String responseContent = new String(baos.toByteArray());
            assert (expectedResponse.equals(responseContent));

            // Get the performance measurements from status provider mbean
            String endpoint = "http://j2ee.netbeans.org/wsdl/echo,echoService,echoPort,Consumer";
            String operation = "getPerformanceInstrumentationMeasurement";
            ObjectName statusProviderMBean = OpenESBMBeanHelper.getBindingStatusMBeanObjectName(SUN_HTTP_BINDING);
            Object [] paramVals  = new Object [] {endpoint};
            String [] paramTypes = new String [] {endpoint.getClass().getName()};
            Properties jmxConnProps = getConnectionProperties();
            TabularData tb = (TabularData)OpenESBMBeanHelper.invokeMBeanOperation(statusProviderMBean, operation, paramVals, paramTypes, jmxConnProps);
            
            System.out.println(tb.toString());
            
            Collection rows = tb.values();
            assert (rows.size()==2);
            Iterator iter = rows.iterator();
            while (iter.hasNext()) {
                CompositeData row = (CompositeData)iter.next();
                String topic = (String)row.get("topic");
                assert (endpoint.equals(topic));
                String subtopic = (String)row.get("sub topic");
                assert ("Normalization".equals(subtopic) || "Denormalization".equals(subtopic));
                Integer n = (Integer)row.get("n");
                assert (n.intValue()==nTimes);
            }
        } catch (Throwable t) {
            fail (t.getMessage());
        }

        
    }
}
