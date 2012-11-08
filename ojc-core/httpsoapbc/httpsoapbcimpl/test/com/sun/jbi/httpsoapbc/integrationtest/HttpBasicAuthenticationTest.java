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

import java.net.Authenticator;
import java.net.PasswordAuthentication;

import java.util.Properties;
import java.util.Collection;
import java.util.Iterator;

import javax.management.Attribute;
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
public class HttpBasicAuthenticationTest extends OpenESBIntegrationTestBase {

    private static final String SUN_HTTP_BINDING = "sun-http-binding";

    // Simple echo SA
    // 0 provisioning endpoint
    // 1 consuming endoint
    private static final String SA = "/openesb/open-jbi-components/ojc-core/httpsoapbc/httpsoapbcimpl/test/com/sun/jbi/httpsoapbc/integrationtest/caps/BasicAuth.zip";
    private String saname;
    private static final String username = "wilma";
    private static final String password = "pebbles";
    private Authenticator authenticator = new TestAuthenticator();
    private static final String AMConfigDir = "C:\\openesb\\glassfish-v2\\domains\\domain1\\config";
    
    private class TestAuthenticator extends java.net.Authenticator {
        public TestAuthenticator() {
            setDefault(this);
        }
        protected PasswordAuthentication getPasswordAuthentication() {
            return new PasswordAuthentication(username, password.toCharArray());
        }
    }
    
    public HttpBasicAuthenticationTest(String testName) {
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

    public void testAccessManagerAuthentication() throws Exception {
        try {
            ObjectName rtcProviderMBean = OpenESBMBeanHelper.getBindingRuntimeConfigurationMBeanName(SUN_HTTP_BINDING);
            String attribute = "AMConfigDirectory";
            Attribute amConfigDirAttr = new Attribute(attribute, AMConfigDir);
            
            Properties jmxConnProps = getConnectionProperties();
            //OpenESBMBeanHelper.setMBeanAttribute(rtcProviderMBean, amConfigDirAttr, jmxConnProps);
            
            Service service = Service.create(new QName("http://j2ee.netbeans.org/wsdl/SoapBasicAuth", "SoapBasicAuthService"));
            QName echoPort = new QName("http://j2ee.netbeans.org/wsdl/SoapBasicAuth", "SoapBasicAuthPortAM");
            String url = "http://localhost:12081/SoapBasicAuthService/SoapBasicAuthAMPort";
            service.addPort(echoPort, null, url); 
            Dispatch<Source> dispatch = service.createDispatch(echoPort,
                                                               Source.class,
                                                               Service.Mode.PAYLOAD);

            SOAPMessage outSoapMsg = MessageFactory.newInstance().createMessage();
            
            // Build the message.
            String request = 
                 "<SoapBasicAuthOperation xmlns=\"http://j2ee.netbeans.org/wsdl/SoapBasicAuth\">" +
                 "<request-part>Testing HTTP Basic Authentication</request-part>" +
                 "</SoapBasicAuthOperation>";
                        
            final String expectedResponse = 
                 "<?xml version=\"1.0\" ?>" +
                 "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
                    "<SOAP-ENV:Body>" +
                       "<m:SoapBasicAuthOperationResponse xmlns:m=\"http://j2ee.netbeans.org/wsdl/SoapBasicAuth\">" +
                          "<response-part xmlns=\"\">Response from SoapBasicAuthAM</response-part>" +
                       "</m:SoapBasicAuthOperationResponse>" +
                    "</SOAP-ENV:Body>" +
                 "</SOAP-ENV:Envelope>";

            
            ByteArrayInputStream bais = new ByteArrayInputStream(request.getBytes());
            Source input = new StreamSource(bais);

            Source output = dispatch.invoke(input);
            System.out.println ("After invoke");

            assert (output != null);
            
            StreamResult result = new StreamResult(new ByteArrayOutputStream());
            Transformer trans = TransformerFactory.newInstance().newTransformer();
            trans.transform(output, result);
            ByteArrayOutputStream baos = (ByteArrayOutputStream) result.getOutputStream();
            // Check the response content.
            String responseContent = new String(baos.toByteArray());
            System.out.println("Got response: \n" + responseContent + "\n");
            assert (expectedResponse.equals(responseContent));
        } catch (Throwable t) {
            fail (t.getMessage());
        }        
    }
    
    public void testRealmAuthentication() throws Exception {
        try {            
            Service service = Service.create(new QName("http://j2ee.netbeans.org/wsdl/SoapBasicAuth", "SoapBasicAuthService"));
            QName echoPort = new QName("http://j2ee.netbeans.org/wsdl/SoapBasicAuth", "SoapBasicAuthPortRealm");
            String url = "http://localhost:12081/SoapBasicAuthService/SoapBasicAuthRealmPort";
            service.addPort(echoPort, null, url); 
            Dispatch<Source> dispatch = service.createDispatch(echoPort,
                                                               Source.class,
                                                               Service.Mode.PAYLOAD);

            SOAPMessage outSoapMsg = MessageFactory.newInstance().createMessage();
            
            // Build the message.
            String request = 
                 "<SoapBasicAuthOperation xmlns=\"http://j2ee.netbeans.org/wsdl/SoapBasicAuth\">" +
                 "<request-part>Testing HTTP Basic Authentication</request-part>" +
                 "</SoapBasicAuthOperation>";
                        
            final String expectedResponse = 
                 "<?xml version=\"1.0\" ?>" +
                 "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
                    "<SOAP-ENV:Body>" +
                       "<m:SoapBasicAuthOperationResponse xmlns:m=\"http://j2ee.netbeans.org/wsdl/SoapBasicAuth\">" +
                          "<response-part xmlns=\"\">Response from SoapBasicAuthRealm</response-part>" +
                       "</m:SoapBasicAuthOperationResponse>" +
                    "</SOAP-ENV:Body>" +
                 "</SOAP-ENV:Envelope>";

            
            ByteArrayInputStream bais = new ByteArrayInputStream(request.getBytes());
            Source input = new StreamSource(bais);

            Source output = dispatch.invoke(input);
            System.out.println ("After invoke");

            assert (output != null);
            
            StreamResult result = new StreamResult(new ByteArrayOutputStream());
            Transformer trans = TransformerFactory.newInstance().newTransformer();
            trans.transform(output, result);
            ByteArrayOutputStream baos = (ByteArrayOutputStream) result.getOutputStream();
            // Check the response content.
            String responseContent = new String(baos.toByteArray());
            System.out.println("Got response: \n" + responseContent + "\n");
            assert (expectedResponse.equals(responseContent));
        } catch (Throwable t) {
            fail (t.getMessage());
        }        
    }    
    
    public void testWssTokenCompareAuthentication() throws Exception {
        try {            
            Service service = Service.create(new QName("http://j2ee.netbeans.org/wsdl/SoapBasicAuth", "SoapBasicAuthService"));
            QName echoPort = new QName("http://j2ee.netbeans.org/wsdl/SoapBasicAuth", "SoapBasicAuthPortWssToken");
            String url = "http://localhost:12081/SoapBasicAuthService/SoapBasicAuthWssTokenPort";
            service.addPort(echoPort, null, url); 
            Dispatch<Source> dispatch = service.createDispatch(echoPort,
                                                               Source.class,
                                                               Service.Mode.PAYLOAD);

            SOAPMessage outSoapMsg = MessageFactory.newInstance().createMessage();
            
            // Build the message.
            String request = 
                 "<SoapBasicAuthOperation xmlns=\"http://j2ee.netbeans.org/wsdl/SoapBasicAuth\">" +
                 "<request-part>Testing HTTP Basic Authentication</request-part>" +
                 "</SoapBasicAuthOperation>";
                        
            final String expectedResponse = 
                 "<?xml version=\"1.0\" ?>" +
                 "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
                    "<SOAP-ENV:Body>" +
                       "<m:SoapBasicAuthOperationResponse xmlns:m=\"http://j2ee.netbeans.org/wsdl/SoapBasicAuth\">" +
                          "<response-part xmlns=\"\">Response from SoapBasicAuthWssTokenCompare</response-part>" +
                       "</m:SoapBasicAuthOperationResponse>" +
                    "</SOAP-ENV:Body>" +
                 "</SOAP-ENV:Envelope>";

            
            ByteArrayInputStream bais = new ByteArrayInputStream(request.getBytes());
            Source input = new StreamSource(bais);

            Source output = dispatch.invoke(input);
            System.out.println ("After invoke");

            assert (output != null);
            
            StreamResult result = new StreamResult(new ByteArrayOutputStream());
            Transformer trans = TransformerFactory.newInstance().newTransformer();
            trans.transform(output, result);
            ByteArrayOutputStream baos = (ByteArrayOutputStream) result.getOutputStream();
            // Check the response content.
            String responseContent = new String(baos.toByteArray());
            System.out.println("Got response: \n" + responseContent + "\n");
            assert (expectedResponse.equals(responseContent));
        } catch (Throwable t) {
            fail (t.getMessage());
        }        
    }        
}
