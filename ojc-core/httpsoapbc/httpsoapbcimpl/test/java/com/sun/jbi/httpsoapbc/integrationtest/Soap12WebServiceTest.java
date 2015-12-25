/**
 * 
 */
package com.sun.jbi.httpsoapbc.integrationtest;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import javax.xml.namespace.QName;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.MimeHeaders;
import javax.xml.soap.SOAPConstants;
import javax.xml.soap.SOAPFault;
import javax.xml.soap.SOAPMessage;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.ws.BindingProvider;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;
import javax.xml.ws.soap.SOAPBinding;
import javax.xml.ws.soap.SOAPFaultException;

import com.sun.jbi.component.test.framework.OpenESBIntegrationTestBase;
import com.sun.jbi.component.test.framework.container.DeploymentException;

/**
 * @author Sujit Biswas
 * 
 * this test the invoke of web-service based on soap1.2
 * 
 * consumer--------->provider------>javaee-WS
 * 
 * note the consumer and the provider represents soap1.2
 * 
 */
public class Soap12WebServiceTest extends OpenESBIntegrationTestBase {

    private static final String SA = "/openesb/open-jbi-components/ojc-core/httpsoapbc/httpsoapbcimpl/test/com/sun/jbi/httpsoapbc/integrationtest/soap12/HelloCA.zip";
    private String saname="HelloCA";

    /**
     * @throws java.lang.Exception
     */

    public void setUp() throws Exception {
	saname = getAdministrationService().deployServiceAssembly(SA);
	getAdministrationService().startServiceAssembly(saname);

    }

    /**
     * @throws java.lang.Exception
     */

    public void tearDown() throws Exception {
	undeploySA();
    }

    private void undeploySA() throws DeploymentException, Exception {
	getAdministrationService().stopServiceAssembly(saname);
	getAdministrationService().shutdownServiceAssembly(saname);
	getAdministrationService().undeployServiceAssembly(saname);
    }

    /**
     * this test assumes that the default http port is 9080, if this is not the
     * case please change all the occurrence of the port 9080 with the actual
     * port specified for a given httpbc instance
     * 
     * make sure the web application for HelloService is deployed
     * 
     * @throws Exception
     */

    public void testSoapAction() throws Exception {
	Service service = Service.create(new QName("http://sample.sun.com/", "HelloServiceProxy"));
	QName proxyPort = new QName("http://sample.sun.com/", "HelloPortProxy");
	String url = "http://localhost:9098/HelloServiceProxy/HelloPortProxy";
	service.addPort(proxyPort, SOAPBinding.SOAP12HTTP_BINDING, url);

	Dispatch<SOAPMessage> dispatch = service.createDispatch(proxyPort, SOAPMessage.class, Service.Mode.MESSAGE);

	
	// Build the message.
	String request = "<soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:sam=\"http://sample.sun.com/\">" + 
                           "<soap:Header>" +
                              "<sam:myheader>Jondoe</sam:myheader>" +
                           "</soap:Header>" +
                           "<soap:Body>" +
                              "<sam:test1>" +
                                  "<parameter>Jsmith</parameter>" +
                              "</sam:test1>" +
                           "</soap:Body>" +
                        "</soap:Envelope>";

	final String expectedResponse = "<env:Envelope xmlns:env=\"http://www.w3.org/2003/05/soap-envelope\">" +
                                           "<env:Header>" +
                                              "<myheader xmlns=\"http://sample.sun.com/\" xmlns:msgns=\"http://sample.sun.com/\" xmlns:ns2=\"http://sample.sun.com/\">got it</myheader>" +
                                           "</env:Header>" +
                                           "<env:Body>" +
                                              "<ns2:test1Response xmlns:msgns=\"http://sample.sun.com/\" xmlns:ns2=\"http://sample.sun.com/\">" +
                                                 "<return>JondoeJsmith</return>" +
                                              "</ns2:test1Response>" +
                                           "</env:Body>" +
                                        "</env:Envelope>";

	
	
	MessageFactory msgFactory = MessageFactory.newInstance(SOAPConstants.SOAP_1_2_PROTOCOL);
	MimeHeaders mimeHeader = new MimeHeaders();
	mimeHeader.addHeader("Content-Type", "application/soap+xml");
	SOAPMessage message = msgFactory
			.createMessage(mimeHeader, new ByteArrayInputStream(
				request.getBytes()));

	dispatch.getRequestContext().put(BindingProvider.SOAPACTION_USE_PROPERTY, true);
	dispatch.getRequestContext().put(BindingProvider.SOAPACTION_URI_PROPERTY, "hello/test1");

	SOAPMessage output = dispatch.invoke(message);
	System.out.println("After invoke");

	assert (output != null);

	
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
	output.writeTo(baos);

	// Check the response content.
	String responseContent = new String(baos.toByteArray());
	System.out.println("Got response: \n" + responseContent + "\n");

	assert (expectedResponse.equals(responseContent));
    }
    
    
    public void testSoapFault() throws Exception {
	Service service = Service.create(new QName("http://sample.sun.com/", "HelloServiceProxy"));
	QName proxyPort = new QName("http://sample.sun.com/", "HelloPortProxy");
	String url = "http://localhost:9093/HelloServiceProxy/HelloPortProxy";
	service.addPort(proxyPort, SOAPBinding.SOAP12HTTP_BINDING, url);

	Dispatch<SOAPMessage> dispatch = service.createDispatch(proxyPort, SOAPMessage.class, Service.Mode.MESSAGE);

	
	// Build the message.
	String request = "<soap:Envelope xmlns:soap=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:sam=\"http://sample.sun.com/\">"+
                           "<soap:Header/>"+
                           "<soap:Body>"+
                              "<sam:test2>"+
                                  "<parameter>test</parameter>"+
                              "</sam:test2>"+
                           "</soap:Body>"+
                        "</soap:Envelope>";

	String expectedResponse = "<S:Envelope xmlns:S=\"http://www.w3.org/2003/05/soap-envelope\">" +
                                   "<S:Body>"+
                                      "<S:Fault xmlns:ns4=\"http://schemas.xmlsoap.org/soap/envelope/\">"+
                                         "<S:Code>"+
                                            "<S:Value>S:Receiver</S:Value>"+
                                         "</S:Code>"+
                                         "<S:Reason>"+
                                            "<S:Text xml:lang=\"en\">hello there</S:Text>"+
                                         "</S:Reason>"+
                                         "<S:Detail>"+
                                            "<ns2:SampleException xmlns:ns2=\"http://sample.sun.com/\">"+
                                               "<message>hello there</message>"+
                                            "</ns2:SampleException>"+
                                         "</S:Detail>"+
                                      "</S:Fault>"+
                                   "</S:Body>"+
                                "</S:Envelope>";

	
	
	MessageFactory msgFactory = MessageFactory.newInstance(SOAPConstants.SOAP_1_2_PROTOCOL);
	MimeHeaders mimeHeader = new MimeHeaders();
	mimeHeader.addHeader("Content-Type", "application/soap+xml");
	SOAPMessage message = msgFactory
			.createMessage(mimeHeader, new ByteArrayInputStream(
				request.getBytes()));

	//dispatch.getRequestContext().put(BindingProvider.SOAPACTION_USE_PROPERTY, true);
	//dispatch.getRequestContext().put(BindingProvider.SOAPACTION_URI_PROPERTY, "hello/test1");
	SOAPFault f = null;
	try{
	    SOAPMessage output = dispatch.invoke(message);
	}catch (SOAPFaultException e) {
	    f =  e.getFault();
	}
	System.out.println("After invoke");

	assert (f != null);

	
	f.getFaultCode();
	f.getFaultActor();
	f.getFaultString();
	f.getFaultRole();
	
	DOMSource d = new DOMSource(f.getFirstChild().getParentNode());
	
	StreamResult result = new StreamResult(new ByteArrayOutputStream());
	Transformer trans = TransformerFactory.newInstance().newTransformer();
	trans.transform(d, result);
	ByteArrayOutputStream baos = (ByteArrayOutputStream) result.getOutputStream();

	// Check the response content.
	String responseContent = new String(baos.toByteArray());
	System.out.println("Got response: \n" + responseContent + "\n");
	
	
    }


}
