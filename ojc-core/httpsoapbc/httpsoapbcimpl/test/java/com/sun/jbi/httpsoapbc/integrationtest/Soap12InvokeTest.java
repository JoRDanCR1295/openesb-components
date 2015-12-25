/**
 * 
 */
package com.sun.jbi.httpsoapbc.integrationtest;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import javax.xml.namespace.QName;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPMessage;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.ws.BindingProvider;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;

import com.sun.jbi.component.test.framework.OpenESBIntegrationTestBase;

/**
 * @author Sujit Biswas
 * 
 * this test the invoke of web-service based on soap1.2
 * 
 * consumer--------->provider------>javaee-WS
 * 
 * note the consumer represents the soap1.1 and the provider represents soap1.2
 * 
 */
public class Soap12InvokeTest extends OpenESBIntegrationTestBase {

    private static final String SA = "/Users/david/java-ext/openesb/bitbucket/openesb-components/ojc-core/httpsoapbc/httpsoapbcimpl/test/com/sun/jbi/httpsoapbc/integrationtest/soap12/SoapInvoke.zip";
    private String saname;

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
        /*
	getAdministrationService().stopServiceAssembly(saname);
	getAdministrationService().shutdownServiceAssembly(saname);
	getAdministrationService().undeployServiceAssembly(saname);
        */ 
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

    public void testInvoke() throws Exception {
	Service service = Service.create(new QName("http://sample.sun.com/", "HelloServiceProxy"));
	QName proxyPort = new QName("http://sample.sun.com/", "HelloPortProxy");
	String url = "http://localhost:9080/HelloServiceProxy/HelloPortProxy";
	service.addPort(proxyPort, null, url);

	Dispatch<Source> dispatch = service.createDispatch(proxyPort, Source.class, Service.Mode.MESSAGE);

	SOAPMessage outSoapMsg = MessageFactory.newInstance().createMessage();

	// Build the message.
	String request = "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:sam=\"http://sample.sun.com/\">" + "<soapenv:Header>"
		+ "<sam:myheader>Jondoe</sam:myheader>" + "</soapenv:Header>" + "<soapenv:Body>" + "<sam:test1>" + "<parameter>Jsmith</parameter>" + "</sam:test1>"
		+ "</soapenv:Body>" + "</soapenv:Envelope>";

	final String expectedResponse = "<?xml version=\"1.0\" ?>" + "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" + "<SOAP-ENV:Header>"
		+ "<myheader xmlns=\"http://sample.sun.com/\" xmlns:msgns=\"http://sample.sun.com/\" xmlns:ns2=\"http://sample.sun.com/\">got it</myheader>" + "</SOAP-ENV:Header>"
		+ "<SOAP-ENV:Body><ns2:test1Response xmlns:msgns=\"http://sample.sun.com/\" xmlns:ns2=\"http://sample.sun.com/\">"
		+ "<return xmlns=\"\">JondoeJsmith</return></ns2:test1Response>" + "</SOAP-ENV:Body>" + "</SOAP-ENV:Envelope>";

	ByteArrayInputStream bais = new ByteArrayInputStream(request.getBytes());
	Source input = new StreamSource(bais);

	dispatch.getRequestContext().put(BindingProvider.SOAPACTION_USE_PROPERTY, true);
	dispatch.getRequestContext().put(BindingProvider.SOAPACTION_URI_PROPERTY, "hello/test1");

	Source output = dispatch.invoke(input);
	System.out.println("After invoke");

	assert (output != null);

	StreamResult result = new StreamResult(new ByteArrayOutputStream());
	Transformer trans = TransformerFactory.newInstance().newTransformer();
	trans.transform(output, result);
	ByteArrayOutputStream baos = (ByteArrayOutputStream) result.getOutputStream();

	// Check the response content.
	String responseContent = new String(baos.toByteArray());
	System.out.println("Got response: \n" + responseContent + "\n");

	assert (expectedResponse.equals(responseContent));
    }

}
