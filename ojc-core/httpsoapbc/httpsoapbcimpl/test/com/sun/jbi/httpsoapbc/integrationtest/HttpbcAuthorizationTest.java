/**
 * 
 */
package com.sun.jbi.httpsoapbc.integrationtest;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.net.PasswordAuthentication;

import javax.xml.namespace.QName;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPMessage;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;

import com.sun.jbi.component.test.framework.OpenESBIntegrationTestBase;

/**
 * @author Sujit Biswas
 * 
 */
public class HttpbcAuthorizationTest extends OpenESBIntegrationTestBase {

	private static final String username = "jondoe";
	private static final String password = "jondoe";
	private static final String SA = "/openesb/open-jbi-components/ojc-core/httpsoapbc/httpsoapbcimpl/test/com/sun/jbi/httpsoapbc/integrationtest/caps/AuthorizationCA.zip";
	private String saname;

	private class TestAuthenticator extends java.net.Authenticator {
		public TestAuthenticator() {
			setDefault(this);
		}

		protected PasswordAuthentication getPasswordAuthentication() {
			return new PasswordAuthentication(username, password.toCharArray());
		}
	}

	/**
	 * @throws java.lang.Exception
	 */
	
	public void setUp() throws Exception {
		new TestAuthenticator();
		saname = getAdministrationService().deployServiceAssembly(SA);
		getAdministrationService().startServiceAssembly(saname);

	}

	/**
	 * @throws java.lang.Exception
	 */
	
	public void tearDown() throws Exception {
		getAdministrationService().stopServiceAssembly(saname);
		getAdministrationService().shutdownServiceAssembly(saname);
		getAdministrationService().undeployServiceAssembly(saname);
	}

	/**
	 * this test assumes that the default http port is 9080, if this is not the
	 * case please change all the occurrence of the port 9080 with the actual
	 * port specified for a given httpbc instance
	 * 
	 * @throws Exception
	 */

	public void testAccessManagerAuthorization() throws Exception {
		Service service = Service.create(new QName("http://j2ee.netbeans.org/wsdl/AuthAM", "AuthAMService"));
		QName echoPort = new QName("http://j2ee.netbeans.org/wsdl/AuthAM", "AuthAMPort");
		String url = "http://localhost:9080/AuthAMService/AuthAMPort";
		service.addPort(echoPort, null, url);

		Dispatch<Source> dispatch = service.createDispatch(echoPort, Source.class, Service.Mode.PAYLOAD);

		SOAPMessage outSoapMsg = MessageFactory.newInstance().createMessage();

		// Build the message.
		String request = "<AuthAMOperation xmlns=\"http://j2ee.netbeans.org/wsdl/AuthAM\">" + "<part1>hello</part1>"
				+ "</AuthAMOperation>";

		final String expectedResponse = "<?xml version=\"1.0\" ?>"
				+ "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">"
				+ "<SOAP-ENV:Body>"
				+ "<m:AuthAMOperationResponse xmlns:m=\"http://j2ee.netbeans.org/wsdl/AuthAM\">"
				+ "<part1 xmlns:aut=\"http://j2ee.netbeans.org/wsdl/AuthAM\" xmlns:msgns=\"http://j2ee.netbeans.org/wsdl/AuthAM\" xmlns=\"\">hello</part1>"
				+ "</m:AuthAMOperationResponse>" + "</SOAP-ENV:Body>" + "</SOAP-ENV:Envelope>";

		ByteArrayInputStream bais = new ByteArrayInputStream(request.getBytes());
		Source input = new StreamSource(bais);

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
