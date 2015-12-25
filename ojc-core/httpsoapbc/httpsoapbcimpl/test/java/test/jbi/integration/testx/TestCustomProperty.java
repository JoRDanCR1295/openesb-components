/**
 * 
 */
package test.jbi.integration.testx;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.impl.HTTPBCSUAssembler;
import test.jbi.integration.testse.core.Command;
import test.jbi.integration.testse.impl.JbiHelper;
import test.jbi.integration.testse.util.Helper;

import com.sun.jbi.httpsoapbc.NormalizedMessageProperties;
import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.internationalization.Messages;

/**
 * @author Sujit Biswas
 * 
 */

/**
 * before running this test make sure the driver test under
 * 
 * /open-jbi-components/driver-tests/httpsoapbc/customProperty/ are deployed ,
 * see the readme file under this folder
 */
public class TestCustomProperty extends IntegrationTestCase {

    /**
     * 
     */
    private static final long serialVersionUID = 1L;

    /**
     * service assembly name
     */
    private static final String SA_NAME = "HTTPBC-CUSTOM-PROPERTY-SA";
    /**
     * consuming end-point
     */
    private static final String CONSUMER_EP = "invokeSoapPort";
    /**
     * consuming end-point service name
     */
    private static final QName INVOKE_SOAP_SERVICE = new QName("http://j2ee.netbeans.org/wsdl/FileBpelModule/custom", "invokeSoapService");

    /**
     * provider end-point service name
     */
    private static final QName PROVIDER_SERVICE = new QName("http://j2ee.netbeans.org/wsdl/FileBpelModule/custom", "customService");

    /**
     * provider end-point
     */
    private static final String PROVIDER_EP = "customPort";

    /**
     * test custom property
     * 
     * @throws Throwable
     */
    public void testCustomProperty() throws Throwable {

	String inputDir = Configuration.getWorkingDir() + File.separator + "custom";

	String[] xmlFiles = new String[] { "wsit-client.xml", "customPort.xml" };

	// Create TestSU
	HTTPBCSUAssembler su = Helper.createTestSU("HTTP-BC-SU-CustomPropertyTest", "HTTP-BC-SU-CustomPropertyTest", "custom.wsdl", xmlFiles, inputDir, this.getClass());
	String testSAPath = Helper.createTestSA(su, SA_NAME, INVOKE_SOAP_SERVICE, CONSUMER_EP, PROVIDER_SERVICE, PROVIDER_EP, null, null);
	String saName = null;
	try {
	    // activate endpoint
	    // ActivateDeactivateEP activateDeactivateEPCmd = new
	    // ActivateDeactivateEP();
	    // activateDeactivateEPCmd.activate = true;
	    // getConnection().execute(activateDeactivateEPCmd);

	    RegisterEP regCmd = new RegisterEP();
	    regCmd.register = true;
	    getConnection().execute(regCmd);

	    // Now deploy and start SA
	    saName = getInstaller().deployServiceAssembly(testSAPath);
	    getInstaller().startServiceAssembly(saName);
	    Thread.sleep(2000); // wait some time before SU start sending

	    SendMsgCommand sendMsg = new SendMsgCommand();

	    String status = (String) getConnection().execute(sendMsg);

	    status.toString();
	    // messages

	    // Drain all the messages meant for this EP
	    // ReceiveMsgCommand receviceMsgCommand = new ReceiveMsgCommand();
	    // String str;
	    // int i = 0;
	    // while (!(str = (String)
	    // getConnection().execute(receviceMsgCommand))
	    // .equals("FAIL")) {
	    //
	    // System.out.println(str);
	    // i++;
	    //
	    // }

	} finally {
	    // Un-deploy SA
	    getInstaller().undeployServiceAssembly(SA_NAME);
	}
    }

    @Override
    protected void setUp() throws Exception {
	super.setUp();
	// Make sure that SA is undeployed
	try {
	    getInstaller().undeployServiceAssembly(SA_NAME);
	} catch (Throwable t) {
	}

    }

    @Override
    protected void tearDown() throws Exception {
	super.tearDown();
    }

    public static class ActivateDeactivateEP implements Command {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public boolean activate;

	public Serializable execute(ComponentContext context) throws Exception {
	    if (activate) {
		JbiHelper.activateEndpoint(context, PROVIDER_SERVICE, PROVIDER_EP);
	    } else {
		JbiHelper.deactivateEndpoint(context, PROVIDER_SERVICE, PROVIDER_EP);
	    }
	    return "SUCCESS";
	}
    }

    public static class RegisterEP implements Command {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	public boolean register;

	public Serializable execute(ComponentContext context) throws Exception {
	    if (register) {
		JbiHelper.registerEndPoint(INVOKE_SOAP_SERVICE, CONSUMER_EP);
	    } else {
		JbiHelper.unregisterEndPoint(INVOKE_SOAP_SERVICE, CONSUMER_EP);
	    }
	    return "SUCCESS";
	}
    }

    public static class ReceiveMsgCommand implements Command {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public Serializable execute(ComponentContext context) throws Exception {
	    InOut ex = (InOut) JbiHelper.getNextMessage(INVOKE_SOAP_SERVICE, PROVIDER_EP);
	    Object[] results = null;
	    if (ex != null && ex.getStatus() != ExchangeStatus.ERROR) {
		// ex.setStatus(ExchangeStatus.DONE);
		if (ex.getStatus() != ExchangeStatus.DONE) {
		    results = JbiHelper.unwrapFromJBISource(ex.getOutMessage().getContent());
		    ex.setStatus(ExchangeStatus.DONE);
		    context.getDeliveryChannel().send(ex);
		}
	    }
	    if (results != null && results.length > 0) {
		return ((Text) results[0]).getTextContent();
	    }
	    return "FAIL";
	}

    }

    public static class SendMsgCommand implements Command {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public Serializable execute(ComponentContext context) throws Exception {

	    sendMessage("hello world", context);

	    return "SUCCESS";
	}
    }

    private static Logger mLog = Messages.getLogger(TestCustomProperty.class);

    private static ServiceEndpoint sendMessage(final String MSG, ComponentContext context) throws MessagingException, SAXException, IOException {

	ServiceEndpoint ep = context.getEndpoint(INVOKE_SOAP_SERVICE, CONSUMER_EP);
	InOut inOut = context.getDeliveryChannel().createExchangeFactory().createInOutExchange();
	inOut.setEndpoint(ep);
	inOut.setOperation(new QName("customOperation"));
	NormalizedMessage nm = inOut.createMessage();

	// set custom property

	Map<String, String> p = new HashMap<String, String>();
	p.put("saml1", "Hello World 1");
	p.put("saml2", "Hello World 2");

	String xml = JbiHelper.wrapIntoJBIMessage(new QName("http://j2ee.netbeans.org/wsdl/FileBpelModule/custom", "customOperationRequest"), new String[] { MSG });
	nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
	inOut.setInMessage(nm);

	nm.setProperty(NormalizedMessageProperties.OUTBOUND_CUSTOM_PROPERTY, p);

	DebugLog.debugLog(mLog, Level.INFO, "input message", inOut.getInMessage().getContent());

	context.getDeliveryChannel().sendSync(inOut);

	DebugLog.debugLog(mLog, Level.INFO, "reply message", inOut.getOutMessage().getContent());

	inOut.setStatus(ExchangeStatus.DONE);

	context.getDeliveryChannel().send(inOut);

	try {
	    Thread.sleep(10000);
	} catch (InterruptedException e) {
	    // TODO Auto-generated catch block
	    e.printStackTrace();
	}

	return ep;
    }

}
