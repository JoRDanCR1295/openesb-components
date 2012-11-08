package test.jbi.jmsbc.integration.testcases.syncreadandjmsreplyto;

import java.io.IOException;

import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.transaction.Status;
import javax.transaction.Transaction;
import javax.xml.namespace.QName;

import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import test.jbi.integration.test.framework.Configuration;
import test.jbi.integration.test.framework.IntegrationTestCase;
import test.jbi.integration.test.framework.SAAssembler;
import test.jbi.integration.test.framework.impl.JMSBCSUAssembler;
import test.jbi.integration.testbc.impl.JbiHelper;
import test.jbi.jmsbc.integration.testcases.nmproperties.DrainDestination;

public class TestSynchronousRead extends IntegrationTestCase {

	private static final int WAIT_TIME = 10000;
	private static final String JMS_TNS = "http://j2ee.netbeans.org/wsdl/JMS";
	private static final String JMS_BC_SU = "JMS-BC-SU-TestSynchronousRead";
	private static final String SA_NAME = "JMS-BC-SA-TestSynchronousRead";
	private static final QName JMS_SERVICE = new QName(JMS_TNS, "JMSService");
	private static final String PROVIDER_EP = "JMSPort";

	// set operation name
	private static final QName OPERATOR_SEND = new QName("JMSOperation");
	private static final QName OPERATOR_READ = new QName("JMSRead");
	private static final QName MESSAGE = new QName(JMS_TNS,
			"JMSOperationRequest");

	public void test_IntegrationTestBasicSyncRead() throws Throwable {
		final String MSG = "ReadSyncMessage";
		ComponentContext context = JbiHelper.getComponentContext();
		// send a message
		NormalizedMessage nm;
		ServiceEndpoint ep = sendMessage(MSG, context);

		boolean pass = false;
		// Now do synchronous read
		Thread.sleep(WAIT_TIME);
		InOut inOut = context.getDeliveryChannel().createExchangeFactory()
				.createInOutExchange();
		inOut.setEndpoint(ep);
		inOut.setOperation(OPERATOR_READ);
		nm = inOut.createMessage();
		String xml = JbiHelper.wrapIntoJBIMessage(MESSAGE, new String[] { MSG });
		nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
		inOut.setInMessage(nm);
		context.getDeliveryChannel().sendSync(inOut);
		if (inOut.getStatus() == ExchangeStatus.ERROR) {
			pass = false;
		} else {
			Object[] results = JbiHelper.unwrapFromJBISource(inOut
					.getOutMessage().getContent());
			if (results != null) {
				assertEquals(MSG, ((Text) results[0]).getTextContent());
				pass = true;
			}
			inOut.setStatus(ExchangeStatus.DONE);
			context.getDeliveryChannel().send(inOut);
		}
		assertTrue(pass);
	}

	public void test_IntegrationTestBasicSyncReadWithTransaction() throws Throwable {
		final String MSG = "ReadSyncMessageTransacted";
		ComponentContext context = JbiHelper.getComponentContext();
		// send a message
		NormalizedMessage nm;
		ServiceEndpoint ep = sendMessage(MSG, context);

		boolean pass = false;
		Transaction tx = null;
		try{
			Thread.sleep(WAIT_TIME);
			InOut inOut = context.getDeliveryChannel().createExchangeFactory()
					.createInOutExchange();
			//Start Transaction
			tx = JbiHelper.startTransaction(context);
			assertTrue(readMsg(MSG, context, ep, inOut, tx));
			//Now rollback the transaction and the same message should be re delivered
			tx.rollback();
			tx = null;
			
			Thread.sleep(WAIT_TIME);
			//Start a new Transaction
			inOut = context.getDeliveryChannel().createExchangeFactory().createInOutExchange();
			tx = JbiHelper.startTransaction(context);
			assertTrue(readMsg(MSG, context, ep, inOut, tx));
			tx.commit();
			tx = null;
			
			Thread.sleep(WAIT_TIME);
			//Now try to read after commit. This should fail
			inOut = context.getDeliveryChannel().createExchangeFactory().createInOutExchange();
			assertFalse(readMsg(MSG, context, ep, inOut, tx));
			
		}catch(Throwable t){
			if(tx != null){
				JbiHelper.resumeTransaction(context, tx);
				tx.setRollbackOnly();
			}
			throw t;
		}finally{
			if(tx != null){
				JbiHelper.resumeTransaction(context, tx);
                if (tx.getStatus() == Status.STATUS_MARKED_ROLLBACK) {
                    tx.rollback();                        
                } else {
    				tx.commit();
                }
			}
		}
	}

	public void test_IntegrationTestBasicSyncReadWithTransaction2() throws Throwable {
		final String MSG = "test_IntegrationTestBasicSyncReadWithTransaction2";
		ComponentContext context = JbiHelper.getComponentContext();
		// send a message
		NormalizedMessage nm;
		ServiceEndpoint ep = sendMessage(MSG, context);

		boolean pass = true;
		Transaction tx = null;
		try{
			Thread.sleep(WAIT_TIME);
			InOut inOut = context.getDeliveryChannel().createExchangeFactory()
					.createInOutExchange();
			//Start Transaction
			tx = JbiHelper.startTransaction(context);
			inOut.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
			inOut.setEndpoint(ep);
			inOut.setOperation(OPERATOR_READ);
			nm = inOut.createMessage();
			String xml = JbiHelper.wrapIntoJBIMessage(MESSAGE, new String[] { MSG });
			nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
			inOut.setInMessage(nm);
			
			JbiHelper.suspendThreadTx(context);
			context.getDeliveryChannel().sendSync(inOut);
			JbiHelper.resumeTransaction(context, tx);
			if (inOut.getStatus() != ExchangeStatus.ERROR) {
				Object[] results = JbiHelper.unwrapFromJBISource(inOut
						.getOutMessage().getContent());
				if (results != null) {
					pass = MSG.equals(((Text)results[0]).getTextContent());
				}
				//Send error this should rollback the transaction.
				inOut.setStatus(ExchangeStatus.ERROR);
				tx.setRollbackOnly();
				context.getDeliveryChannel().send(inOut);
				tx.rollback();
			}else{
				pass = false;
			}
			tx = null;
			assertTrue(pass);
			
			Thread.sleep(WAIT_TIME);
			//Start a new Transaction
			inOut = context.getDeliveryChannel().createExchangeFactory().createInOutExchange();
			tx = JbiHelper.startTransaction(context);
			assertTrue(readMsg(MSG, context, ep, inOut, tx));
			tx.commit();
			tx = null;
			
			Thread.sleep(WAIT_TIME);
			//Now try to read after commit. This should fail
			inOut = context.getDeliveryChannel().createExchangeFactory().createInOutExchange();
			assertFalse(readMsg(MSG, context, ep, inOut, tx));
			
		}catch(Throwable t){
			if(tx != null){
				JbiHelper.resumeTransaction(context, tx);
				tx.setRollbackOnly();
			}
			throw t;
		}finally{
			if(tx != null){
				JbiHelper.resumeTransaction(context, tx);
                if (tx.getStatus() == Status.STATUS_MARKED_ROLLBACK) {
                    tx.rollback();                        
                } else {
    				tx.commit();
                }
			}
		}
	}

	private boolean readMsg(final String MSG, ComponentContext context,
			ServiceEndpoint ep, InOut inOut, Transaction tx) throws Exception,
			MessagingException, SAXException, IOException {
		boolean pass = false;
		NormalizedMessage nm;
		if(tx!=null) inOut.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, tx);
		
		inOut.setEndpoint(ep);
		inOut.setOperation(OPERATOR_READ);
		nm = inOut.createMessage();
		String xml = JbiHelper.wrapIntoJBIMessage(MESSAGE, new String[] { MSG });
		nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
		inOut.setInMessage(nm);
		
		if(tx!=null) JbiHelper.suspendThreadTx(context);
		context.getDeliveryChannel().sendSync(inOut);
		if(tx!=null) JbiHelper.resumeTransaction(context, tx);
		if (inOut.getStatus() != ExchangeStatus.ERROR) {
			Object[] results = JbiHelper.unwrapFromJBISource(inOut
					.getOutMessage().getContent());
			if (results != null && results.length > 0) {
				pass = MSG.equals(((Text)results[0]).getTextContent());
			}
			inOut.setStatus(ExchangeStatus.DONE);
			context.getDeliveryChannel().send(inOut);
		}
		return pass;
	}

	private ServiceEndpoint sendMessage(final String MSG,
			ComponentContext context) throws Exception,
			IOException {
		drainMessages(context);
		boolean pass = true;
		ServiceEndpoint ep = context.getEndpoint(JMS_SERVICE, PROVIDER_EP);
		InOnly inOnly = context.getDeliveryChannel().createExchangeFactory()
				.createInOnlyExchange();
		inOnly.setEndpoint(ep);
		inOnly.setOperation(OPERATOR_SEND);
		NormalizedMessage nm = inOnly.createMessage();
		String xml = JbiHelper
				.wrapIntoJBIMessage(MESSAGE, new String[] { MSG });
		nm.setContent(JbiHelper.fromXMLToDOMSource(xml));
		inOnly.setInMessage(nm);
		
		context.getDeliveryChannel().sendSync(inOnly);
		if (inOnly.getStatus() != ExchangeStatus.DONE) {
			pass = false;
		}
		assertTrue(pass);
		return ep;
	}
	
	private void drainMessages(ComponentContext context) throws Exception{
		DrainDestination drain = new DrainDestination(getInstaller(), "SyncReadQueue", "Queue", context);
		drain.drainDestiantion();
	}
	
	private String createTestSU() throws IOException {
		JMSBCSUAssembler su = new JMSBCSUAssembler(JMS_BC_SU, JMS_BC_SU);
		su.addWsdl(Configuration.getPath(getClass(), "JMS.wsdl"));
		su.setJbiFile(Configuration.getPath(getClass(), "jbi.xml"));

		SAAssembler sa = new SAAssembler(SA_NAME, SA_NAME);
		sa.addSUAssembler(su);
		return sa.assemble(Configuration.getWorkingDir());
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		// Make sure that SA is undeployed
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}
		String zipTestSU = createTestSU();
		getInstaller().deployServiceAssembly(zipTestSU);
		getInstaller().startServiceAssembly(SA_NAME);
	}

	@Override
	protected void tearDown() throws Exception {
		// Make sure that SA is undeployed
		try {
			getInstaller().undeployServiceAssembly(SA_NAME);
		} catch (Throwable t) {
		}
		// TODO Auto-generated method stub
		super.tearDown();
	}

}
