package com.sun.mock.participant1;

import java.io.InputStream;
import java.net.URL;
import java.security.cert.CRL;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.ExchangeStatus;
import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.servlet.http.Cookie;
import javax.sql.DataSource;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.Element;

import com.sun.jbi.component.lifecycle.ComponentManager;
import com.sun.jbi.crl.mep.AcceptManager;
import com.sun.jbi.crl.mep.ExchangeContext;
import com.sun.jbi.crl.mep.exchange.CRLInOnly;
import com.sun.jbi.crl.mep.exchange.CRLInOut;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchange;
import com.sun.jbi.crl.mep.exchange.CRLMessageExchangeFactory;
import com.sun.jbi.crl.mep.exchange.ExchangePattern;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOnlyConsumer;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOutConsumer;
import com.sun.jbi.crl.mep.proc.impl.AbstractInOutProvider;
import com.sun.jbi.crl.messaging.api.CorrelatedMessageExchangeFactory;
import com.sun.jbi.crl.messaging.api.MessageExchangeHelperFactory;

import javax.transaction.Transaction;
import javax.transaction.TransactionManager;

public class ParticipantSE1InOutConsumer extends AbstractInOutConsumer  {

	
	private static final Logger LOGGER = Logger.getLogger(ParticipantSE1InOutConsumer.class.getName());
	private Participant1Engine mEngine;
	
	private AcceptManager mAcceptManager;
	
	private Definition mDef;
	
	private Hashtable<String, InOnlyWrapper> mRequests = new Hashtable<String, InOnlyWrapper>();
	
	private Entry mEntry;
	
	
	public ParticipantSE1InOutConsumer(Definition definition, Participant1Engine engine, AcceptManager acceptManager) {
		this.mDef = definition;
		this.mEngine = engine;
		this.mAcceptManager = acceptManager;
		this.mDef = definition;
		QName serviceName = ServiceConstants.PARTICIPANT2_IN_OUT_SERVICE_NAME;
        String endpointName = ServiceConstants.PARTICIPANT2_IN_OUT_ENDPOINT_NAME;
        PortType pt = mDef.getPortType(new QName("http://j2ee.netbeans.org/wsdl/transferAmount", "transferAmountPortType"));
        Operation op = pt.getOperation("transferAmountOperation", "input1", "output1");
        
		this.mEntry = new Entry(serviceName, endpointName, op, pt);
		
		
	}
	
	@Override
	public void processOut(CRLInOut msg, ExchangeContext ctx) throws JBIException {
		InOnlyWrapper inWrapper = mRequests.get(msg.getExchangeId());
		if(inWrapper != null) {
			if(msg.getOutMessage() != null) {
				msg.setStatus(ExchangeStatus.DONE);
                msg.send();
                
				mEngine.replyPositiveToInOutParentExchange(msg);
//				ComponentManager cm = this.mAcceptManager.getComponentManager();
//                ComponentContext cc = cm.getComponentContext();
//                try {
//					TransactionManager tm = (TransactionManager) cc.getTransactionManager();
//	            	Transaction t = (Transaction) msg.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
//	            	if(t != null) {
//	            		tm.resume(t);
//	            		tm.commit();
//	            	}
//                } catch(Exception ex) {
//                	LOGGER.log(Level.SEVERE, "error in transaction rollback", ex);
//                }
                
                
                
			} else if(msg.getFault() != null || msg.getStatus() == ExchangeStatus.ERROR) {
//			    ComponentManager cm = this.mAcceptManager.getComponentManager();
//                ComponentContext cc = cm.getComponentContext();
//                try {
//					TransactionManager tm = (TransactionManager) cc.getTransactionManager();
//	            	Transaction t = (Transaction) msg.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
//	            	if(t != null) {
//	            		tm.resume(t);
//	            		tm.rollback();
//	            	}
//                } catch(Exception ex) {
//                	LOGGER.log(Level.SEVERE, "error in transaction rollback", ex);
//                }
				
				msg.setStatus(ExchangeStatus.ERROR);
                msg.send();
                
				mEngine.replyNegativeToInOutParentExchange(msg);
				
				 
			}
		}
		
	}
	
//		public void executeInTransaction() {
//			Transaction t = doDBOperation();
//			startExchange(t);
//			
//		}
		
//		public synchronized MessageExchange  startChildExchange(Transaction t) {
//			MessageExchange childExchange = null;
//            try {
//                    ComponentManager cm = this.mAcceptManager.getComponentManager();
//                    ComponentContext cc = cm.getComponentContext();
//                    CRLMessageExchangeFactory factory = this.mAcceptManager.getComponentManager().getExchangeFactory();
//                    
//                    CRLInOut inOut =  (CRLInOut) factory.createExchange(ExchangePattern.IN_OUT);
//                    NormalizedMessage nm = inOut.createMessage();
//                    Source source = Util.createDummySource(this.mDef);
//                    
//                    nm.setContent(source);
//                    inOut.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, t);
//                    
//                    QName serviceName = this.mEntry.getService();
//                    String endpointName = this.mEntry.getEndpoint();
//                    PortType pt = this.mEntry.getPortType();
//                    Operation op = this.mEntry.getOperation();
//                    if(op != null) {
//                    		QName qName = new QName(pt.getQName().getNamespaceURI(), op.getName());
//                            inOut.setOperation(qName);
//                    }
//                    
//                    ServiceEndpoint se = cc.getEndpoint(serviceName, endpointName);
//                    if(se != null) {
//                    	inOut.setInMessage(nm);
//                    	inOut.setEndpoint(se);
//                            mRequests.put(inOut.getExchangeId(), new InOnlyWrapper(inOut, this.mEntry));
//                            inOut.send();
//                    }
//                    
//                    childExchange = inOut;
//		} catch(Exception ex) {
//			LOGGER.log(Level.SEVERE, "error sending notification on deliver channel", ex);
//		}
//		
//			return childExchange;
//        }
        
		
		public synchronized MessageExchange  startChildExchange(MessageExchange parentExchange) {
			MessageExchange childExchange = null;
            try {
                    ComponentManager cm = this.mAcceptManager.getComponentManager();
                    ComponentContext cc = cm.getComponentContext();
                    
                    CorrelatedMessageExchangeFactory correlatedExchangeFactory = this.mEngine.getCorrelatedMessageExchangeFactory();
                    InOut inOut =  correlatedExchangeFactory.createInOutExchange(parentExchange);
                    
                    NormalizedMessage nm = inOut.createMessage();
                    Source source = Util.createDummySource(this.mDef);
                    
                    nm.setContent(source);
                    
                    QName serviceName = this.mEntry.getService();
                    String endpointName = this.mEntry.getEndpoint();
                    PortType pt = this.mEntry.getPortType();
                    Operation op = this.mEntry.getOperation();
                    if(op != null) {
                    		QName qName = new QName(pt.getQName().getNamespaceURI(), op.getName());
                            inOut.setOperation(qName);
                    }
                    
                    ServiceEndpoint se = cc.getEndpoint(serviceName, endpointName);
                    if(se != null) {
                    	inOut.setInMessage(nm);
                    	inOut.setEndpoint(se);
                            mRequests.put(inOut.getExchangeId(), new InOnlyWrapper(inOut, this.mEntry));
                            correlatedExchangeFactory.send(inOut);
                    }
                    
                    childExchange = inOut;
		} catch(Exception ex) {
			LOGGER.log(Level.SEVERE, "error sending notification on deliver channel", ex);
		}
		
			return childExchange;
        }
        
		
		
		private Transaction doDBOperation() {
			
			ComponentManager cm = this.mAcceptManager.getComponentManager();
            ComponentContext cc = cm.getComponentContext();
            InitialContext initContext = cc.getNamingContext();
            
            DataSource ds = null;
            try {
                ds = (DataSource) initContext.lookup(ServiceConstants.dataSourceJNDIName);
            } catch (NamingException e) {
                LOGGER.log(Level.SEVERE, "failed to get datasource from JNDI", e);
            }
            
            //handle tx here
            try {
            	TransactionManager tm = (TransactionManager) cc.getTransactionManager();
            	//tm.setTransactionTimeout(120);
            	
            	tm.begin();
            	Connection c = ds.getConnection();
            	String sql = "UPDATE \"TXDEMO\".\"BOFA_ACCOUNT\"" +
            				 " SET \"TXDEMO\".\"BOFA_ACCOUNT\".\"amount\" = 125 where \"TXDEMO\".\"BOFA_ACCOUNT\".\"accountNumber\" = 1000";
            	
            	PreparedStatement ps = c.prepareStatement(sql);
            	int rows = ps.executeUpdate();
            	Transaction t = tm.suspend();
            	
            	return t;
            } catch (Exception e) {
                LOGGER.log(Level.SEVERE, "problem related to transaction", e);
            }
            
            return null;
		}

		
                
        private static class InOnlyWrapper {
	        private InOut mInOnly;

	        private Entry mEntry;

	        public InOnlyWrapper(InOut inOnly, Entry entry) {
	        	mInOnly = inOnly;
	            mEntry = entry;
	        }

	        public Entry getEntry() {
	            return mEntry;
	        }

	        public InOut getInOut() {
	            return mInOnly;
	        }

	    }

		@Override
		public void processFault(CRLInOut msg, ExchangeContext ctx) throws JBIException {
			
		}
		
		
}