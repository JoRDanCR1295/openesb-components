/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.systemic.quality.propagation.api;

import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessageExchangeFactory;
import javax.security.auth.Subject;

import com.sun.jbi.systemic.quality.propagation.jbi.DummyMessageExchangeFactory;
import com.sun.jbi.systemic.quality.propagation.jbi.DummyNormalizedMessage;

import junit.framework.TestCase;

/**
 *
 * @author radval
 */
public class ParentChildExchangeCorrelatorFactoryTest extends TestCase {
    
    public ParentChildExchangeCorrelatorFactoryTest(String testName) {
        super(testName);
    }            

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }


    /**
     * Test of getDefault method, of class ParentChildExchangeCorrelatorFactory.
     */
    public void testGetDefault() {
        System.out.println("getDefault");
        ParentChildExchangeCorrelatorFactory result = ParentChildExchangeCorrelatorFactory.getDefault();
        assertNotNull("ParentChildExchangeCorrelatorFactory should not be null", result);
    }
    
    public void testNewParentChildExchangeCorrelator() throws Exception {
        ParentChildExchangeCorrelatorFactory result = ParentChildExchangeCorrelatorFactory.getDefault();
        ConfigManager testManager = new DummyConfigManager(ConfigManager.TRANSACTIONTYPE.JOIN_PARENT, ConfigManager.SECURITYTYPE.ALWAYS);
        
        ParentChildExchangeCorrelator correlator = result.newParentChildExchangeCorrelator(testManager);
        assertNotNull("ParentChildExchangeCorrelator should not be null", correlator);
       
    }
    
    public void testJoinParentTransactionPropagation() throws Exception {
        ParentChildExchangeCorrelatorFactory result = ParentChildExchangeCorrelatorFactory.getDefault();
        ConfigManager testManager = new DummyConfigManager(ConfigManager.TRANSACTIONTYPE.JOIN_PARENT, ConfigManager.SECURITYTYPE.NEVER);
        
        ParentChildExchangeCorrelator correlator = result.newParentChildExchangeCorrelator(testManager);
        assertNotNull("ParentChildExchangeCorrelator should not be null", correlator);
       
        DummyMessageExchangeFactory factory = new DummyMessageExchangeFactory();
        InOut parentExchange = factory.createInOutExchange();
        Object dummyParentTransaction = new Object();
        parentExchange.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, dummyParentTransaction);
        DummyNormalizedMessage parentInMessage = new DummyNormalizedMessage();
        parentExchange.setInMessage(parentInMessage);
        
        
        InOut childExchange = factory.createInOutExchange();
        DummyNormalizedMessage inMessage = new DummyNormalizedMessage();
        childExchange.setInMessage(inMessage);
        
        correlator.assignChildExchange(parentExchange, 
                                       childExchange);
        
        Object propagatedTransaction = childExchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
        
        assertNotNull("Child Exchange should have transaction", propagatedTransaction);
        
        assertEquals("child exchange transaction should be same as parent transaction", propagatedTransaction, dummyParentTransaction);
    }
    
    public void testrequiresNewTransactionPropagation() throws Exception {
        ParentChildExchangeCorrelatorFactory result = ParentChildExchangeCorrelatorFactory.getDefault();
        ConfigManager testManager = new DummyConfigManager(ConfigManager.TRANSACTIONTYPE.REQUIRES_NEW, ConfigManager.SECURITYTYPE.ALWAYS);
        
        ParentChildExchangeCorrelator correlator = result.newParentChildExchangeCorrelator(testManager);
        assertNotNull("ParentChildExchangeCorrelator should not be null", correlator);
       
        DummyMessageExchangeFactory factory = new DummyMessageExchangeFactory();
        InOut parentExchange = factory.createInOutExchange();
        Object dummyParentTransaction = new Object();
        parentExchange.setProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME, dummyParentTransaction);
        DummyNormalizedMessage parentInMessage = new DummyNormalizedMessage();
        parentExchange.setInMessage(parentInMessage);
        
        
        InOut childExchange = factory.createInOutExchange();
        DummyNormalizedMessage inMessage = new DummyNormalizedMessage();
        childExchange.setInMessage(inMessage);
        
        correlator.assignChildExchange(parentExchange, 
                                       childExchange);
        
        Object propagatedTransaction = childExchange.getProperty(MessageExchange.JTA_TRANSACTION_PROPERTY_NAME);
        
        assertNotNull("Child Exchange should have transaction", propagatedTransaction);
        
        Object newTransaction = testManager.getTransaction(childExchange);
        assertEquals("child exchange transaction should be same as new transaction", propagatedTransaction, newTransaction);
    }

    
    public void testAlwaysSecurityPropagation() throws Exception {
        ParentChildExchangeCorrelatorFactory result = ParentChildExchangeCorrelatorFactory.getDefault();
        ConfigManager testManager = new DummyConfigManager(ConfigManager.TRANSACTIONTYPE.REQUIRES_NEW, ConfigManager.SECURITYTYPE.ALWAYS);
        
        ParentChildExchangeCorrelator correlator = result.newParentChildExchangeCorrelator(testManager);
        assertNotNull("ParentChildExchangeCorrelator should not be null", correlator);
       
        DummyMessageExchangeFactory factory = new DummyMessageExchangeFactory();
        
        InOut parentExchange = factory.createInOutExchange();
        DummyNormalizedMessage inParentMessage = new DummyNormalizedMessage();
        parentExchange.setInMessage(inParentMessage);
        Subject securitySubject = new Subject();
        inParentMessage.setSecuritySubject(securitySubject);
        
        InOut childExchange = factory.createInOutExchange();
        DummyNormalizedMessage inChildMessage = new DummyNormalizedMessage();
        childExchange.setInMessage(inChildMessage);
        
        correlator.assignChildExchange(parentExchange, 
                                       childExchange);
        
        Subject childSubject = inChildMessage.getSecuritySubject();
        assertNotNull("Child Exchange Input Message should have subject", childSubject);
        
        assertEquals("Child Exchange Input Message should have same subject as parent exchange input message", childSubject, securitySubject);
        
    }
    
    
    public void testNeverSecurityPropagation() throws Exception {
        ParentChildExchangeCorrelatorFactory result = ParentChildExchangeCorrelatorFactory.getDefault();
        ConfigManager testManager = new DummyConfigManager(ConfigManager.TRANSACTIONTYPE.REQUIRES_NEW, ConfigManager.SECURITYTYPE.NEVER);
        
        ParentChildExchangeCorrelator correlator = result.newParentChildExchangeCorrelator(testManager);
        assertNotNull("ParentChildExchangeCorrelator should not be null", correlator);
       
        DummyMessageExchangeFactory factory = new DummyMessageExchangeFactory();
        
        InOut parentExchange = factory.createInOutExchange();
        DummyNormalizedMessage inParentMessage = new DummyNormalizedMessage();
        parentExchange.setInMessage(inParentMessage);
        Subject securitySubject = new Subject();
        inParentMessage.setSecuritySubject(securitySubject);
        
        InOut childExchange = factory.createInOutExchange();
        DummyNormalizedMessage inChildMessage = new DummyNormalizedMessage();
        childExchange.setInMessage(inChildMessage);
        
        correlator.assignChildExchange(parentExchange, 
                                       childExchange);
        
        Subject childSubject = inChildMessage.getSecuritySubject();
        assertNull("Child Exchange Input Message should not have subject", childSubject);
        
        
    }
    
}
