/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.systemic.quality.propagation.api.impl;

import com.sun.jbi.systemic.quality.propagation.api.ConfigManager;
import com.sun.jbi.systemic.quality.propagation.api.DummyConfigManager;
import com.sun.jbi.systemic.quality.propagation.jbi.DummyMessageExchangeFactory;
import com.sun.jbi.systemic.quality.propagation.jbi.DummyNormalizedMessage;

import javax.jbi.messaging.InOut;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;

import junit.framework.TestCase;

/**
 *
 * @author radval
 */
public class ParentChildExchangeCorrelatorImplTest extends TestCase {
    
    public ParentChildExchangeCorrelatorImplTest(String testName) {
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
     * Test of assignChildExchange method, of class ParentChildExchangeCorrelatorImpl.
     */
    public void testAssignChildExchange() throws Exception {
        System.out.println("assignChildExchange");
        
        DummyConfigManager manager = new DummyConfigManager(ConfigManager.TRANSACTIONTYPE.JOIN_PARENT, ConfigManager.SECURITYTYPE.ALWAYS);
        DummyMessageExchangeFactory factory = new DummyMessageExchangeFactory();
        
        InOut parentExchange = factory.createInOutExchange();
        DummyNormalizedMessage inParentMessage = new DummyNormalizedMessage();
        parentExchange.setInMessage(inParentMessage);
        
        InOut childExchange = factory.createInOutExchange();
        DummyNormalizedMessage inMessage = new DummyNormalizedMessage();
        childExchange.setInMessage(inMessage);
        
        ParentChildExchangeCorrelatorImpl instance = new ParentChildExchangeCorrelatorImpl(manager);
        instance.assignChildExchange(parentExchange, childExchange);
        
    }
    
    public void testdoSecurityPropogationToChildExchange1() throws Exception {
    	System.out.println("testdoSecurityPropogationToChildExchange");
        
        DummyConfigManager manager = new DummyConfigManager(ConfigManager.TRANSACTIONTYPE.JOIN_PARENT, ConfigManager.SECURITYTYPE.ALWAYS);
        DummyMessageExchangeFactory factory = new DummyMessageExchangeFactory();
        
        MessageExchange parentExchange = factory.createInOutExchange();
        MessageExchange childExchange = factory.createInOutExchange();
        ParentChildExchangeCorrelatorImpl instance = new ParentChildExchangeCorrelatorImpl(manager);
        Exception expectedException = null;
        
        try {
        	instance.doSecurityPropogationToChildExchange(parentExchange, childExchange);
        } catch(Exception ex) {
        	expectedException = ex;
        }
        
        assertNotNull("Expecting Exception", expectedException);
        assertEquals(ParentChildExchangeCorrelatorImpl.EXCEPTION_PARENT_EXCHANGE_SHOULD_HAVE_INPUT_MESSAGE, expectedException.getMessage());
    }
    
    public void testdoSecurityPropogationToChildExchange2() throws Exception {
    	System.out.println("testdoSecurityPropogationToChildExchange");
        
        DummyConfigManager manager = new DummyConfigManager(ConfigManager.TRANSACTIONTYPE.JOIN_PARENT, ConfigManager.SECURITYTYPE.ALWAYS);
        DummyMessageExchangeFactory factory = new DummyMessageExchangeFactory();
        
        InOut parentExchange = factory.createInOutExchange();
        DummyNormalizedMessage inParentMessage = new DummyNormalizedMessage();
        parentExchange.setInMessage(inParentMessage);
        
        
        MessageExchange childExchange = factory.createInOutExchange();
        ParentChildExchangeCorrelatorImpl instance = new ParentChildExchangeCorrelatorImpl(manager);
        Exception expectedException = null;
        
        try {
        	instance.doSecurityPropogationToChildExchange(parentExchange, childExchange);
        } catch(Exception ex) {
        	expectedException = ex;
        }
        
        assertNotNull("Expecting Exception", expectedException);
        assertEquals(ParentChildExchangeCorrelatorImpl.EXCEPTION_CHILD_EXCHANGE_SHOULD_HAVE_INPUT_MESSAGE, expectedException.getMessage());
    }

}
