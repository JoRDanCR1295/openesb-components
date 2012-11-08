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
 * @(#)InboundReceiverTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc;

import junit.framework.*;
import java.util.HashMap;
import java.util.Map;
import javax.jbi.component.ComponentContext;
import javax.jbi.messaging.DeliveryChannel;
import javax.xml.namespace.QName;
import com.sun.jbi.smtpbc.extensions.MailTo;
import com.sun.jbi.smtpbc.extensions.SMTPAddress;
import org.jmock.*;

/**
 *
 * @author rchen
 */
public class InboundReceiverTest extends org.jmock.cglib.MockObjectTestCase {
    
    InboundReceiver instance = null;
    MessageStore messageStore = null;
    Mock deliveryChannel = null;
    Mock componentContext = null;
    Map operations = new HashMap();
    Endpoint endpoint = null;
    
    public InboundReceiverTest(final String testName) {
        super(testName);
    }
    
    @Override
	protected void setUp() throws Exception {
        messageStore = new MessageStore();
        componentContext = mock(ComponentContext.class);
        deliveryChannel = mock(DeliveryChannel.class);
        endpoint =  new EndpointImpl();
        endpoint.setEndpointType(Endpoint.EndpointType.INBOUND);
        endpoint.setEndpointName("some-endpointname");
        endpoint.setServiceName(new QName("somenamespace", "someOp1"));
        final MailTo mailTo = new MailTo("mailto:afung@seebeyond.com");
        final SMTPAddress smtpAddress = new SMTPAddress();
        smtpAddress.setLocation(mailTo);
        smtpAddress.setSMTPServer("localhost");
        smtpAddress.setSMTPPort(9876);
        endpoint.setSMTPAddress(smtpAddress);
        instance = new InboundReceiver((ComponentContext) componentContext.proxy(),
                (DeliveryChannel) deliveryChannel.proxy(),
                messageStore);
        
    }
    
    @Override
	protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        final TestSuite suite = new TestSuite(InboundReceiverTest.class);
        return suite;
    }
    
    /**
     * Test of stopReceiving method, of class com.sun.jbi.smtpbc.InboundReceiver.
     */
    public void testStopReceiving() throws Exception {
        System.out.println("stopReceiving");
        
        
        instance.stopReceiving();
        
        
    }
    
    /**
     * Test of endpointInitialized method, of class com.sun.jbi.smtpbc.InboundReceiver.
     */
    public void testEndpointInitialized() {
        System.out.println("endpointInitialized");
        
        instance.endpointInitialized(endpoint);
        
        
    }
    
    /**
     * Test of endpointActivated method, of class com.sun.jbi.smtpbc.InboundReceiver.
     */
    public void testEndpointActivated() throws Exception {
        System.out.println("endpointActivated");
        try{
        instance.endpointActivated(endpoint);
        instance.endpointDeactivated(endpoint);
        } catch (final EndpointChangeException ex)
        {
        	fail(ex.getMessage());
        }
                
        
    }
    
    /**
     * Test of endpointDeactivated method, of class com.sun.jbi.smtpbc.InboundReceiver.
     */
    public void testEndpointDeactivated() {
        

        try {
            instance.endpointActivated(endpoint);
			instance.endpointDeactivated(endpoint);
		} catch (EndpointChangeException e) {
			fail(e.getMessage());

		}
        
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    
    /**
     * Test of endpointShutdown method, of class com.sun.jbi.smtpbc.InboundReceiver.
     */
    public void testEndpointShutdown() {
        System.out.println("endpointShutdown");
        
        instance.endpointShutdown(endpoint);
        
    }
    
}
