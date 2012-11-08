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
 * @(#)OutboundMessageProcessorTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc;

import com.sun.jbi.smtpbc.extservice.EmailClientAgent;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Properties;
import javax.jbi.messaging.DeliveryChannel;
import javax.jbi.messaging.InOnly;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.servicedesc.ServiceEndpoint;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.smtpbc.extensions.MailTo;
import com.sun.jbi.smtpbc.extensions.Mailbox;
import com.sun.jbi.smtpbc.extensions.SMTPAddress;
import com.sun.jbi.smtpbc.extensions.SMTPConstants;
import com.sun.jbi.smtpbc.extensions.SMTPOperation;
import com.sun.jbi.smtpbc.extensions.SMTPOperationInput;
import java.io.StringReader;
import java.io.FileInputStream;
import java.io.File;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Service;
import javax.wsdl.Port;
import javax.wsdl.Binding;
import javax.wsdl.PortType;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import org.jmock.*;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;

/**
 *
 * @author rchen
 */
public class OutboundMessageProcessorTest extends org.jmock.cglib.MockObjectTestCase {
    
    private final String PROPERTIES_FILE = "SMTPBCTestConfigure.properties";
    
    Properties testProperties = null;
    private String smtpServerHost = null;
    private String toEmailId = null;
    private String ccEmailId = null;
    private String bccEmailId = null;
    

     // some static strings
    private static final String serviceName = "serviceName";
    private static final String endpointName = "endpointName";
    private static final String exchangeID = "exchangeID";
    
     // proxies && concrete classes
    private OutboundMessageProcessor outMsgProcessor = null;
    private DeliveryChannel deliveryChannel = null;
    private Endpoint endpoint = null;
    private EndpointStatus endpointStatus = null;
    private ServiceEndpoint serviceEndpoint = null;
    private Definition definition = null;
    private Service service = null;
    private Port port = null;
    private Binding binding = null;
    private PortType portType = null;
    
     // mocks
    private Mock deliveryChannelMock = mock(DeliveryChannel.class);
    private Mock endpointMock = mock(Endpoint.class);
    private Mock endpointStatusMock = mock(EndpointStatus.class);
    private Mock serviceEndpointMock = mock(ServiceEndpoint.class );    
    private Mock serviceUnitMock = mock(ServiceUnit.class);
    private Mock definitionMock = mock(Definition.class);
    private Mock serviceMock = mock(Service.class);
    private Mock portMock = mock(Port.class);
    private Mock bindingMock = mock(Binding.class);
    private Mock portTypeMock = mock(PortType.class);
    private Mock operationMock = mock(Operation.class);
    private Mock inputMock = mock(Input.class);
    private Mock messageMock = mock(Message.class);
    private Mock partMock = mock(Part.class);
    
    
    
    
    private MessageStore messageStore = null;
    
    private Document document;
    private Node node;
    
    
    public OutboundMessageProcessorTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
 
        testProperties = new Properties();
        testProperties.load(this.getClass().getClassLoader().getResourceAsStream(PROPERTIES_FILE));
        smtpServerHost = testProperties.getProperty("emailServerHost");
        toEmailId = testProperties.getProperty("toEmailId"); 
        ccEmailId = testProperties.getProperty("ccEmailId");
        bccEmailId = testProperties.getProperty("bccEmailId");

        
        
        List endpointsList = Collections.synchronizedList(new ArrayList());
        endpointsList.add(endpointMock.proxy());
        
        // ServiceUnit and ServiceUnit List
        serviceUnitMock.stubs().method("getEndpoints").will(returnValue(endpointsList));
        
        HashMap serviceUnitsMap = new HashMap();
        serviceUnitsMap.put("suID123456789", (ServiceUnit)serviceUnitMock.proxy());
        Collection serviceUnitsCollection = serviceUnitsMap.values();
        
         messageStore = new MessageStore();
         
          // Note: Use the OutboundMessageProcessor constructor "designed" for JUnit test
        outMsgProcessor = new OutboundMessageProcessor ((DeliveryChannel)deliveryChannelMock.proxy(),
                                                        serviceUnitsCollection,
                                                         messageStore);

         
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of run method, of class com.sun.jbi.smtpbc.OutboundMessageProcessor.
     */
   
    
     public void testProcessInOnly() throws Exception{
        System.out.println("Testing processInOnly");
        
        String nmsg = "<jbi:message xmlns:msgns=\"urn:FooBar3\"" +
              "             xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"" +
              "             type=\"msgns:message1\"" +
              "             version=\"1.0\">" +
              "     <jbi:part>" + 
              "       <foo xmlns=\"urn:FooBar3\">" +
              "           <string>HelloWorld</string>" +
              "       </foo>" +
              "     </jbi:part>" +
              "</jbi:message>";
        
        document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(nmsg)));
        node = document.getDocumentElement();
        
        // NormalizedMessage mock (in message)
        Mock normalizedInMessageMock = mock(NormalizedMessage.class);
        NormalizedMessage normalizedInMessage = (NormalizedMessage)normalizedInMessageMock.proxy();
        normalizedInMessageMock.stubs().method("getContent").will(returnValue(new DOMSource(node))); 
        
        // Endpoint Mock
        endpointMock.expects(atLeastOnce()).method("getDefinition").will(returnValue((Definition)definitionMock.proxy()));
        endpointMock.expects(atLeastOnce()).method("getServiceName").will(returnValue(new QName(serviceName)));
        endpointMock.expects(atLeastOnce()).method("getEndpointName").will(returnValue(endpointName));
        
        //endpointMock.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus));
        
        SMTPAddress smtpAddress = new SMTPAddress();
        Mailbox toMailBox = new Mailbox();
        toMailBox.unmarshal(toEmailId);
        MailTo mailTo = new MailTo();
        mailTo.addMailbox(toMailBox);
        smtpAddress.setLocation(mailTo);
        smtpAddress.setSMTPServer(smtpServerHost);
        
        endpointMock.expects(atLeastOnce()).method("getSMTPAddress").will(returnValue(smtpAddress));
        

        //Definition Mock
        definitionMock.expects(once()).method("getService").will(returnValue((Service)serviceMock.proxy()));
        serviceMock.expects(once()).method("getPort").will(returnValue((Port)portMock.proxy()));     
        portMock.expects(once()).method("getBinding").will(returnValue((Binding)bindingMock.proxy()));    
        bindingMock.expects(once()).method("getPortType").will(returnValue((PortType)portTypeMock.proxy())) ;       
        messageMock.expects(atLeastOnce()).method("getQName").will(returnValue(new QName("message1")));
        inputMock.expects(once()).method("getMessage").will(returnValue((Message)messageMock.proxy()));
        operationMock.expects(atLeastOnce()).method("getName").will(returnValue("SMTPInOnlyOperation"));
        operationMock.expects(atLeastOnce()).method("getInput").will(returnValue((Input)inputMock.proxy()));
        
        
        List operations = new ArrayList();        
        operations.add((Operation)operationMock.proxy());
        
        
        //PortType Mock
        portTypeMock.expects(once()).method("getOperations").will(returnValue(operations));      
        
        
         // in-only operation name
        String operation = "SMTPInOnlyOperation";
        QName operationQName = new QName(operation);
        
        Mock inonlyMock = mock(InOnly.class);
        inonlyMock.expects(once()).method("getInMessage").will(returnValue(normalizedInMessage));
        inonlyMock.expects(once()).method("getOperation").will(returnValue(operationQName));
       
        SMTPOperation smtpOperation = new SMTPOperation();
        smtpOperation.setElementType(new QName("SMTPInOnlyOperation"));
        
        SMTPOperationInput smtpInput = new SMTPOperationInput();
        smtpInput.setMessage("part1");
        smtpInput.setSmtpUseType(SMTPConstants.SMTP_USE_TYPE_LITERAL);

        
        HashMap map = new HashMap();
        map.put(operationQName,smtpOperation);
        
        endpointMock.expects(atLeastOnce()).method("getSMTPOperations").will(returnValue(map));
        endpointMock.expects(atLeastOnce()).method("getSMTPOperationInput").will(returnValue(smtpInput));       
        
        ArrayList parts = new ArrayList();
        parts.add((Part)partMock.proxy());
        partMock.expects(atLeastOnce()).method("getName").will(returnValue("part1"));
        
        messageMock.expects(atLeastOnce()).method("getOrderedParts").will(returnValue(parts));
        definitionMock.expects(atLeastOnce()).method("getMessage").will(returnValue((Message)messageMock.proxy()));
        endpointMock.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue((EndpointStatus)endpointStatusMock.proxy()));
        endpointStatusMock.expects(atLeastOnce()).method("incrementReceivedRequests").withNoArguments();
        
        inonlyMock.expects(once()).method("setStatus");
        deliveryChannelMock.expects(once()).method("send");
        endpointStatusMock.expects(once()).method("incrementSentDones");
        
        outMsgProcessor.processInOnly((InOnly)inonlyMock.proxy(), (Endpoint)endpointMock.proxy());    
       
       
    }

    

    /**
     * Test of stopReceiving method, of class com.sun.jbi.smtpbc.OutboundMessageProcessor.
     */
    public void testStopReceiving() {
        System.out.println("stopReceiving");
        
  
        outMsgProcessor.stopReceiving();
        
       
    }
    
}
