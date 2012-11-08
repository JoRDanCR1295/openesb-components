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
 * @(#)ExecDenormalizerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc;

import junit.framework.*;

import com.sun.jbi.execbc.Endpoint;
import com.sun.jbi.execbc.ExecDenormalizer;
import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.extensions.ExecOperation;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Definition;
import javax.wsdl.Binding;
import javax.wsdl.Input;
import javax.wsdl.Output;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class ExecDenormalizerTest extends org.jmock.cglib.MockObjectTestCase {
    ExecDenormalizer instance = new ExecDenormalizer();
    
    Mock normalizedMsg = mock(NormalizedMessage.class);
    Mock endpoint = mock(Endpoint.class);
    Mock definition = mock(Definition.class);
    Mock service = mock(Service.class);
    Mock port = mock(Port.class);
    Mock portType = mock(PortType.class);
    Mock binding = mock(Binding.class);
    Mock operation = mock(Operation.class);
    Mock input = mock(Input.class);
    Mock output = mock(Output.class);
    Mock message = mock(Message.class);
    Mock part = mock(Part.class);
    
    QName operationName = QName.valueOf("test");
    String partname = "";
    byte[] data = null;
    HashMap endpointOps = new HashMap();
    ArrayList operations = new ArrayList();
    ArrayList parts = new ArrayList();
    HashMap partmap = new HashMap();
    HashMap operationMeps = new HashMap();
    DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
    DocumentBuilder builder;
    Document document;
    Node node;
    DOMSource source;
    
    public ExecDenormalizerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        endpoint.expects(atLeastOnce()).method("getDefinition").will(returnValue(definition.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(QName.valueOf("service1")));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue("endpoint1"));
        endpoint.expects(atLeastOnce()).method("getExecOperations").will(returnValue(endpointOps));
        endpoint.expects(atLeastOnce()).method("getOperationMsgExchangePattern").will(returnValue(operationMeps));
        endpoint.expects(atLeastOnce()).method("getMessagePartEncoderMapping").will(returnValue(new HashMap()));
        definition.expects(atLeastOnce()).method("getService").will(returnValue((Service)service.proxy()));
        service.expects(atLeastOnce()).method("getPort").will(returnValue((Port) port.proxy()));
        port.expects(atLeastOnce()).method("getBinding").will(returnValue((Binding) binding.proxy()));
        binding.expects(atLeastOnce()).method("getPortType").will(returnValue((PortType) portType.proxy()));
        portType.expects(atLeastOnce()).method("getOperations").will(returnValue(operations));
        portType.expects(atLeastOnce()).method("getQName").will(returnValue(QName.valueOf("service1")));
        
        partmap.put("part1", (Part)part.proxy());
        parts.add((Part)part.proxy());
        builder = factory.newDocumentBuilder();
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of denormalize method, of class com.sun.jbi.execbc.ExecDenormalizer.
     */
    public void testDenormalizeCase1 () throws Exception {
        System.out.println("Testing denormalize case 1.");
        
        byte[] expResult = null;
        byte[] result = null;
        
        ExecMessage execMessage = new ExecMessage();
        
        // testing the following case
        // operation not found
        document = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();
        node = document.createTextNode("HelloWorld");
        normalizedMsg.expects(atLeastOnce()).method("getContent").will(returnValue(new DOMSource(node)));
        try {
            result = instance.denormalize((NormalizedMessage)normalizedMsg.proxy(),
                                           operationName, 
                                           (Endpoint)endpoint.proxy(), 
                                           execMessage);
            fail("Failed to test denormalize - an exception should've been raised by ExecDenormalizer due to unmatched operation name.");
        } catch (Exception e) {
           
        }
        
        // testing the following case
        // invalid mep
        endpointOps.put(QName.valueOf("test"), (Operation)operation.proxy());
        operationMeps.put(QName.valueOf("test"), "outin");
        try {
            result = instance.denormalize((NormalizedMessage)normalizedMsg.proxy(),
                                           operationName, 
                                           (Endpoint)endpoint.proxy(), 
                                           execMessage);
            fail("Failed to test denormalize - an exception should've been raised by ExecDenormalizer due to unmatched operation name.");
        } catch (Exception e) {
           
        }
        
        System.out.println("Testing denormalize case 1.");
    }
    
   /**
     * Test of denormalize method, of class com.sun.jbi.execbc.ExecDenormalizer.
     */
    public void testDenormalizeCase2 () throws Exception {
        System.out.println("Testing denormalize case 2.");     
        byte[] result = null;
        
        ExecMessage execMessage = new ExecMessage();
        
        // testing the following case
        // mep type is "inonly"
        // part element is defined
        // valid data
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
        document = builder.parse(new InputSource(new StringReader(nmsg)));
        node = document.getDocumentElement();
        source = new DOMSource(node);
        endpointOps.put(QName.valueOf("test"), new ExecOperation());
        operationMeps.put(QName.valueOf("test"), "inonly");
        operations.add((Operation)operation.proxy());
        operation.expects(atLeastOnce()).method("getName").will(returnValue("test"));
        normalizedMsg.expects(atLeastOnce()).method("getContent").will(returnValue(source));
        part.expects(atLeastOnce()).method("getName").will(returnValue("part1"));
        part.expects(atLeastOnce()).method("getElementName").will(returnValue(QName.valueOf("foo")));
        definition.expects(atLeastOnce()).method("getMessage").will(returnValue((Message)message.proxy()));
        message.expects(atLeastOnce()).method("getQName").will(returnValue(QName.valueOf("message1")));
        message.expects(atLeastOnce()).method("getOrderedParts").will(returnValue(parts));
        message.expects(atLeastOnce()).method("getParts").will(returnValue(partmap));
        operation.expects(once()).method("getInput").will(returnValue((Input)input.proxy()));
        input.expects(once()).method("getMessage").will(returnValue((Message)message.proxy()));
        try {
            result = instance.denormalize((NormalizedMessage)normalizedMsg.proxy(),
                                           operationName, 
                                           (Endpoint)endpoint.proxy(), 
                                           execMessage);
        } catch (Exception e) {
            fail("Failed to test denormalize due to: " + e.getMessage());
        }
        
        // testing the following case
        // mep type is "inout"
        // part is defined
        //execMessage.setPart("part1");
        operationMeps.put(QName.valueOf("test"), "inout");
        operation.expects(once()).method("getOutput").will(returnValue((Output)output.proxy()));
        //message.expects(once()).method("getPart").will(returnValue((Part) part.proxy()));
        output.expects(once()).method("getMessage").will(returnValue((Message)message.proxy()));
        try {
            result = instance.denormalize((NormalizedMessage)normalizedMsg.proxy(),
                                           operationName, 
                                           (Endpoint)endpoint.proxy(), 
                                           execMessage);
        } catch (Exception e) {
            fail("Failed to test denormalize due to: " + e.getMessage());
        }
        
        System.out.println("Successfully tested denormalize case 2.");
    }
    
}
