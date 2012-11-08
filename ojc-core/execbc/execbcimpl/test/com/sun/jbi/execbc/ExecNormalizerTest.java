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
 * @(#)ExecNormalizerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc;

import junit.framework.*;

import com.sun.jbi.execbc.Endpoint;
import com.sun.jbi.execbc.ExecNormalizer;
import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.extensions.ExecOperation;
import com.sun.jbi.execbc.util.WSDLUtilities;
import com.sun.jbi.execbc.util.XmlUtil;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.impl.NodeListImpl;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.HashMap;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Output;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class ExecNormalizerTest extends org.jmock.cglib.MockObjectTestCase  {
    ExecNormalizer instance = null;
    
    Mock exchange = mock(MessageExchange.class);
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
    Mock normalizedMsg = mock(NormalizedMessage.class);
    
    HashMap endpointOps = new HashMap();
    HashMap operationMeps = new HashMap();
    
    QName operationName = QName.valueOf("test");
    String partname = "";
    byte[] data = null;
    ArrayList operations = new ArrayList();
    ArrayList parts = new ArrayList();
    HashMap partmap = new HashMap();
        
    public ExecNormalizerTest(String testName) {
        super(testName);
        try {
            instance = new ExecNormalizer();
        }
        catch (Exception ex) {
            // let later test detect and fail
        }
        
    }

    protected void setUp() throws Exception {
        normalizedMsg.expects(atLeastOnce()).method("setContent").with(isA(DOMSource.class));
        endpoint.expects(atLeastOnce()).method("getDefinition").will(returnValue(definition.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(QName.valueOf("service1")));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue("endpoint1"));
        endpoint.expects(atLeastOnce()).method("getMessagePartEncoderMapping").will(returnValue(new HashMap()));
        //endpoint.expects(atLeastOnce()).method("getExecOperations").will(returnValue(endpointOps));
        endpoint.expects(atLeastOnce()).method("getOperationMsgExchangePattern").will(returnValue(operationMeps));
        definition.expects(atLeastOnce()).method("getService").will(returnValue((Service)service.proxy()));
        service.expects(atLeastOnce()).method("getPort").will(returnValue( (Port) port.proxy()));
        port.expects(atLeastOnce()).method("getBinding").will(returnValue((Binding) binding.proxy()));
        binding.expects(atLeastOnce()).method("getPortType").will(returnValue((PortType) portType.proxy()));
        portType.expects(atLeastOnce()).method("getOperations").will(returnValue(operations));
        portType.expects(atLeastOnce()).method("getQName").will(returnValue(QName.valueOf("portType1")));
        operation.expects(atLeastOnce()).method("getInput").will(returnValue((Input)input.proxy()));
        input.expects(atLeastOnce()).method("getMessage").will(returnValue((Message) message.proxy()));
        message.expects(atLeastOnce()).method("getQName").will(returnValue(QName.valueOf("message1")));
        message.expects(atLeastOnce()).method("getOrderedParts").will(returnValue(parts));
        endpointOps.put(QName.valueOf("test"), new ExecOperation());
        operationMeps.put(QName.valueOf("test"), "inonly");
        operations.add((Operation)operation.proxy());
        //operation.expects(at).method("getName").will(returnValue("test"));
        
        partmap.put("part1", (Part)part.proxy());
        parts.add((Part)part.proxy());
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of normalize method, of class com.sun.jbi.execbc.ExecNormalizer.
     */
    public void testNormalizeCase1 () throws Exception {
        System.out.println("Testing normalize case 1.");
        
        NormalizedMessage result = null;
        
        //setting up
        ExecMessage execMessage = new ExecMessage();
        exchange.expects(atLeastOnce()).method("createMessage").will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        message.expects(atLeastOnce()).method("getParts").will(returnValue(partmap));
        operations.add((Operation)operation.proxy());
        
        // testing the following case
        // 1. no matching operation
        operation.expects(atLeastOnce()).method("getName").will(returnValue("blah"));

        if ( instance == null )
            fail("Failed to test normalize - normalizer instance not available.");
        
        try {
            result = instance.normalize((MessageExchange)exchange.proxy(), 
                                          operationName, 
                                          (Endpoint)endpoint.proxy(), 
                                          execMessage, 
                                          data);
            fail("Failed to test normalize - an exception should've been raised by ExecNormalizer because no operation name match can be found.");
        } catch (Exception e) {
        }
        
        
        // testing the following case
        // 1. no part name defined
        // 2. part element is defined
        
        operation.expects(atLeastOnce()).method("getName").will(returnValue("test"));
        part.expects(atLeastOnce()).method("getName").will(returnValue("part1"));
        part.expects(atLeastOnce()).method("getElementName").will(returnValue(QName.valueOf("foo")));
        data = "<foo>helloWorld</foo>".getBytes();
        
        try {
            result = instance.normalize((MessageExchange)exchange.proxy(), 
                                          operationName, 
                                          (Endpoint)endpoint.proxy(), 
                                          execMessage, 
                                          data);
            assertTrue(result instanceof NormalizedMessage);
        } catch (Exception e) {
            fail("Failed to test normalize due to: " + e.getMessage());
        }
        
        // testing the following case
        // 1. no part name defined
        // 2. part element defined 
        // 3. malformed data
        data = "<foo>helloWorld</bar>".getBytes();
        try {
            result = instance.normalize((MessageExchange)exchange.proxy(), 
                                          operationName, 
                                          (Endpoint)endpoint.proxy(), 
                                          execMessage, 
                                          data);
            fail("Failed to test normalize - an exception should've been raised by ExecNormalizer due to malformed xml data");
        } catch (Exception e) {
        }
        
        // testing the following case
        // 1. no part name defined
        // 2. part element defined 
        // 3. invalid data
        data = "HelloWorld".getBytes();
        try {
            result = instance.normalize((MessageExchange)exchange.proxy(), 
                                          operationName, 
                                          (Endpoint)endpoint.proxy(), 
                                          execMessage, 
                                          data);
            fail("Failed to test normalize - an exception should've been raised by ExecNormalizer due to data type mismatch");
        } catch (Exception e) {
        }
           
        System.out.print("Successfully tested normalize case 1.");
    }
    
    /**
     * Test of normalize method, of class com.sun.jbi.execbc.ExecNormalizer.
     */
    public void testNormalizeCase2 () throws Exception {
        System.out.println("Testing normalize case 2.");
        
        NormalizedMessage result = null;
        
        //setting up
        ExecMessage execMessage = new ExecMessage();
        //fileMessage.setPart("part1");
        exchange.expects(atLeastOnce()).method("createMessage").will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        //message.expects(atLeastOnce()).method("getPart").with(eq("part1")).will(returnValue((Part) part.proxy()));
        part.expects(atLeastOnce()).method("getElementName").will(returnValue(null));
        operation.expects(atLeastOnce()).method("getName").will(returnValue("test"));
        operations.add((Operation)operation.proxy());
        message.expects(atLeastOnce()).method("getParts").will(returnValue(partmap));

        // testing the following case
        // 1. part name is defined
        // 2. part type is a complex type
        
        part.expects(atLeastOnce()).method("getName").will(returnValue("part1"));
        part.expects(atLeastOnce()).method("getTypeName").will(returnValue(QName.valueOf("testType")));
        data = "<foo>helloWorld</foo>".getBytes();
        
        if ( instance == null )
            fail("Failed to test normalize - normalizer instance not available.");

        try {
            result = instance.normalize((MessageExchange)exchange.proxy(), 
                                          operationName, 
                                          (Endpoint)endpoint.proxy(), 
                                          execMessage, 
                                          data);
            assertTrue(result instanceof NormalizedMessage);
        } catch (Exception e) {
            fail("Failed to test normalize due to: " + e.getMessage());
        }
        
        // testing the following case
        // 1. part name 
        // 2. malformed data
        data = "<foo>helloWorld</bar>".getBytes();
        try {
            result = instance.normalize((MessageExchange)exchange.proxy(), 
                                          operationName, 
                                          (Endpoint)endpoint.proxy(), 
                                          execMessage, 
                                          data);
            fail("Failed to test normalize - an exception should've been raised by ExecNormalizer due to malformed xml data");
        } catch (Exception e) {
        }
        
        // testing the following case
        // 1. part name defined
        // 2. simple type
        part.expects(atLeastOnce()).method("getTypeName").will(returnValue(new QName("http://www.w3.org/2001/XMLSchema", "string")));
        data = "HelloWorld".getBytes();
        try {
            result = instance.normalize((MessageExchange)exchange.proxy(), 
                                          operationName, 
                                          (Endpoint)endpoint.proxy(), 
                                          execMessage, 
                                          data);
            
        } catch (Exception e) {
            fail("Failed to test normalize due to: " + e.getMessage());
        }
           
        System.out.print("Successfully tested normalize case 2.");
    }
    
}
