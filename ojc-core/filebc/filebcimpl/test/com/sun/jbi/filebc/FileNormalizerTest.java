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
 * @(#)FileNormalizerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc;

import com.sun.jbi.filebc.extensions.FileMessage;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.util.FileStreamHandler;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.namespace.QName;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.transform.dom.DOMSource;
import org.jmock.*;

/**
 *
 * @author sweng
 */
public class FileNormalizerTest extends org.jmock.cglib.MockObjectTestCase {

    FileNormalizer instance = null;
    Mock exchange = mock(MessageExchange.class);
    Mock endpoint = mock(Endpoint.class);
    Mock definition = mock(Definition.class);
    Mock service = mock(Service.class);
    Mock port = mock(Port.class);
    Mock portType = mock(PortType.class);
    Mock binding = mock(Binding.class);
    Mock operation = mock(Operation.class);
    Mock input = mock(Input.class);
    Mock message = mock(Message.class);
    Mock part = mock(Part.class);
    Mock normalizedMsg = mock(NormalizedMessage.class);
    QName operationName = QName.valueOf("test");
    String partname = "";
    byte[] data = null;
    ArrayList operations = new ArrayList();
    ArrayList parts = new ArrayList();
    HashMap partmap = new HashMap();
    Map fileOps = new HashMap();
    FileOperation fileOp = new FileOperation();

    public FileNormalizerTest(String testName) {
        super(testName);
        try {
            instance = new FileNormalizer();
        } catch (Exception ex) {
            // let later test detect and fail
        }

    }

    protected void setUp() throws Exception {
        normalizedMsg.expects(atLeastOnce()).method("setContent").with(isA(DOMSource.class));
        endpoint.expects(atLeastOnce()).method("getDefinition").will(returnValue(definition.proxy()));
        endpoint.expects(atLeastOnce()).method("getServiceName").will(returnValue(QName.valueOf("service1")));
        endpoint.expects(atLeastOnce()).method("getEndpointName").will(returnValue("endpoint1"));
        endpoint.expects(atLeastOnce()).method("getMessagePartEncoderMapping").will(returnValue(new HashMap()));
        definition.expects(atLeastOnce()).method("getService").will(returnValue((Service) service.proxy()));
        service.expects(atLeastOnce()).method("getPort").will(returnValue((Port) port.proxy()));
        port.expects(atLeastOnce()).method("getBinding").will(returnValue((Binding) binding.proxy()));
        binding.expects(atLeastOnce()).method("getPortType").will(returnValue((PortType) portType.proxy()));
        portType.expects(atLeastOnce()).method("getOperations").will(returnValue(operations));
        operation.expects(atLeastOnce()).method("getInput").will(returnValue((Input) input.proxy()));
        input.expects(atLeastOnce()).method("getMessage").will(returnValue((Message) message.proxy()));
        message.expects(atLeastOnce()).method("getQName").will(returnValue(QName.valueOf("message1")));
        message.expects(atLeastOnce()).method("getOrderedParts").will(returnValue(parts));

        QName portTypeQName = new QName("");
        fileOps.put(operationName, fileOp);
        portType.expects(atLeastOnce()).method("getQName").will(returnValue(portTypeQName));
        endpoint.expects(atLeastOnce()).method("getFileOperations").will(returnValue(fileOps));

        partmap.put("part1", (Part) part.proxy());
        parts.add((Part) part.proxy());
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of normalize method, of class com.sun.jbi.filebc.FileNormalizer.
     */
    public void testNormalizeCase1() throws Exception {
        System.out.println("Testing normalize case 1.");

        NormalizedMessage result = null;

        //setting up
        FileMessage fileMessage = new FileMessage();
        exchange.expects(atLeastOnce()).method("createMessage").will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        message.expects(atLeastOnce()).method("getParts").will(returnValue(partmap));
        operations.add((Operation) operation.proxy());

        // testing the following case
        // 1. no matching operation
        operation.expects(atLeastOnce()).method("getName").will(returnValue("blah"));

        if (instance == null) {
            fail("Failed to test normalize - normalizer instance not available.");
        }

        try {
            result = instance.normalize((MessageExchange) exchange.proxy(),
                    operationName,
                    (Endpoint) endpoint.proxy(),
                    fileMessage,
                    null,
                    false);
            fail("Failed to test normalize - an exception should've been raised by FileNormalizer because no operation name match can be found.");
        } catch (Exception e) {
        }


        // testing the following case
        // 1. no part name defined
        // 2. part element is defined

        operation.expects(atLeastOnce()).method("getName").will(returnValue("test"));
        part.expects(atLeastOnce()).method("getName").will(returnValue("part1"));
        part.expects(atLeastOnce()).method("getElementName").will(returnValue(QName.valueOf("foo")));
        part.expects(atLeastOnce()).method("getTypeName").will(returnValue(QName.valueOf("foo")));
        data = "<foo>helloWorld</foo>".getBytes();

        try {
            FileStreamHandler streamHandler = new FileStreamHandler(
                    new ByteArrayInputStream(data), false, new byte[0], -1,
                    data.length);

            result = instance.normalize((MessageExchange) exchange.proxy(),
                    operationName,
                    (Endpoint) endpoint.proxy(),
                    fileMessage,
                    streamHandler, false);
            assertTrue(result instanceof NormalizedMessage);
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to test normalize due to: " + e.getMessage());
        }

        // testing the following case
        // 1. no part name defined
        // 2. part element defined 
        // 3. malformed data
        data = "<foo>helloWorld</bar>".getBytes();
        try {
            FileStreamHandler streamHandler = new FileStreamHandler(
                    new ByteArrayInputStream(data), false, null, -1,
                    data.length);
            result = instance.normalize((MessageExchange) exchange.proxy(),
                    operationName,
                    (Endpoint) endpoint.proxy(),
                    fileMessage,
                    streamHandler, false);
            fail("Failed to test normalize - an exception should've been raised by FileNormalizer due to malformed xml data");
        } catch (Exception e) {
        }

        // testing the following case
        // 1. no part name defined
        // 2. part element defined 
        // 3. invalid data
        data = "HelloWorld".getBytes();
        try {
            FileStreamHandler streamHandler = new FileStreamHandler(
                    new ByteArrayInputStream(data), false, null, -1,
                    data.length);
            result = instance.normalize((MessageExchange) exchange.proxy(),
                    operationName,
                    (Endpoint) endpoint.proxy(),
                    fileMessage,
                    streamHandler, false);
            fail("Failed to test normalize - an exception should've been raised by FileNormalizer due to data type mismatch");
        } catch (Exception e) {
        }

        System.out.print("Successfully tested normalize case 1.");
    }

    /**
     * Test of normalize method, of class com.sun.jbi.filebc.FileNormalizer.
     */
    public void testNormalizeCase2() throws Exception {
        System.out.println("Testing normalize case 2.");

        NormalizedMessage result = null;

        //setting up
        FileMessage fileMessage = new FileMessage();
        fileMessage.setPart("part1");
        exchange.expects(atLeastOnce()).method("createMessage").will(returnValue((NormalizedMessage) normalizedMsg.proxy()));
        message.expects(atLeastOnce()).method("getPart").with(eq("part1")).will(returnValue((Part) part.proxy()));
        part.expects(atLeastOnce()).method("getElementName").will(returnValue(null));
        operation.expects(atLeastOnce()).method("getName").will(returnValue("test"));
        operations.add((Operation) operation.proxy());

        // testing the following case
        // 1. part name is defined
        // 2. part type is a complex type

        part.expects(atLeastOnce()).method("getName").will(returnValue("part1"));
        part.expects(atLeastOnce()).method("getTypeName").will(returnValue(QName.valueOf("testType")));
        data = "<foo>helloWorld</foo>".getBytes();

        if (instance == null) {
            fail("Failed to test normalize - normalizer instance not available.");
        }

        try {
            FileStreamHandler streamHandler = new FileStreamHandler(
                    new ByteArrayInputStream(data), false, new byte[0], -1,
                    data.length);
            result = instance.normalize((MessageExchange) exchange.proxy(),
                    operationName,
                    (Endpoint) endpoint.proxy(),
                    fileMessage,
                    streamHandler, false);
            assertTrue(result instanceof NormalizedMessage);
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed to test normalize due to: " + e.getMessage());
        }

        // testing the following case
        // 1. part name 
        // 2. malformed data
        data = "<foo>helloWorld</bar>".getBytes();
        try {
            FileStreamHandler streamHandler = new FileStreamHandler(
                    new ByteArrayInputStream(data), false, new byte[0], -1,
                    data.length);
            result = instance.normalize((MessageExchange) exchange.proxy(),
                    operationName,
                    (Endpoint) endpoint.proxy(),
                    fileMessage,
                    streamHandler, false);
            fail("Failed to test normalize - an exception should've been raised by FileNormalizer due to malformed xml data");
        } catch (Exception e) {
        }

        // testing the following case
        // 1. part name defined
        // 2. simple type
        part.expects(atLeastOnce()).method("getTypeName").will(returnValue(new QName("http://www.w3.org/2001/XMLSchema", "string")));
        data = "HelloWorld".getBytes();
        try {
            FileStreamHandler streamHandler = new FileStreamHandler(
                    new ByteArrayInputStream(data), false, new byte[0], -1,
                    data.length);
            result = instance.normalize((MessageExchange) exchange.proxy(),
                    operationName,
                    (Endpoint) endpoint.proxy(),
                    fileMessage,
                    streamHandler, false);

        } catch (Exception e) {
            fail("Failed to test normalize due to: " + e.getMessage());
        }

        System.out.print("Successfully tested normalize case 2.");
    }
}
