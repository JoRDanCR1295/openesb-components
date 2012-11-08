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

package com.sun.jbi.execbc;

import junit.framework.*;
import com.ibm.wsdl.DefinitionImpl;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.execbc.Endpoint;
import com.sun.jbi.execbc.EndpointImpl;
import com.sun.jbi.execbc.ExecDenormalizer;
import com.sun.jbi.execbc.MessageExchangeReplyListener;
import com.sun.jbi.execbc.OutboundMessageProcessor;
import com.sun.jbi.execbc.ServiceUnit;
import com.sun.jbi.execbc.Endpoint.EndpointState;
import com.sun.jbi.execbc.extensions.ExecAddress;
import com.sun.jbi.execbc.extensions.ExecBinding;
import com.sun.jbi.execbc.extensions.ExecInput;
import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.extensions.ExecOperation;
import com.sun.jbi.execbc.extensions.ExecOutput;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.Map;
import javax.jbi.messaging.*;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.jmock.*;


/**
 *
 * @author sweng
 */
public class OutboundMessageProcessorTest extends org.jmock.cglib.MockObjectTestCase {
    static final QName THE_PORTTYPE = new QName("http://myfile-test", "portType1");
    static final QName THE_OPERATION  = new QName("http://myfile-test", "operation1");
    static final QName THE_SERVICE = new QName("http://myfile-test", "MyFileService");
    static final String THE_ENDPOINT = "MyFilePort";
    byte[] data = null;
    
    OutboundMessageProcessor instance = null;
    Endpoint endpoint = null;
    QName operation = null;
    Map<QName, Service> services = null;
    
    Mock deliveryChannel = null;
    Mock endpointProxy = null;
    Mock endpointStatus = null;
    Mock serviceUnit = null;
    Mock service = null;
    Mock port = null;
    Mock portType = null;
    Mock binding = null;
    Mock wsdlDefinition = null;
    Mock normalizer = null;
    Mock normalizedMsg = null;
    
    List endpoints = new ArrayList();
    Map serviceUnits = new HashMap();
    Map operations = new HashMap();
    
    Document doc = null; 
    
    public OutboundMessageProcessorTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        data = "".getBytes();
        wsdlDefinition = mock(Definition.class);
        operation = THE_OPERATION;
        deliveryChannel = mock(DeliveryChannel.class);
        endpointProxy = mock(Endpoint.class);
        endpointStatus = mock(EndpointStatus.class);
        endpoint = (Endpoint) endpointProxy.proxy();
        service = mock(Service.class);
        port = mock(Port.class);
        portType = mock(PortType.class);
        binding = mock(Binding.class);
        serviceUnit = mock(ServiceUnit.class);
        endpoints.add(endpointProxy.proxy());
        serviceUnits.put("1", serviceUnit.proxy());
        normalizer = mock(ExecNormalizer.class);
        normalizedMsg = mock(NormalizedMessage.class);
        
        services = new HashMap<QName, Service>();
        services.put(THE_SERVICE, (Service) service.proxy());

         try {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            synchronized(dbf) {
                dbf.setNamespaceAware(true);
                doc = dbf.newDocumentBuilder().newDocument();
            }
        } catch(Exception e) {
            System.out.println("Cannot create a document");
        }
        
        if (doc != null) {
            org.w3c.dom.Element root = doc.createElement("Simple-exec-invoke-test");
            doc.appendChild(root);
        }
        
        instance = new OutboundMessageProcessor((DeliveryChannel)deliveryChannel.proxy(),
                                                serviceUnits,
                                                new HashMap());
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(OutboundMessageProcessorTest.class);
        
        return suite;
    }

    /**
     * Test of processRequestReplyOutbound method, of class com.sun.jbi.execbc.OutboundMessageProcessor.
     */
    public void testProcessRequestReplyOutbound() {
        System.out.println("Testing processRequestReplyOutbound");
        
        Mock inout = mock(InOut.class);
        Mock inMessage = mock(NormalizedMessage.class);
        Mock outMessage =  mock(NormalizedMessage.class);
        Mock denormalizer = mock(ExecDenormalizer.class);
        
        ExecOperation execoperation = new ExecOperation();
        ExecInput execInput = new ExecInput();
        ExecOutput execOutput = new ExecOutput();
        ExecMessage execMessage = new ExecMessage();
        ExecAddress addr = new ExecAddress();
        
        execInput.setExecMessage(execMessage);
        execOutput.setExecMessage(execMessage);
        execoperation.setExecOperationInput(execInput);
        execoperation.setExecOperationOutput(execOutput);
        String command;
        String os = System.getProperty("os.name");
        if (os != null) {
            os = os.toLowerCase();
        }
        if (os != null && os.startsWith("windows")) {
            command = "hostname";
        } else if (os != null && os.startsWith("mac os")) {
            command = "vm_stat";
        } else {
            command = "vmstat";
        }
        execoperation.setCommand(command);
        operations.put(THE_OPERATION, execoperation);
        addr.setHostName("localhost");
        
        normalizer.expects(atLeastOnce()).method("normalize").withAnyArguments().will(returnValue((NormalizedMessage)normalizedMsg.proxy()));
        endpointProxy.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus.proxy()));
        endpointProxy.expects(atLeastOnce()).method("getExecOperations").will(returnValue(operations));
        endpointProxy.expects(atLeastOnce()).method("getExecAddress").will(returnValue(addr));
        endpointProxy.expects(atLeastOnce()).method("getServiceName").will(returnValue(THE_SERVICE));
        endpointProxy.expects(atLeastOnce()).method("getEndpointName").will(returnValue(THE_ENDPOINT));
        deliveryChannel.expects(atLeastOnce()).method("send").with(isA(InOut.class));
        endpointProxy.expects(atLeastOnce()).method("getDefinition").will(returnValue(wsdlDefinition.proxy()));
        wsdlDefinition.expects(atLeastOnce()).method("getServices").will(returnValue((Map<QName, Service>)services));
        //wsdlDefinition.expects(atLeastOnce()).method("getService").with(isA(QName.class)).will(returnValue(service.proxy()));
        service.expects(atLeastOnce()).method("getPort").will(returnValue((Port) port.proxy()));
        port.expects(atLeastOnce()).method("getBinding").will(returnValue((Binding) binding.proxy()));
        binding.expects(atLeastOnce()).method("getPortType").will(returnValue((PortType) portType.proxy()));
        portType.expects(atLeastOnce()).method("getQName").will(returnValue(THE_PORTTYPE));

        // 1. testing the case when exchange status is done
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.DONE));
        endpointStatus.expects(atLeastOnce()).method("incrementReceivedDones");
        instance.processRequestReplyOutbound((InOut)inout.proxy(), endpoint, operation);
        endpointStatus.verify();
        
        // 2. testing the case when exchange status is error
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ERROR));
        endpointStatus.expects(once()).method("incrementReceivedErrors");
        instance.processRequestReplyOutbound((InOut)inout.proxy(), endpoint, operation);
        endpointStatus.verify();
        
        // 3. testing the case when status is active and file write is successful
        endpointStatus.expects(atLeastOnce()).method("incrementReceivedRequests");
        endpointStatus.expects(atLeastOnce()).method("incrementSentReplies");
        denormalizer.expects(atLeastOnce()).method("denormalize").withAnyArguments().will(returnValue(data));
        instance.setExecDenormalizer((ExecDenormalizer)denormalizer.proxy());
        instance.setExecNormalizer((ExecNormalizer)normalizer.proxy());
        inout.expects(atLeastOnce()).method("getStatus").will(returnValue(ExchangeStatus.ACTIVE));
        inout.expects(atLeastOnce()).method("getInMessage").will(returnValue(inMessage.proxy()));
        //inout.expects(atLeastOnce()).method("createMessage").will(returnValue(outMessage.proxy()));
        inout.expects(atLeastOnce()).method("setOutMessage").withAnyArguments();
        //outMessage.expects(atLeastOnce()).method("setContent").withAnyArguments();
        //outMessage.expects(atLeastOnce()).method("setProperty").withAnyArguments();
        instance.processRequestReplyOutbound((InOut)inout.proxy(), endpoint, operation);
        inMessage.verify();
        outMessage.verify();        
    }
}
