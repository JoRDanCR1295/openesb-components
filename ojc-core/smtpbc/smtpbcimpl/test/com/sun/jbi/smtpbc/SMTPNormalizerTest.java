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
 * @(#)SMTPNormalizerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc;

import com.ibm.wsdl.PartImpl;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.jbi.smtpbc.extensions.SMTPConstants;
import com.sun.jbi.smtpbc.extensions.SMTPOperation;
import com.sun.jbi.smtpbc.extensions.SMTPOperationInput;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessageExchange;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Part;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import org.jmock.Mock;

/**
 *
 * @author Vishnuvardhan P.R
 */
public class SMTPNormalizerTest extends org.jmock.cglib.MockObjectTestCase {
    
    
   //Mock objects
    Mock endpointMock = mock(Endpoint.class);    
    Mock normalizedMessageMock = mock(NormalizedMessage.class);
    Mock messageExchangeMock = mock(MessageExchange.class);    
    Mock smtpOperationMock = mock(SMTPOperation.class);
    Mock smtpOperationInputMock = mock(SMTPOperationInput.class);
    Mock serviceMock = mock(Service.class);
    Mock portMock = mock(Port.class);
    Mock portTypeMock = mock(PortType.class);
    Mock messageMock = mock(Message.class);
    Mock inputMock = mock(Input.class);
    Mock definitionMock = mock(Definition.class);
    Mock bindingMock = mock(Binding.class);
    Mock operationMock = mock(Operation.class);
    Mock partMock = mock(Part.class);
           
    
    
    SMTPNormalizer instance = null;
    String operation = null;
    QName operationQName = null;
    
    public SMTPNormalizerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {

        instance = new SMTPNormalizer();

                // in-only operation name
        operation = "SMTPInOnlyOperation";
        operationQName = new QName(operation);

        HashMap smtpOperationsMap = new HashMap();
        smtpOperationsMap.put(operationQName,(SMTPOperation)smtpOperationMock.proxy());

        endpointMock.expects(atLeastOnce()).method("getEndpointName").will(returnValue("endpoint1"));
        endpointMock.expects(atLeastOnce()).method("getServiceName").will(returnValue(new QName("service1")));
        endpointMock.expects(atLeastOnce()).method("getSMTPOperations").will(returnValue(smtpOperationsMap));
        endpointMock.expects(atLeastOnce()).method("getSMTPOperationInput").will(returnValue((SMTPOperationInput)smtpOperationInputMock.proxy()));
        endpointMock.expects(atLeastOnce()).method("getDefinition").will(returnValue((Definition)definitionMock.proxy()));
        definitionMock.expects(atLeastOnce()).method("getService").will(returnValue((Service)serviceMock.proxy()));
        serviceMock.expects(atLeastOnce()).method("getPort").will(returnValue((Port)portMock.proxy()));
        portMock.expects(atLeastOnce()).method("getBinding").will(returnValue((Binding)bindingMock.proxy()));
        bindingMock.expects(atLeastOnce()).method("getPortType").will(returnValue((PortType)portTypeMock.proxy()));

        ArrayList operationsList = new ArrayList();
        operationsList.add((Operation)operationMock.proxy());
        portTypeMock.expects(atLeastOnce()).method("getOperations").will(returnValue(operationsList));

        operationMock.expects(atLeastOnce()).method("getName").will(returnValue("SMTPInOnlyOperation"));
        operationMock.expects(atLeastOnce()).method("getInput").will(returnValue((Input)inputMock.proxy()));

        inputMock.expects(atLeastOnce()).method("getMessage").will(returnValue(messageMock.proxy()));

        ArrayList parts = new ArrayList();
        //parts.add((Part)partMock.proxy());
        Part part1 = new PartImpl();
        part1.setName("part1");
        Part part2 = new PartImpl();
        part2.setName("part2");
        Part part3 = new PartImpl();
        part3.setName("part3");
        
        parts.add(part1);
        parts.add(part2);
        parts.add(part3);
        
        //partMock.expects(atLeastOnce()).method("getName").will(returnValue("part1"));

        messageMock.expects(atLeastOnce()).method("getOrderedParts").will(returnValue(parts));
        messageMock.expects(atLeastOnce()).method("getQName").will(returnValue(new QName("message1")));
        
    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of normalize method, of class com.sun.jbi.smtpbc.SMTPNormalizer.
     */
    public void testNormalize() throws Exception{

        System.out.println("Testing normalize");

        StringBuffer message = new StringBuffer("Date: Fri, 29 Dec 2006 12:49:32 +0530\n");
        message.append("From: Vishnu <Vishnuvardhan.piskalaramesh@sun.com>\n");
        message.append("User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.7.1) Gecko/20040707\n");
        message.append("X-Accept-Language: en-us, en\n");
        message.append("MIME-Version: 1.0\n");
        message.append("To:  someUser@localhost.com");
        message.append("Subject: test\n");
        message.append("Content-Type: text/plain; charset=us-ascii; format=flowed\n");
        message.append("Content-Transfer-Encoding: 7bit\n");
        message.append("\n");
        message.append("Hello World !");

        byte[] byteMessage = message.toString().getBytes();

        MessageExchange messageExchange = (MessageExchange)messageExchangeMock.proxy();
        messageExchangeMock.expects(once()).method("createMessage").will(returnValue((NormalizedMessage)normalizedMessageMock.proxy()));
        
        normalizedMessageMock.expects(once()).method("setContent");

        smtpOperationInputMock.expects(atLeastOnce()).method("getSubject").will(returnValue(null));
        smtpOperationInputMock.expects(atLeastOnce()).method("getMessage").will(returnValue("part1"));
        smtpOperationInputMock.expects(atLeastOnce()).method("getFrom").will(returnValue(null));
        smtpOperationInputMock.expects(atLeastOnce()).method("getSmtpUseType").will(returnValue(SMTPConstants.SMTP_USE_TYPE_LITERAL));
        smtpOperationInputMock.expects(atLeastOnce()).method("getTo").will(returnValue(null));
        smtpOperationInputMock.expects(atLeastOnce()).method("getCc").will(returnValue(null));

        instance.normalize(byteMessage, (MessageExchange)messageExchangeMock.proxy(), operationQName, (Endpoint)endpointMock.proxy());

    }
    
    public void testNormalizeWithEncoderSupport() throws Exception {
            
            System.out.println("Testing testNormalizeWithEncoderSupport()");
            
            StringBuffer message = new StringBuffer("Date: Fri, 29 Dec 2006 12:49:32 +0530\n");
            message.append("From: Vishnu <Vishnuvardhan.piskalaramesh@sun.com>\n");
            message.append("User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.7.1) Gecko/20040707\n");
            message.append("X-Accept-Language: en-us, en\n");
            message.append("MIME-Version: 1.0\n");
            message.append("To:  someUser@localhost.com");
            message.append("Subject: test\n");
            message.append("Content-Type: text/plain; charset=us-ascii; format=flowed\n");
            message.append("Content-Transfer-Encoding: 7bit\n");
            message.append("\n");
            message.append("Hello|World|!");

            byte[] byteMessage = message.toString().getBytes();

            MessageExchange messageExchange = (MessageExchange)messageExchangeMock.proxy();
            messageExchangeMock.expects(once()).method("createMessage").will(returnValue((NormalizedMessage)normalizedMessageMock.proxy()));
            
            normalizedMessageMock.expects(atLeastOnce()).method("setContent");

            smtpOperationInputMock.expects(once()).method("getSubject").will(returnValue(null));
            smtpOperationInputMock.expects(atLeastOnce()).method("getMessage").will(returnValue("part1"));
            smtpOperationInputMock.expects(once()).method("getFrom").will(returnValue(null));
            smtpOperationInputMock.expects(once()).method("getSmtpUseType").will(returnValue(SMTPConstants.SMTP_USE_TYPE_ENCODED));
            smtpOperationInputMock.expects(atLeastOnce()).method("getTo").will(returnValue(null));
            smtpOperationInputMock.expects(atLeastOnce()).method("getCc").will(returnValue(null));
            
            //String xsdLoc = "demo.xsd";
            //URL xsdURL = this.getClass().getClassLoader().getResource("com/sun/jbi/smtpbc/demo.xsd");
            URL xsdURL = this.getClass().getClassLoader().getResource("com/sun/jbi/smtpbc/HelloWorldSchema.xsd");
            //String encodingStyle = "ud1encoder-1.0";
            String encodingStyle = "customencoder-1.0";
            TestMetaRef testMetaRef = new TestMetaRef(xsdURL.getFile(),new QName("http://xml.netbeans.org/schema/HelloWorldSchema","root"));
            //TestMetaRef testMetaRef = new TestMetaRef(xsdURL.getFile(),new QName("part1"));
            EncoderFactory encoderFactory = EncoderFactory.newInstance();
            Encoder encoder = encoderFactory.newEncoder(encoderFactory.makeType(encodingStyle), testMetaRef);        
            
            HashMap partEncoderMapping = new HashMap();
            partEncoderMapping.put("message1part1",encoder);
            
            endpointMock.expects(once()).method("getMessagePartEncoderMapping").will(returnValue(partEncoderMapping));

            NormalizedMessage nmsg = instance.normalize(byteMessage, (MessageExchange)messageExchangeMock.proxy(), operationQName, (Endpoint)endpointMock.proxy());
            if(nmsg == null){
                fail();
            }
    }
    
    public void testNormalizeWithSubjectPart() throws Exception {
        System.out.println("Testing testNormalizeWithSubjectPart");

        StringBuffer message = new StringBuffer("Date: Fri, 29 Dec 2006 12:49:32 +0530\n");
        message.append("From: Vishnu <Vishnuvardhan.piskalaramesh@sun.com>\n");
        message.append("User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.7.1) Gecko/20040707\n");
        message.append("X-Accept-Language: en-us, en\n");
        message.append("MIME-Version: 1.0\n");
        message.append("To:  someUser@localhost.com");
        message.append("Subject: test\n");
        message.append("Content-Type: text/plain; charset=us-ascii; format=flowed\n");
        message.append("Content-Transfer-Encoding: 7bit\n");
        message.append("\n");
        message.append("Hello World !");

        byte[] byteMessage = message.toString().getBytes();

        MessageExchange messageExchange = (MessageExchange)messageExchangeMock.proxy();
        messageExchangeMock.expects(once()).method("createMessage").will(returnValue((NormalizedMessage)normalizedMessageMock.proxy()));
        
        normalizedMessageMock.expects(once()).method("setContent");

        smtpOperationInputMock.expects(atLeastOnce()).method("getSubject").will(returnValue("part2"));
        smtpOperationInputMock.expects(atLeastOnce()).method("getMessage").will(returnValue("part1"));
        smtpOperationInputMock.expects(atLeastOnce()).method("getFrom").will(returnValue(null));
        smtpOperationInputMock.expects(atLeastOnce()).method("getSmtpUseType").will(returnValue(SMTPConstants.SMTP_USE_TYPE_LITERAL));
        smtpOperationInputMock.expects(atLeastOnce()).method("getTo").will(returnValue(null));
        smtpOperationInputMock.expects(atLeastOnce()).method("getCc").will(returnValue(null));        


        instance.normalize(byteMessage, (MessageExchange)messageExchangeMock.proxy(), operationQName, (Endpoint)endpointMock.proxy());
        
    }
    
    public void testNormalizeWithFromPart() throws Exception {
        System.out.println("Testing testNormalizeWithSubjectPart");

        StringBuffer message = new StringBuffer("Date: Fri, 29 Dec 2006 12:49:32 +0530\n");
        message.append("From: Vishnu <Vishnuvardhan.piskalaramesh@sun.com>\n");
        message.append("User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.7.1) Gecko/20040707\n");
        message.append("X-Accept-Language: en-us, en\n");
        message.append("MIME-Version: 1.0\n");
        message.append("To:  someUser@localhost.com");
        message.append("Subject: test\n");
        message.append("Content-Type: text/plain; charset=us-ascii; format=flowed\n");
        message.append("Content-Transfer-Encoding: 7bit\n");
        message.append("\n");
        message.append("Hello World !");

        byte[] byteMessage = message.toString().getBytes();

        MessageExchange messageExchange = (MessageExchange)messageExchangeMock.proxy();
        messageExchangeMock.expects(once()).method("createMessage").will(returnValue((NormalizedMessage)normalizedMessageMock.proxy()));
        
        normalizedMessageMock.expects(once()).method("setContent");

        smtpOperationInputMock.expects(atLeastOnce()).method("getSubject").will(returnValue("part2"));
        smtpOperationInputMock.expects(atLeastOnce()).method("getMessage").will(returnValue("part1"));
        smtpOperationInputMock.expects(atLeastOnce()).method("getFrom").will(returnValue("part3"));
        smtpOperationInputMock.expects(atLeastOnce()).method("getSmtpUseType").will(returnValue(SMTPConstants.SMTP_USE_TYPE_LITERAL));
        smtpOperationInputMock.expects(atLeastOnce()).method("getTo").will(returnValue(null));
        smtpOperationInputMock.expects(atLeastOnce()).method("getCc").will(returnValue(null));        


        instance.normalize(byteMessage, (MessageExchange)messageExchangeMock.proxy(), operationQName, (Endpoint)endpointMock.proxy());
        
    } 
    
    public void testNormalizeWithXMLData() throws Exception {
        System.out.println("Testing testNormalizeWithSubjectPart");

        StringBuffer message = new StringBuffer("Date: Fri, 29 Dec 2006 12:49:32 +0530\n");
        message.append("From: Vishnu <Vishnuvardhan.piskalaramesh@sun.com>\n");
        message.append("User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.7.1) Gecko/20040707\n");
        message.append("X-Accept-Language: en-us, en\n");
        message.append("MIME-Version: 1.0\n");
        message.append("To:  someUser@localhost.com");
        message.append("Subject: test\n");
        message.append("Content-Type: text/plain; charset=us-ascii; format=flowed\n");
        message.append("Content-Transfer-Encoding: 7bit\n");
        message.append("\n");
        message.append("<Greeting> Hello World ! </Greeting>");

        byte[] byteMessage = message.toString().getBytes();

        MessageExchange messageExchange = (MessageExchange)messageExchangeMock.proxy();
        messageExchangeMock.expects(once()).method("createMessage").will(returnValue((NormalizedMessage)normalizedMessageMock.proxy()));
        
        normalizedMessageMock.expects(once()).method("setContent");

        smtpOperationInputMock.expects(atLeastOnce()).method("getSubject").will(returnValue("part2"));
        smtpOperationInputMock.expects(atLeastOnce()).method("getMessage").will(returnValue("part1"));
        smtpOperationInputMock.expects(atLeastOnce()).method("getFrom").will(returnValue("part3"));
        smtpOperationInputMock.expects(atLeastOnce()).method("getSmtpUseType").will(returnValue(SMTPConstants.SMTP_USE_TYPE_LITERAL));
        smtpOperationInputMock.expects(atLeastOnce()).method("getTo").will(returnValue(null));
        smtpOperationInputMock.expects(atLeastOnce()).method("getCc").will(returnValue(null));        


        instance.normalize(byteMessage, (MessageExchange)messageExchangeMock.proxy(), operationQName, (Endpoint)endpointMock.proxy());
        
    }
    
        /**
     * An implementation of the MetaRef interface
     */
    private class TestMetaRef implements MetaRef {
        private final String mXsdPath;

        private final QName mRootElemName;

        private final String mToString;

        /**
         * Constructor
         */
        protected TestMetaRef(final String xsdLoc) {
            this(xsdLoc, null);
        }

        /**
         * Alternative constructor that constructs a MetaRef object with the file path location of
         * the main XSD and qualified name of the root element
         */
        protected TestMetaRef(final String xsdLoc, final QName rootElemName) {
            mXsdPath = xsdLoc;
            mRootElemName = rootElemName;
            mToString = toString();
        }

        /**
         * Gets the URL of the main metadata file.  This URL should point to an
         * XSD file somewhere.  If this method returns a value other than
         * <code>null</code>, the return value of <code>getPath()</code> will
         * be ignored.  To load encoder metadata from a jar file, a URL in form
         * "jar:&lt;url&gt;!/{entry}" can be used.
         * 
         * @return the URL of the main meta file
         */
        public URL getURL() {
            return null;
        }        
        /**
         * Return the file path location of the main XSD
         * 
         * @return the path of the main meta file
         */
        public String getPath() {
            return mXsdPath;
        }

        /**
         * Return the QName of the root element.
         * 
         * @return the QName of the root element
         */
        public QName getRootElemName() {
            return mRootElemName;
        }

        @Override
		public String toString() {
            return mXsdPath + mRootElemName.toString();
        }

        @Override
		public boolean equals(final Object obj) {
            if (!(obj instanceof TestMetaRef)) {
                return false;
            }
            return mToString.equals(((TestMetaRef) obj).mToString);
        }

        @Override
		public int hashCode() {
            return mToString.hashCode();
        }
    }
}
