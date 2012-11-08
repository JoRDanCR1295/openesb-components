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
 * @(#)SMTPDenormalizerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.smtpbc;

import com.ibm.wsdl.PartImpl;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.encoder.Encoder;
import com.sun.encoder.EncoderConfigurationException;
import com.sun.encoder.EncoderFactory;
import com.sun.encoder.MetaRef;
import com.sun.jbi.eManager.provider.EndpointStatus;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.smtpbc.extensions.MailTo;
import com.sun.jbi.smtpbc.extensions.Mailbox;
import com.sun.jbi.smtpbc.extensions.SMTPAddress;
import com.sun.jbi.smtpbc.extensions.SMTPConstants;
import com.sun.jbi.smtpbc.extensions.SMTPExtensionRegistry;
import com.sun.jbi.smtpbc.extensions.SMTPOperation;
import com.sun.jbi.smtpbc.extensions.SMTPOperationInput;
import java.util.HashMap;
import javax.jbi.messaging.NormalizedMessage;
import javax.wsdl.Message;
import javax.wsdl.Operation;
import javax.wsdl.Port;
import javax.wsdl.PortType;
import javax.wsdl.Service;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;
import org.jmock.Mock;
import com.sun.jbi.smtpbc.extservice.EmailData;
import java.io.File;
import java.io.StringReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Binding;
import javax.wsdl.Definition;
import javax.wsdl.Input;
import javax.wsdl.Part;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.xml.parsers.DocumentBuilderFactory;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.apps.resolver;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;

/**
 *
 * @author Vishnuvardhan P.R
 */
public class SMTPDenormalizerTest extends org.jmock.cglib.MockObjectTestCase {
    
    
         // some static strings
    private static final String serviceName = "serviceName";
    private static final String endpointName = "endpointName";

    private SMTPDenormalizer smtpDenormalizer = null;



    
    // mocks
    private Mock endpointMock = mock(Endpoint.class);
    private Mock endpointStatusMock = mock(EndpointStatus.class);
    private Mock serviceEndpointMock = mock(ServiceEndpoint.class);    
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
    
    public SMTPDenormalizerTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        smtpDenormalizer = new SMTPDenormalizer();



    }

    protected void tearDown() throws Exception {
    }

    /**
     * Test of denormalize method, of class com.sun.jbi.smtpbc.SMTPDenormalizer.
     */
    public void testDenormalize() throws Exception {
        System.out.println("Testing denormalize");
        
        setupExpectationsForDenormalize();
        
        String nmsg = "<jbi:message xmlns:msgns=\"urn:FooBar3\"" +
              "             xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"" +
              "             type=\"msgns:message1\"" +
              "             version=\"1.0\">" +
              "     <jbi:part>Hello World</jbi:part>" + 
              "</jbi:message>";
        
        
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(nmsg)));
        Node node = document.getDocumentElement();



        
        // NormalizedMessage mock (in message)
        Mock normalizedInMessageMock = mock(NormalizedMessage.class);
        NormalizedMessage normalizedInMessage = (NormalizedMessage)normalizedInMessageMock.proxy();
        normalizedInMessageMock.stubs().method("getContent").will(returnValue(new DOMSource(node))); 
        
         // in-only operation name
        String operation = "SMTPInOnlyOperation";
        QName operationQName = new QName(operation);
       
        
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
        


        EmailData result = smtpDenormalizer.denormalize(normalizedInMessage, operationQName, (Endpoint)endpointMock.proxy());
        assertEquals(result.getEmailMessage().getMsgText(),"Hello World") ;

        
    }

    public void testDenormalizeWithSubjectPart() throws Exception {
        System.out.println("Testing denormalize");
        
        setupExpectationsForDenormalize();
        
        String nmsg = "<jbi:message xmlns:msgns=\"urn:FooBar3\"" +
              "             xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"" +
              "             type=\"msgns:message1\"" +
              "             version=\"1.0\">" +
              "     <jbi:part>Hello World</jbi:part>" + 
              "     <jbi:part>Mail Subject</jbi:part>" +                 
              "</jbi:message>";
        
        
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(nmsg)));
        Node node = document.getDocumentElement();



        
        // NormalizedMessage mock (in message)
        Mock normalizedInMessageMock = mock(NormalizedMessage.class);
        NormalizedMessage normalizedInMessage = (NormalizedMessage)normalizedInMessageMock.proxy();
        normalizedInMessageMock.stubs().method("getContent").will(returnValue(new DOMSource(node))); 
        
         // in-only operation name
        String operation = "SMTPInOnlyOperation";
        QName operationQName = new QName(operation);
       
        
        SMTPOperation smtpOperation = new SMTPOperation();
        smtpOperation.setElementType(new QName("SMTPInOnlyOperation"));
        
        SMTPOperationInput smtpInput = new SMTPOperationInput();
        smtpInput.setMessage("part1");
        smtpInput.setSubject("part2");
        smtpInput.setSmtpUseType(SMTPConstants.SMTP_USE_TYPE_LITERAL);

        
        HashMap map = new HashMap();
        map.put(operationQName,smtpOperation);
        
        endpointMock.expects(atLeastOnce()).method("getSMTPOperations").will(returnValue(map));
        endpointMock.expects(atLeastOnce()).method("getSMTPOperationInput").will(returnValue(smtpInput));        
       

        
        ArrayList parts = new ArrayList();
        Part part1 = new PartImpl();
        part1.setName("part1");
        Part part2 = new PartImpl();
        part2.setName("part2");
        parts.add(part1);
        parts.add(part2);
        //parts.add((Part)partMock.proxy());
        //partMock.expects(atLeastOnce()).method("getName").will(returnValue("part1"));
        
        messageMock.expects(atLeastOnce()).method("getOrderedParts").will(returnValue(parts));
        
        definitionMock.expects(atLeastOnce()).method("getMessage").will(returnValue((Message)messageMock.proxy()));

        EmailData result = smtpDenormalizer.denormalize(normalizedInMessage, operationQName, (Endpoint)endpointMock.proxy());
        assertEquals(result.getEmailMessage().getMsgText(),"Hello World") ;
        assertEquals(result.getEmailMessage().getSubject(),"Mail Subject") ;
        
    }
    
    public void testDenormalizeWithFromPart() throws Exception {
        System.out.println("Testing testDenormalizeWithFromPart");
        
        setupExpectationsForDenormalize();
        
        String nmsg = "<jbi:message xmlns:msgns=\"urn:FooBar3\"" +
              "             xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"" +
              "             type=\"msgns:message1\"" +
              "             version=\"1.0\">" +
              "     <jbi:part>Hello World</jbi:part>" + 
              "     <jbi:part>Mail Subject</jbi:part>" +                 
              "     <jbi:part>fromUser@fromDomain.com</jbi:part>" +                                 
              "</jbi:message>";
        
        
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(nmsg)));
        Node node = document.getDocumentElement();



        
        // NormalizedMessage mock (in message)
        Mock normalizedInMessageMock = mock(NormalizedMessage.class);
        NormalizedMessage normalizedInMessage = (NormalizedMessage)normalizedInMessageMock.proxy();
        normalizedInMessageMock.stubs().method("getContent").will(returnValue(new DOMSource(node))); 
        
         // in-only operation name
        String operation = "SMTPInOnlyOperation";
        QName operationQName = new QName(operation);
       
        
        SMTPOperation smtpOperation = new SMTPOperation();
        smtpOperation.setElementType(new QName("SMTPInOnlyOperation"));
        
        SMTPOperationInput smtpInput = new SMTPOperationInput();
        smtpInput.setMessage("part1");
        smtpInput.setSubject("part2");
        smtpInput.setFrom("part3");
        smtpInput.setSmtpUseType(SMTPConstants.SMTP_USE_TYPE_LITERAL);

        
        HashMap map = new HashMap();
        map.put(operationQName,smtpOperation);
        
        endpointMock.expects(atLeastOnce()).method("getSMTPOperations").will(returnValue(map));
        endpointMock.expects(atLeastOnce()).method("getSMTPOperationInput").will(returnValue(smtpInput));        
       

        
        ArrayList parts = new ArrayList();
        Part part1 = new PartImpl();
        part1.setName("part1");
        Part part2 = new PartImpl();
        part2.setName("part2");
        Part part3 = new PartImpl();
        part3.setName("part3");
        parts.add(part1);
        parts.add(part2);
        parts.add(part3);
        //parts.add((Part)partMock.proxy());
        //partMock.expects(atLeastOnce()).method("getName").will(returnValue("part1"));
        
        messageMock.expects(atLeastOnce()).method("getOrderedParts").will(returnValue(parts));
        
        definitionMock.expects(atLeastOnce()).method("getMessage").will(returnValue((Message)messageMock.proxy()));
        


        EmailData result = smtpDenormalizer.denormalize(normalizedInMessage, operationQName, (Endpoint)endpointMock.proxy());
        assertEquals(result.getEmailMessage().getMsgText(),"Hello World") ;
        assertEquals(result.getEmailMessage().getSubject(),"Mail Subject") ;
        assertEquals(result.getEmailMessage().getFrom().getAddress(),"fromUser@fromDomain.com") ;        
        
        
    }
        
    public void testDenormalizeWithEncoderSupport() throws Exception {
        System.out.println("Testing testDenormalizeWithFromPart");
        
        setupExpectationsForDenormalize();
        
                   
       String nmsg="<?xml version=\"1.0\" encoding=\"UTF-8\"?><jbi:message type=\"msgns:message1\" version=\"1.0\" xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"><jbi:part><root xmlns=\"http://xml.netbeans.org/schema/HelloWorldSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://xml.netbeans.org/schema/HelloWorldSchema \"><Verb>Hello</Verb><Noun>World</Noun><Punct>!</Punct></root></jbi:part></jbi:message>";
  
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(nmsg)));
        Node node = document.getDocumentElement();

       
        // NormalizedMessage mock (in message)
        Mock normalizedInMessageMock = mock(NormalizedMessage.class);
        NormalizedMessage normalizedInMessage = (NormalizedMessage)normalizedInMessageMock.proxy();
        
        normalizedInMessageMock.stubs().method("getContent").will(returnValue(new DOMSource(node))); 
        
         // in-only operation name
        String operation = "SMTPInOnlyOperation";
        QName operationQName = new QName(operation);
       
        
        SMTPOperation smtpOperation = new SMTPOperation();
        smtpOperation.setElementType(new QName("SMTPInOnlyOperation"));
        
        SMTPOperationInput smtpInput = new SMTPOperationInput();
        smtpInput.setMessage("part1");
        smtpInput.setSmtpUseType(SMTPConstants.SMTP_USE_TYPE_ENCODED);
        
        URL xsdURL = this.getClass().getClassLoader().getResource("com/sun/jbi/smtpbc/HelloWorldSchema.xsd");
        String encodingStyle = "customencoder-1.0";
        QName qname = new QName("http://xml.netbeans.org/schema/HelloWorldSchema","root");
        TestMetaRef testMetaRef = new TestMetaRef(xsdURL.getFile(),qname);
        EncoderFactory encoderFactory = EncoderFactory.newInstance();
        Encoder encoder = null;

        try {
            encoder = encoderFactory.newEncoder(encoderFactory.makeType(encodingStyle), testMetaRef);        
        }
        catch (EncoderConfigurationException ece) {
            ece.printStackTrace();
            fail(ece.getMessage());
        }

        HashMap partEncoderMapping = new HashMap();
        partEncoderMapping.put("message1part1",encoder);

        endpointMock.expects(once()).method("getMessagePartEncoderMapping").will(returnValue(partEncoderMapping));
        

        
        HashMap map = new HashMap();
        map.put(operationQName,smtpOperation);
        
        endpointMock.expects(atLeastOnce()).method("getSMTPOperations").will(returnValue(map));
        endpointMock.expects(atLeastOnce()).method("getSMTPOperationInput").will(returnValue(smtpInput));        
       

        
        ArrayList parts = new ArrayList();
        Part part1 = new PartImpl();
        part1.setName("part1");
        parts.add(part1);
       
        messageMock.expects(atLeastOnce()).method("getOrderedParts").will(returnValue(parts));
        
        definitionMock.expects(atLeastOnce()).method("getMessage").will(returnValue((Message)messageMock.proxy()));

        EmailData result = smtpDenormalizer.denormalize(normalizedInMessage, operationQName, (Endpoint)endpointMock.proxy());
        assertEquals(result.getEmailMessage().getMsgText(),"Hello|World|!");
        
    }

    
    public void testDenormalizeWithoutMessagePart() throws Exception {
        System.out.println("Testing denormalize");
        
        setupExpectationsForDenormalize();
        
        String nmsg = "<jbi:message xmlns:msgns=\"urn:FooBar3\"" +
              "             xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"" +
              "             type=\"msgns:message1\"" +
              "             version=\"1.0\">" +
              "     <jbi:part>Hello World</jbi:part>" + 
              "</jbi:message>";
        
        
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(nmsg)));
        Node node = document.getDocumentElement();

        
        // NormalizedMessage mock (in message)
        Mock normalizedInMessageMock = mock(NormalizedMessage.class);
        NormalizedMessage normalizedInMessage = (NormalizedMessage)normalizedInMessageMock.proxy();
        normalizedInMessageMock.stubs().method("getContent").will(returnValue(new DOMSource(node))); 
        
         // in-only operation name
        String operation = "SMTPInOnlyOperation";
        QName operationQName = new QName(operation);
       
        
        SMTPOperation smtpOperation = new SMTPOperation();
        smtpOperation.setElementType(new QName("SMTPInOnlyOperation"));
        
        SMTPOperationInput smtpInput = new SMTPOperationInput();
        smtpInput.setSubject("part1");
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


        EmailData result = smtpDenormalizer.denormalize(normalizedInMessage, operationQName, (Endpoint)endpointMock.proxy());
        assertEquals("Hello World",result.getEmailMessage().getSubject()) ;

        
    }
    
    public void testDenormalizeWithoutAnyPart() throws Exception {
        System.out.println("Testing denormalize");
        
        setupExpectationsForDenormalize();
        
        String nmsg = "<jbi:message xmlns:msgns=\"urn:FooBar3\"" +
              "             xmlns:jbi=\"http://java.sun.com/xml/ns/jbi/wsdl-11-wrapper\"" +
              "             type=\"msgns:message1\"" +
              "             version=\"1.0\">" +
              "</jbi:message>";
        
        
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(nmsg)));
        Node node = document.getDocumentElement();

        
        // NormalizedMessage mock (in message)
        Mock normalizedInMessageMock = mock(NormalizedMessage.class);
        NormalizedMessage normalizedInMessage = (NormalizedMessage)normalizedInMessageMock.proxy();
        normalizedInMessageMock.stubs().method("getContent").will(returnValue(new DOMSource(node))); 
        
         // in-only operation name
        String operation = "SMTPInOnlyOperation";
        QName operationQName = new QName(operation);
       
        
        SMTPOperation smtpOperation = new SMTPOperation();
        smtpOperation.setElementType(new QName("SMTPInOnlyOperation"));
        
        SMTPOperationInput smtpInput = new SMTPOperationInput();
        smtpInput.setSmtpUseType(SMTPConstants.SMTP_USE_TYPE_LITERAL);

        
        HashMap map = new HashMap();
        map.put(operationQName,smtpOperation);
        
        endpointMock.expects(atLeastOnce()).method("getSMTPOperations").will(returnValue(map));
        endpointMock.expects(atLeastOnce()).method("getSMTPOperationInput").will(returnValue(smtpInput));        
       
       
        ArrayList parts = new ArrayList();
        messageMock.expects(atLeastOnce()).method("getOrderedParts").will(returnValue(parts));
        
        definitionMock.expects(atLeastOnce()).method("getMessage").will(returnValue((Message)messageMock.proxy()));


        EmailData result = smtpDenormalizer.denormalize(normalizedInMessage, operationQName, (Endpoint)endpointMock.proxy());
       
    }    
    
    
    /**
     * Test of transformPart method, of class com.sun.jbi.smtpbc.SMTPDenormalizer.
     */
    public void testTransformPart() throws Exception {
        System.out.println("transformPart");
        
        String xmlString = "<root><node>data</node></root>";
        
        Document document = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(new InputSource(new StringReader(xmlString)));
        Node node = document.getDocumentElement();        

        SMTPDenormalizer instance = new SMTPDenormalizer();
        
        String expResult = "<root>\n<node>data</node>\n</root>";
        String result = instance.transformPart(document.getChildNodes());
        
        System.out.println("Result = " + result);
               
    } 
    
    private void setupExpectationsForDenormalize() throws Exception{
                // Endpoint Mock
        endpointMock.expects(atLeastOnce()).method("getDefinition").will(returnValue((Definition)definitionMock.proxy()));
        endpointMock.expects(atLeastOnce()).method("getServiceName").will(returnValue(new QName(serviceName)));
        endpointMock.expects(atLeastOnce()).method("getEndpointName").will(returnValue(endpointName));
        //endpointMock.expects(atLeastOnce()).method("getEndpointStatus").will(returnValue(endpointStatus));
        
        SMTPAddress smtpAddress = new SMTPAddress();
        Mailbox toMailBox = new Mailbox();
        toMailBox.unmarshal("mailwayi@boreas.stc.com");
        MailTo mailTo = new MailTo();
        mailTo.addMailbox(toMailBox);
        smtpAddress.setLocation(mailTo);
        smtpAddress.setSMTPServer("boreas.stc.com");
        
        endpointMock.expects(atLeastOnce()).method("getSMTPAddress").will(returnValue(smtpAddress));
        

        //Definition Mock
        definitionMock.expects(once()).method("getService").will(returnValue((Service)serviceMock.proxy()));
        
        //Service Mock
        serviceMock.expects(once()).method("getPort").will(returnValue((Port)portMock.proxy()));     
        
        //Port Mock
        portMock.expects(once()).method("getBinding").will(returnValue((Binding)bindingMock.proxy()));    
        
        //Binding Mock
        bindingMock.expects(once()).method("getPortType").will(returnValue((PortType)portTypeMock.proxy())) ;       


        //Message Mock
        messageMock.expects(atLeastOnce()).method("getQName").will(returnValue(new QName("message1")));
        
        //Input Mock
        inputMock.expects(once()).method("getMessage").will(returnValue((Message)messageMock.proxy()));
        

        operationMock.expects(atLeastOnce()).method("getName").will(returnValue("SMTPInOnlyOperation"));
        operationMock.expects(atLeastOnce()).method("getInput").will(returnValue((Input)inputMock.proxy()));
        
        
        List operations = new ArrayList();        
        operations.add((Operation)operationMock.proxy());
        
        
        //PortType Mock
        portTypeMock.expects(once()).method("getOperations").will(returnValue(operations));       

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
    
    private Definition returnWSDLDefinition(String wsdlFilePath) throws Exception{
        
        final CatalogManager catalogManager = new CatalogManager();
        catalogManager.setCatalogFiles("xml-catalog.xml");
        catalogManager.setRelativeCatalogs(true);
        final EntityResolver resolver = new CatalogResolver(catalogManager);

        final WSDLFactory wsdlFactory = WSDLFactory.newInstance();
        final WSDLReader reader =
            ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        reader.setExtensionRegistry(new SMTPExtensionRegistry(new HashMap()));
        final Definition def = reader.readWSDL(wsdlFilePath);
        
        return def;

    }

    
}
