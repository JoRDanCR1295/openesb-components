#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://${package}.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(${symbol_pound})WSDLConfigurationsTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.filebc.packaging;

import junit.framework.*;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.ibm.wsdl.BindingOperationImpl;
import com.ibm.wsdl.OperationImpl;
import com.sun.jbi.filebc.Endpoint;
import com.sun.jbi.filebc.extensions.FileAddress;
import com.sun.jbi.filebc.extensions.FileBinding;
import com.sun.jbi.filebc.extensions.FileInput;
import com.sun.jbi.filebc.extensions.FileOperation;
import com.sun.jbi.filebc.extensions.FileOutput;
import com.sun.jbi.filebc.extensions.FileMessage;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import javax.wsdl.Binding;
import javax.wsdl.BindingOperation;
import javax.wsdl.Definition;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.Operation;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import org.xml.sax.EntityResolver;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;

/**
 * * *
 * @author sweng
 */
public class WSDLConfigurationsTest extends TestCase {

    static final int INBOUND = 0;
    static final int OUTBOUND = 1;
    WSDLConfigurations instance;
    BindingOperation bo;
    Operation operation;
    EntityResolver resolver;
    Definition definition;
    WSDLReader reader;
    WSDLFactory wsdlFactory;
    List endpoints = new ArrayList();

    public WSDLConfigurationsTest(String testName) {
        super(testName);
    }

    protected void setUp()
            throws Exception {
        instance = new WSDLConfigurations("testDir");
        bo = new BindingOperationImpl();
        operation = new OperationImpl();
        resolver = new CatalogResolver(new CatalogManager());
        wsdlFactory = WSDLFactory.newInstance();
        reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(resolver);
        definition = reader.readWSDL((new File("test/com/sun/jbi/filebc/packaging/wsdls/TestFile.wsdl")).getAbsolutePath());
    }

    protected void tearDown()
            throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(WSDLConfigurationsTest.class);
        return suite;
    }

    /**
     * Test of parseWSDL method, of class com.sun.jbi.filebc.packaging.WSDLConfigurations
     */
    public void testParseWSDL() {
        System.out.println("Testing parseWSDL");
        Iterator it = null;
        File aWSDL = new File("test/com/sun/jbi/filebc/packaging/wsdls/TestFile.wsdl");
        List interfaces = new ArrayList();
        interfaces.add(new EndpointDataImpl("{urn:FileTest}HelloIF", "{urn:FileTest}MyHelloService", "{urn:FileTest}HelloIFPort", 0));
        try {
            List endpoints = instance.parseWSDL(aWSDL, resolver, interfaces, new Vector(), new HashMap());
            assertEquals(endpoints.size(), 1);
            assertTrue(endpoints.get(0) instanceof Endpoint);
            Endpoint ep = (Endpoint) endpoints.get(0);
            assertEquals("{urn:FileTest}HelloIFPort", ep.getEndpointName());
            assertEquals("{urn:FileTest}MyHelloService", ep.getServiceName().toString());
            assertEquals(0, ep.getEndpointType());
            assertTrue(ep.getFileAddress() instanceof FileAddress);
            FileAddress address = ep.getFileAddress();
            assertEquals("/tmp/testing", address.getFileDirectory());
            assertTrue(ep.getFileBinding() instanceof FileBinding);
            Map operations = ep.getFileOperations();
            assertEquals(operations.size(), 1);
            it = operations.values().iterator();
            FileOperation operation = (FileOperation) it.next();
            FileInput fileInput = operation.getFileOperationInput();
            FileOutput fileOutput = operation.getFileOperationOutput();
            FileMessage inMessage = fileInput.getFileMessage();
            FileMessage outMessage = fileOutput.getFileMessage();
            assertEquals("FileBCInput.txt", inMessage.getFileName());
            assertEquals(9999, inMessage.getPollingInterval().longValue());
            assertTrue(inMessage.getMultipleRecordsPerFile());
            assertTrue(inMessage.getFileNameIsPattern());
            assertEquals("FileBCOutput.txt", outMessage.getFileName());
            assertFalse(outMessage.getFileNameIsPattern());
            assertFalse(outMessage.getMultipleRecordsPerFile());
            Map meps = ep.getOperationMsgExchangePattern();
            assertEquals(meps.size(), 1);
            assertEquals("inout", (String) meps.get(new QName("urn:FileTest", "sayHello")));
        } catch (Exception e) {
            e.printStackTrace();
            fail("Failed testing parseWSDL due to exception: " + e.getMessage());
        }
        System.out.println("Successfully tested parseWSDL");
    }

    /**
     * Test of listWSDLFiles method, of class com.sun.jbi.filebc.packaging.WSDLConfigurations.
     */
    public void testListWSDLFiles() {
        System.out.println("Testing listWSDLFiles");
        File currentDir = new File("test/com/sun/jbi/filebc/packaging/wsdls");
        List expResult = new ArrayList();
        expResult.add(new File("test/com/sun/jbi/filebc/packaging/wsdls/TestTrigger.wsdl"));
        expResult.add(new File("test/com/sun/jbi/filebc/packaging/wsdls/FileInTest/TestFileIn.wsdl"));
        expResult.add(new File("test/com/sun/jbi/filebc/packaging/wsdls/TestFile.wsdl"));
        List result = instance.listResourceFiles(currentDir, ".wsdl");
        assertEquals(expResult.size(), result.size());
        for (int ii = 0; ii < result.size(); ii++) {
            File aWSDL = (File) result.get(ii);
            assertTrue(aWSDL.getPath().endsWith(".wsdl"));
        }
        System.out.println("Successfully tested listWSDLFiles");
    }

    /**
     * Test of getBinding method, of class com.sun.jbi.filebc.packaging.WSDLConfigurations.
     */
    public void testGetBinding() {
        System.out.println("Testing getBinding");
        String serviceName = "{urn:FileTest}MyHelloService";
        String endpointName = "{urn:FileTest}HelloIFPort";
        Binding result = instance.getBinding(definition, serviceName, endpointName);
        assertNotNull(result);
        assertTrue(result.getQName().toString().equals("{urn:FileTest}HelloIFBinding"));
        assertTrue(result.getPortType().getQName().toString().equals("{urn:FileTest}HelloIF"));
        System.out.println("Successfully tested getBinding");
    }

    /**
     * Test of determineMEP method, of class com.sun.jbi.filebc.packaging.WSDLConfigurations.
     */
    public void testDetermineMEP() {
//        System.out.println( "Testing determineMEP" );
//        String expResult = "";
//        String result = "";
//        // testing inbound operations
//        int direction = INBOUND;
//        // 1. test inbound request response type operations
//        operation.setStyle( OperationType.REQUEST_RESPONSE );
//        bo.setOperation( operation );
//        expResult = "inout";
//        result = instance.determineMEP( direction, bo );
//        assertEquals( expResult, result );
//        // 2. test inbound solicit response type operations
//        operation.setStyle( OperationType.SOLICIT_RESPONSE );
//        result = instance.determineMEP( direction, bo );
//        assertEquals( "unsupported", result );
//        // 3. test inbound one way type operations
//        expResult = "inonly";
//        operation.setStyle( OperationType.ONE_WAY );
//        result = instance.determineMEP( direction, bo );
//        assertEquals( expResult, result );
//        // 4. test inbound notification type operations
//        operation.setStyle( OperationType.NOTIFICATION );
//        result = instance.determineMEP( direction, bo );
//        assertEquals( "unsupported", result );
//        // testing outbound operations
//        direction = OUTBOUND;
//        // 5. test outbound request-response type operations
//        operation.setStyle( OperationType.REQUEST_RESPONSE );
//        expResult = "outin";
//        result = instance.determineMEP( direction, bo );
//        assertEquals( expResult, result );
//        // 6. test outbound solicit response type operations
//        operation.setStyle( OperationType.SOLICIT_RESPONSE );
//        result = instance.determineMEP( direction, bo );
//        assertEquals( expResult, result );
//        // 7. test outbound one way type operations
//        operation.setStyle( OperationType.ONE_WAY );
//        expResult = "outonly";
//        result = instance.determineMEP( direction, bo );
//        assertEquals( expResult, result );
//        // 8. test outbound notification type operations
//        operation.setStyle( OperationType.NOTIFICATION );
//        result = instance.determineMEP( direction, bo );
//        assertEquals( expResult, result );
//        System.out.println( "Successfully tested determineMEP" );
    }
}
