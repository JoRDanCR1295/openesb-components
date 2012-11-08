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
 * @(#)WSDLConfigurationsTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.packaging;


import junit.framework.*;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.ibm.wsdl.BindingOperationImpl;
import com.ibm.wsdl.OperationImpl;
import com.sun.jbi.execbc.Endpoint;
import com.sun.jbi.execbc.packaging.EndpointDataImpl;
import com.sun.jbi.execbc.EndpointImpl;
import com.sun.jbi.execbc.extensions.ExecAddress;
import com.sun.jbi.execbc.extensions.ExecBinding;
import com.sun.jbi.execbc.extensions.ExecInput;
import com.sun.jbi.execbc.extensions.ExecMessage;
import com.sun.jbi.execbc.extensions.ExecOperation;
import com.sun.jbi.execbc.extensions.ExecOutput;
import com.sun.jbi.execbc.packaging.WSDLConfigurations;
import com.sun.jbi.execbc.packaging.WSDLConfigurationsTest;
import com.sun.jbi.execbc.extensions.ExecExtensionRegistry;
import com.sun.jbi.execbc.packaging.EndpointData;
import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl4j.ext.WSDL4JExt;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.wsdl.Binding;
import javax.wsdl.BindingInput;
import javax.wsdl.BindingOperation;
import javax.wsdl.BindingOutput;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.Operation;
import javax.wsdl.OperationType;
import javax.wsdl.Port;
import javax.wsdl.Service;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.xml.sax.EntityResolver;
import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.w3c.dom.Document;


/**
 * * *
 * @author sweng
 */
public class WSDLConfigurationsTest extends TestCase
{

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

    public WSDLConfigurationsTest( String testName )
    {
        super( testName );
    }

    protected void setUp()
        throws Exception
    {
        instance = new WSDLConfigurations( "testDir");
        bo = new BindingOperationImpl();
        operation = new OperationImpl();
        resolver = new CatalogResolver( new CatalogManager() );
        reader = WSDL4JExt.newWSDLReader( resolver );
        definition = reader.readWSDL( (new File( "test/com/sun/jbi/execbc/packaging/wsdls/TestExec.wsdl" )).getAbsolutePath() );
    }

    protected void tearDown()
        throws Exception
    {}

    public static Test suite()
    {
        TestSuite suite = new TestSuite( WSDLConfigurationsTest.class );
        return suite;
    }

    /**
     * Test of parseWSDL method, of class com.sun.jbi.execbc.packaging.WSDLConfigurations
     */
    public void testParseWSDL()
    {
        System.out.println( "Testing parseWSDL" );
        Iterator it = null;
        File aWSDL = new File( "test/com/sun/jbi/execbc/packaging/wsdls/TestExec.wsdl" );
        List interfaces = new ArrayList();
        interfaces.add( new EndpointDataImpl( "{urn:ExecTest}HelloIF", "{urn:ExecTest}MyHelloService", "{urn:ExecTest}HelloIFPort", 1 ) );
        try {
            List endpoints = instance.parseWSDL( aWSDL, resolver, interfaces, new HashMap() );
            assertEquals( endpoints.size(), 1 );
            assertTrue( endpoints.get( 0 ) instanceof Endpoint );
            Endpoint ep = (Endpoint) endpoints.get( 0 );
            assertEquals( "{urn:ExecTest}HelloIFPort", ep.getEndpointName() );
            assertEquals( "{urn:ExecTest}MyHelloService", ep.getServiceName().toString() );
            assertEquals( 1, ep.getEndpointType() );
            assertTrue( ep.getExecAddress() instanceof ExecAddress );
            ExecAddress address = ep.getExecAddress();
            assertTrue( ep.getExecBinding() instanceof ExecBinding );
            Map operations = ep.getExecOperations();
            assertEquals( operations.size(), 1 );
            it = operations.values().iterator();
            ExecOperation operation = (ExecOperation) it.next();
            ExecInput execInput = operation.getExecOperationInput();
            ExecOutput execOutput = operation.getExecOperationOutput();
            ExecMessage inMessage = execInput.getExecMessage();
            ExecMessage outMessage = execOutput.getExecMessage();
            assertEquals( 9999, operation.getPollingInterval() );
            assertEquals( "encoded", outMessage.getExecUseType() );
            assertEquals( "customencoder-1.0", outMessage.getExecEncodingStyle() );
            Map meps = ep.getOperationMsgExchangePattern();
            assertEquals( meps.size(), 1 );
            assertEquals( "outin", (String) meps.get(new QName("urn:ExecTest", "sayHello" ) ) );
        } catch ( Exception e ) {
            e.printStackTrace();
            fail( "Failed testing parseWSDL due to exception: " + e.getMessage() );
        }
        System.out.println( "Successfully tested parseWSDL" );
    }

    /**
     * Test of listWSDLFiles method, of class com.sun.jbi.execbc.packaging.WSDLConfigurations.
     */
    public void testListWSDLFiles()
    {
        System.out.println( "Testing listWSDLFiles" );
        File currentDir = new File( "test/com/sun/jbi/execbc/packaging/wsdls" );
        List expResult = new ArrayList();
        expResult.add( new File( "test/com/sun/jbi/execbc/packaging/wsdls/TestTrigger.wsdl" ) );
        expResult.add( new File( "test/com/sun/jbi/execbc/packaging/wsdls/execReceiveTest/TestExecReceive.wsdl" ) );
        expResult.add( new File( "test/com/sun/jbi/execbc/packaging/wsdls/TestReceive.wsdl" ) );
        List result = instance.listResourceFiles(currentDir, ".wsdl");
        assertEquals( expResult.size(), result.size() );
        for (int ii = 0; ii < result.size(); ii++) {
            File aWSDL = (File) result.get( ii );
            assertTrue( aWSDL.getPath().endsWith( ".wsdl" ) );
        }
        System.out.println( "Successfully tested listWSDLFiles" );
    }

    /**
     * Test of getBinding method, of class com.sun.jbi.execbc.packaging.WSDLConfigurations.
     */
    public void testGetBinding()
    {
        System.out.println( "Testing getBinding" );
        String serviceName = "{urn:ExecTest}MyHelloService";
        String endpointName = "{urn:ExecTest}HelloIFPort";
        Binding result = instance.getBinding( definition, serviceName, endpointName );
        assertNotNull( result );
        assertTrue( result.getQName().toString().equals( "{urn:ExecTest}HelloIFBinding" ) );
        assertTrue( result.getPortType().getQName().toString().equals( "{urn:ExecTest}HelloIF" ) );
        System.out.println( "Successfully tested getBinding" );
    }
    /**
     * Test of determineMEP method, of class com.sun.jbi.execbc.packaging.WSDLConfigurations.
     */
    public void testDetermineMEP()
    {
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
