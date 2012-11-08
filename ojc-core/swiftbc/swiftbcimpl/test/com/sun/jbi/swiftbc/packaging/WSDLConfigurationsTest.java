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

package com.sun.jbi.swiftbc.packaging;


import junit.framework.*;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.ibm.wsdl.BindingOperationImpl;
import com.ibm.wsdl.OperationImpl;
import com.sun.jbi.swiftbc.Endpoint;
import com.sun.jbi.swiftbc.extensions.SwiftAddress;
import com.sun.jbi.swiftbc.extensions.SwiftBinding;
import com.sun.jbi.swiftbc.extensions.SwiftInput;
import com.sun.jbi.swiftbc.extensions.SwiftOperation;
import com.sun.jbi.swiftbc.extensions.SwiftMessage;
import com.sun.jbi.swiftbc.extensions.SwiftOutput;
import com.sun.jbi.swiftbc.packaging.EndpointDataImpl;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
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
 * @author Sun Microsystems, Inc.
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
    
    public WSDLConfigurationsTest( String testName ) {
        super( testName );
    }
    
    protected void setUp()
            throws Exception {
        instance = new WSDLConfigurations( "testDir" );
        bo = new BindingOperationImpl();
        operation = new OperationImpl();
        resolver = new CatalogResolver( new CatalogManager() );
        wsdlFactory = WSDLFactory.newInstance();
        reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader( resolver );
        definition = reader.readWSDL( (new File( "test/com/sun/jbi/swiftbc/packaging/wsdls/TestSwift.wsdl" )).getAbsolutePath() );
    }
    
    protected void tearDown()
            throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite( WSDLConfigurationsTest.class );
        return suite;
    }
    
    /**
     * Test of parseWSDL method, of class com.sun.jbi.swiftbc.packaging.WSDLConfigurations
     */
    public void testParseWSDL() {
        /*
        System.out.println( "Testing parseWSDL" );
        Iterator it = null;
        File aWSDL = new File( "test/com/sun/jbi/swiftbc/packaging/wsdls/TestSwift.wsdl" );
        List interfaces = new ArrayList();
        interfaces.add( new EndpointDataImpl( "{http://j2ee.netbeans.org/TestSwiftWsdl}TestSwiftWsdlPortType", "{http://j2ee.netbeans.org/TestSwiftWsdl}service1", "{http://j2ee.netbeans.org/TestSwiftWsdl}port1", 0 ) );
        try {
            List endpoints = instance.parseWSDL( aWSDL, resolver, interfaces, new HashMap() );
            assertEquals( 1, endpoints.size() );
            assertTrue( endpoints.get( 0 ) instanceof Endpoint );
            Endpoint ep = (Endpoint) endpoints.get( 0 );
            assertEquals( "{http://j2ee.netbeans.org/TestSwiftWsdl}port1", ep.getEndpointName() );
            assertEquals( "{http://j2ee.netbeans.org/TestSwiftWsdl}service1", ep.getServiceName().toString() );
            assertEquals( 0, ep.getEndpointType() );
            assertTrue( ep.getSwiftAddress() instanceof SwiftAddress );
            SwiftAddress address = ep.getSwiftAddress();
            assertEquals( "swift://localhost:48002", address.getSwiftServerLocationURL() );
            assertTrue( ep.getSwiftBinding() instanceof SwiftBinding );
            Map operations = ep.getSwiftOperations();
            assertEquals( 1, operations.size() );
            it = operations.values().iterator();
            SwiftOperation operation = (SwiftOperation) it.next();
            SwiftInput swiftInput = operation.getSwiftOperationInput();
           // SwiftOutput swiftOutput = operation.getSwiftOperationOutput();
            SwiftMessage inMessage = swiftInput.getSwiftMessage();
            //SwiftMessage outMessage = swiftOutput.getSwiftMessage();
            assertEquals( "swiftencoder-1.0", inMessage.getEncodingStyle() );
            //assertEquals( "swiftencoder-1.0", outMessage.getEncodingStyle() );
            Map meps = ep.getOperationMsgExchangePattern();
            assertEquals( 1, meps.size() );
            assertEquals( "inonly", (String) meps.get( QName.valueOf( "TestSwiftWsdlOperation" ) ) );
        } catch ( Exception e ) {
            e.printStackTrace();
            fail( "Failed testing parseWSDL due to exception: " + e.getMessage() );
        }
        System.out.println( "Successfully tested parseWSDL" );
        */
    }
    
    
    
    /**
     * Test of getBinding method, of class com.sun.jbi.swiftbc.packaging.WSDLConfigurations.
     */
    public void testGetBinding() {
        /*
        System.out.println( "Testing getBinding" );
        String serviceName = "{http://j2ee.netbeans.org/TestSwiftWsdl}service1";
        String endpointName = "{http://j2ee.netbeans.org/TestSwiftWsdl}port1";
        Binding result = instance.getBinding( definition, serviceName, endpointName );
        assertNotNull( result );
        assertTrue( result.getQName().toString().equals( "{http://j2ee.netbeans.org/TestSwiftWsdl}binding1" ) );
        assertTrue( result.getPortType().getQName().toString().equals( "{http://j2ee.netbeans.org/TestSwiftWsdl}TestSwiftWsdlPortType" ) );
        System.out.println( "Successfully tested getBinding" );
        */
    }
    public static void main(String [] args){
        try     {
            com.sun.jbi.swiftbc.packaging.WSDLConfigurationsTest test = new com.sun.jbi.swiftbc.packaging.WSDLConfigurationsTest("mytest");
            test.setUp();
            test.testParseWSDL();
            test.testGetBinding();
            
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        
    }
}
