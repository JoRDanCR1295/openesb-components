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

package com.sun.jbi.sapbc.packaging;


import junit.framework.*;
import com.ibm.wsdl.BindingOperationImpl;
import com.ibm.wsdl.OperationImpl;
import com.sun.jbi.sapbc.Endpoint;
import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.jbi.sapbc.extensions.SAPAddress;
import com.sun.jbi.sapbc.extensions.SAPBinding;
import com.sun.jbi.sapbc.extensions.SAPEnvironmentalVars;
import com.sun.jbi.sapbc.extensions.SAPFmOperation;
import com.sun.jbi.sapbc.extensions.WSDLInput;
import com.sun.jbi.sapbc.extensions.WSDLOutput;
import com.sun.jbi.sapbc.extensions.SAPMessage;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import com.sun.wsdl.model.Binding;
import com.sun.wsdl.model.WSDLDefinitions;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.wsdl.BindingOperation;
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
    WSDLDefinitions definition;
    WSDLReader reader;
    WSDLFactory wsdlFactory;
    List endpoints = new ArrayList();
    
    final String THE_ROOTPATH = "test/com/sun/jbi/sapbc/wsdls/";
    final File THE_WSDL = new File( THE_ROOTPATH + "Z_FlightWSD_EUC_SAP.wsdl" );
    final String targetNamespace = "urn:sap-com:document:sap:soap:functions:mc-style";
    final QName THE_OPERATION  = new QName("", "FlightGetDetail");
    final QName THE_SERVICE = new QName(targetNamespace, "Z_FlightWSDService");
    final String THE_ENDPOINT = "Z_FlightWSDSAPBindingPort";
    
    
    public WSDLConfigurationsTest( String testName ) {
        super( testName );
    }

    protected void setUp()
        throws Exception
    {
        instance = new WSDLConfigurations( THE_ROOTPATH );
        bo = new BindingOperationImpl();
        operation = new OperationImpl();
        resolver = new CatalogResolver( new CatalogManager() );
        definition = instance.readWSDL( THE_WSDL);
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
     * Test of parseWSDL method, of class com.sun.jbi.sapbc.packaging.WSDLConfigurations
     */
    public void testParseWSDL()
    {
        System.out.println( "Testing parseWSDL" );
        Iterator it = null;
        List interfaces = new ArrayList();
        QName interfaceName = new QName(targetNamespace, "Z_FlightWSD");
        interfaces.add( new EndpointDataImpl( interfaceName, 
                                              THE_SERVICE, 
                                              THE_ENDPOINT, 
                                              EndpointType.INBOUND ) 
                                             );
        try {
            SAPEnvironmentalVars envVars = new SAPEnvironmentalVars(new HashMap());
            instance.setSAPEnvVars(envVars);
            List endpoints = instance.parseWSDL( THE_WSDL, resolver, interfaces );
            assertEquals( endpoints.size(), 1 );
            assertTrue( endpoints.get( 0 ) instanceof Endpoint );
            Endpoint ep = (Endpoint) endpoints.get( 0 );
            assertEquals( THE_ENDPOINT, ep.getEndpointName() );
            assertEquals( THE_SERVICE.toString(), ep.getServiceName().toString() );
            assertEquals( EndpointType.INBOUND, ep.getEndpointType() );
            assertTrue( ep.getSAPAddress() instanceof SAPAddress );
            SAPAddress address = ep.getSAPAddress();
            //assertEquals( "/tmp/testing", address.getFileDirectory() );
            assertTrue( ep.getSAPBinding() instanceof SAPBinding );
            Map operations = ep.getSAPOperations();
            assertEquals( operations.size(), 1 );
            it = operations.values().iterator();
            SAPFmOperation operation = (SAPFmOperation) it.next();
            WSDLInput wsdlInput = operation.getSAPOperationInput();
            WSDLOutput wsdlOutput = operation.getSAPOperationOutput();
            SAPMessage inMessage = wsdlInput.getSAPMessage();
            SAPMessage outMessage = wsdlOutput.getSAPMessage();
            //assertEquals( "SAPBCInput.txt", inMessage.getFileName() );
            //assertEquals( 9999, inMessage.getPollingInterval().longValue() );
            //assertTrue( inMessage.getMultipleRecordsPerFile() );
            //assertTrue( inMessage.getFileNameIsPattern() );
            //assertEquals( "SAPBCOutput.txt", outMessage.getFileName() );
            //assertFalse( outMessage.getFileNameIsPattern() );
            //assertFalse( outMessage.getMultipleRecordsPerFile() );
            Map meps = ep.getOperationMsgExchangePattern();
            assertEquals( meps.size(), 4 );
            assertEquals( "inout", (String) meps.get( QName.valueOf( "FlightGetDetail" ) ) );
        } catch ( Exception e ) {
            e.printStackTrace();
            fail( "Failed testing parseWSDL due to exception: " + e.getMessage() );
        }
        System.out.println( "Successfully tested parseWSDL" );
    }

    /**
     * Test of listWSDLFiles method, of class com.sun.jbi.sapbc.packaging.WSDLConfigurations.
     */
    public void testListWSDLFiles()
    {
        System.out.println( "Testing listWSDLFiles" );
        File currentDir = new File( THE_ROOTPATH );
        List expResult = new ArrayList();
        expResult.add( THE_WSDL );
        //expResult.add( new File( "test/com/sun/jbi/sapbc/packaging/wsdls/SAPInTest/TestSAPIn.wsdl" ) );
        //expResult.add( new File( "test/com/sun/jbi/sapbc/packaging/wsdls/TestSAP.wsdl" ) );
        List result = SAPWSDLUtilities.listResourceFiles(currentDir, ".wsdl");
        assertEquals( expResult.size(), result.size() );
        for (int ii = 0; ii < result.size(); ii++) {
            File aWSDL = (File) result.get( ii );
            assertTrue( aWSDL.getPath().endsWith( ".wsdl" ) );
        }
        System.out.println( "Successfully tested listWSDLFiles" );
    }

    /**
     * Test of findSAPBinding method, of class com.sun.jbi.sapbc.packaging.WSDLConfigurations.
     */
    public void testFindSAPBinding()
    {
        System.out.println( "Testing findSAPBinding" );
        QName endpointQName = new QName(targetNamespace, THE_ENDPOINT);
        final Binding wsdlBinding = SAPWSDLUtilities.findBindingDefinition(definition, 
                                                                           THE_SERVICE, 
                                                                           endpointQName);
        /*
        if (wsdlBinding == null) {
            // This is not a SAP WSDL
            mLogger.warning("Could not find service ["+mServiceQName.toString()+"] and port ["+pm.getEndpoint()+"] in ["+wsdlFile.getAbsolutePath()+"]");
            continue;
        }
         **/
        SAPBinding result = null;
        try {
            SAPEnvironmentalVars envVars = new SAPEnvironmentalVars(new HashMap());
            instance.setSAPEnvVars(envVars);
            result = instance.findSAPBinding( wsdlBinding, THE_SERVICE, THE_ENDPOINT);
        } catch (Exception ex) {
            ex.printStackTrace();
            String errMsg = "testSetGetSAPAddress: Exception Thrown ["+ex.getClass().getName()+"] Message ["+ex.getMessage()+"]";
            fail(errMsg);
        }
        assertNotNull( result );
        //assertTrue( result.getQName().toString().equals( "{urn:SAPTest}HelloIFBinding" ) );
        //assertTrue( result.getPortType().getQName().toString().equals( "{urn:SAPTest}HelloIF" ) );
        System.out.println( "Successfully tested findSAPBinding" );
    }
    /**
     * Test of determineMEP method, of class com.sun.jbi.sapbc.packaging.WSDLConfigurations.
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
