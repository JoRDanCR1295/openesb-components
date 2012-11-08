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
 * @(#)SAPWSDLUtilitiesTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.utils;


import junit.framework.*;
import com.ibm.wsdl.BindingOperationImpl;
import com.ibm.wsdl.OperationImpl;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import com.sun.wsdl.model.WSDLDefinitions;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
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
public class SAPWSDLUtilitiesTest extends TestCase {
    static final int INBOUND = 0;
    static final int OUTBOUND = 1;
    SAPWSDLUtilities instance;
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
    
    
    public SAPWSDLUtilitiesTest( String testName ) {
        super( testName );
    }

    protected void setUp()
        throws Exception
    {
        instance = new SAPWSDLUtilities();
        /*
        bo = new BindingOperationImpl();
        operation = new OperationImpl();
        resolver = new CatalogResolver( new CatalogManager() );
        definition = instance.readWSDL( THE_WSDL);
         */
    }

    protected void tearDown()
        throws Exception
    {}

    public static Test suite()
    {
        TestSuite suite = new TestSuite( SAPWSDLUtilitiesTest.class );
        return suite;
    }

    /**
     * Test of listWSDLFiles method, of class com.sun.jbi.sapbc.utils.SAPWSDLUtilities.
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


}
