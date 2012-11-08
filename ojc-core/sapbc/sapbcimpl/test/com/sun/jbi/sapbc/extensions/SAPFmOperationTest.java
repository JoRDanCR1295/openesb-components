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
 * @(#)SAPFmOperationTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import com.sun.jbi.sapbc.Endpoint;
import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.jbi.sapbc.EndpointImpl;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import java.io.File;
import java.util.HashMap;
import junit.framework.*;
import javax.xml.namespace.QName;

/**
 *
 * @author sweng
 */
public class SAPFmOperationTest extends TestCase {
    SAPFmOperation instance = null;
    String targetNamespace;
    QName THE_OPERATION = null;
    QName THE_SERVICE = null;
    String THE_ENDPOINT = null;
    
    public SAPFmOperationTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
        targetNamespace = "urn:sap-com:document:sap:soap:functions:mc-style";
        THE_OPERATION  = new QName("", "FlightGetDetail");
        THE_SERVICE = new QName(targetNamespace, "Z_FlightWSDService");
        THE_ENDPOINT = "Z_FlightWSDSAPBindingPort";
        
        File wsdlFile = new File( "test/com/sun/jbi/sapbc/wsdls/Z_FlightWSD_EUC_SAP.wsdl" );
        QName interfaceQName = new QName(targetNamespace, "Z_FlightWSD");
        EndpointType direction = EndpointType.INBOUND;
        SAPEnvironmentalVars envVars = new SAPEnvironmentalVars(new HashMap());
        Endpoint endpoint = (EndpointImpl) SAPWSDLUtilities.getWSDLEndpointByName(wsdlFile, 
                interfaceQName, 
                THE_SERVICE, 
                THE_ENDPOINT, 
                direction,
                envVars); 
        Object op = endpoint.getSAPOperations().get(THE_OPERATION);
        if (op instanceof SAPFmOperation) {
            instance = (SAPFmOperation) op;
        } else {
            fail("Operation ["+THE_OPERATION.toString()+"] is not an instance of SAPFmOperation");
        }
        
     }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(SAPFmOperationTest.class);
        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.sapbc.extensions.SAPFmOperation.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");
        
        // 1. testing default element type value
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/sap/", SAPWsdlConstants.FM_OPERATION);
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
        // 2. testing setElementType
        QName val = new QName("http://sap-operation-test/", SAPWsdlConstants.FM_OPERATION); 
        expResult = new QName("http://sap-operation-test/", SAPWsdlConstants.FM_OPERATION);
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setElementType and getElementType");
    }

    /**
     * Test of setRequired and setgetRequired method, of class com.sun.jbi.sapbc.extensions.SAPFmOperation.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");
        
        Boolean expResult = Boolean.TRUE;
        instance.setRequired(Boolean.TRUE);
        Boolean result = instance.getRequired();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setRequired and getRequired");
    }

    /**
     * Test of toString method, of class com.sun.jbi.sapbc.extensions.SAPAddress.
     */
    public void testToString() {
        System.out.println("Testing toString");
        
        String expResult = "SAPFmOperation {http://my-sap-operation/test}fmoperation:" +
                "\nRequired: false"+
                "\nfunctionName: BAPI_FLIGHT_GETDETAIL";
        
        instance.setElementType(new QName("http://my-sap-operation/test", SAPWsdlConstants.FM_OPERATION));
        instance.setRequired(Boolean.FALSE);
        
        String result = instance.toString();
        assertTrue(result.equals(expResult));
        
        System.out.println("Successfully tested toString");
    }    
}
