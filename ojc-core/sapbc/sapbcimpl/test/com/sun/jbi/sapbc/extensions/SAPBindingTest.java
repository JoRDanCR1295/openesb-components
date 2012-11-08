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
 * @(#)SAPBindingTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.sapbc.extensions;

import javax.xml.namespace.QName;

import com.ibm.wsdl.Constants;
import com.sun.jbi.sapbc.Endpoint;
import com.sun.jbi.sapbc.Endpoint.EndpointType;
import com.sun.jbi.sapbc.EndpointImpl;
import com.sun.jbi.sapbc.util.SAPWSDLUtilities;
import java.io.File;
import java.util.HashMap;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 *
 * @author sweng
 */
public class SAPBindingTest extends TestCase {
    SAPBinding instance = null;
    String targetNamespace;
    QName THE_OPERATION = null;
    QName THE_SERVICE = null;
    String THE_ENDPOINT = null;
    
    public SAPBindingTest(String testName) {
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
        instance = endpoint.getSAPBinding();
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(SAPBindingTest.class);
        
        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.sapbc.extensions.SAPBinding.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");
        
        // 1. testing default element type value
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/sap/", Constants.ELEM_BINDING);
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
        // 2. testing setElementType
        QName val = new QName("http://my-sap-binding/test", Constants.ELEM_BINDING);
        expResult = new QName("http://my-sap-binding/test", Constants.ELEM_BINDING);
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setElementType and getElementType");
    }
    
    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.sapbc.extensions.SAPBinding.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");
        
        Boolean expResult = Boolean.FALSE;
        instance.setRequired(Boolean.FALSE);
        Boolean result = instance.getRequired();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setRequired and getRequired");
    }

    /**
     * Test of toString method, of class com.sun.jbi.sapbc.extensions.SAPBinding.
     */
    public void testToString() {
        System.out.println("Testing toString");
        
        String expResult = "SAPBinding {http://my-sap-binding/test}binding:" +
                "\nRequired: false"+
                "\ntransactionalMode: Transactional"+
                "\ntransactionIDVerificationDatabase: "+
                "\nmaxTIDDatabaseRows: 0";
        
        instance.setElementType(new QName("http://my-sap-binding/test", Constants.ELEM_BINDING));
        instance.setRequired(Boolean.FALSE);
        
        String result = instance.toString();
        assertTrue(result.equals(expResult));
        
        System.out.println("Successfully tested toString");
    }
    
}
