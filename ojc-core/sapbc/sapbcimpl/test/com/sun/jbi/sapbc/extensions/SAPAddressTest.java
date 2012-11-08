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
 * @(#)SAPAddressTest.java 
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
public class SAPAddressTest extends TestCase {
    String targetNamespace;
    QName THE_OPERATION = null;
    QName THE_SERVICE = null;
    String THE_ENDPOINT = null;
    SAPAddress instance = null;
    
    public SAPAddressTest(String testName) {
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
        instance = endpoint.getSAPAddress();
    }
    
    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(SAPAddressTest.class);
        
        return suite;
    }
    
    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.sapbc.extensions.SAPAddress.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");
        
        // 1. testing the default value of element type
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/sap/", "address");
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
        // 2. testing setElementType
        QName val = new QName("http://my-sap-address-test", "address");
        expResult = new QName("http://my-sap-address-test", "address");
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setElementType and getElementType");
    }
    
    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.sapbc.extensions.SAPAddress.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");
        
        Boolean val = Boolean.TRUE;
        Boolean expResult = Boolean.TRUE;
        instance.setRequired(val);
        Boolean result = instance.getRequired();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setRequired and getRequired");
    }
    
    /**
     * Test of toString method, of class com.sun.jbi.sapbc.extensions.SAPAddress.
     */
    public void testToString() {
        System.out.println("Testing toString");
        
        String expResult = "SAPAddress {http://my-sap-address/test}address:" +
                "\nRequired: false"+
                "\napplicationServerHostname: sap50uni"+
                "\nclientNumber: 800"+
                "\nenableABAPDebugWindow: false"+
                "\ngatewayHostname: sap50uni"+
                "\ngatewayService: sapgw00"+
                "\nisSAPSystemUnicode: true"+
                "\nlanguage: EN"+
                "\npassword: (length 7)"+
                "\nrouterString: "+
                "\nsystemID: EUC"+
                "\nsystemNumber: 00"+
                "\nuser: PS1";
        
        instance.setElementType(new QName("http://my-sap-address/test", "address"));
        instance.setRequired(Boolean.FALSE);
        
        String result = instance.toString();
        assertTrue(result.equals(expResult));
        
        System.out.println("Successfully tested toString");
    }
}
