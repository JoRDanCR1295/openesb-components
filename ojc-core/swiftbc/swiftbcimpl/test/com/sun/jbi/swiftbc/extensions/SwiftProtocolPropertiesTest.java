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
 * @(#)SwiftProtocolPropertiesTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.swiftbc.extensions;

import junit.framework.*;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author Raghunadh
 */
public class SwiftProtocolPropertiesTest extends TestCase {
    SwiftProtocolProperties instance = new SwiftProtocolProperties();
    
    public SwiftProtocolPropertiesTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
    }
    
    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(SwiftProtocolPropertiesTest.class);
        
        return suite;
    }
    
    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");
        
        // 1. testing the default value of element type
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/swift/", "protocolproperties");
        QName result = instance.getElementType();
        assertEquals(expResult, result);
        
        // 2. testing setElementType
        QName val = new QName("http://my-Swift-protocolproperties-test", "protocolproperties");
        expResult = new QName("http://my-Swift-protocolproperties-test", "protocolproperties");
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);
        
        System.out.println("Successfully tested setElementType and getElementType");
    }
    
    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties.
     */
    public void testSetGetRequired() {
        System.out.println("Testing setRequired and getRequired");
        
        Boolean required = Boolean.TRUE;
        instance.setRequired(required);
        Boolean result = instance.getRequired();
        assertEquals(required, result);
        
        System.out.println("Successfully tested setRequired and getRequired");
    }
        
    /**
     * Test of getVersionID and setVersionID method, of class com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties.
     */
    public void testSetGetVersionID() {
        System.out.println("Testing setLLPType and getLLPType");
        
        
        String expResult = "2.1";
        instance.setVersionID(expResult);
        String result = instance.getVersionID();
        assertEquals(expResult, result);
        
    }
        
    /**
     * Test of setSoftwareVendorOrganization and getSoftwareVendorOrganization method, of class com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties.
     */
    public void testSetGetSoftwareVendorOrganization() {
        System.out.println("Testing setSoftwareVendorOrganization and getSoftwareVendorOrganization");
        
        
        String expResult = "Sun Microsystems, Inc.";
        instance.setSoftwareVendorOrganization(expResult);
        String result = instance.getSoftwareVendorOrganization();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSoftwareCertifiedVersionOrReleaseNumber and getSoftwareCertifiedVersionOrReleaseNumber method, of class com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties.
     */
    public void testSetGetSoftwareCertifiedVersionOrReleaseNumber() {
        System.out.println("Testing setSoftwareCertifiedVersionOrReleaseNumber and getSoftwareCertifiedVersionOrReleaseNumber");
        
        
        String expResult = "6.0";
        instance.setSoftwareCertifiedVersionOrReleaseNumber(expResult);
        String result = instance.getSoftwareCertifiedVersionOrReleaseNumber();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSoftwareProductName and getSoftwareProductName method, of class com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties.
     */
    public void testSetGetSoftwareProductName() {
        System.out.println("Testing setSoftwareProductName and getSoftwareProductName");
        
        
        String expResult = "Sun Swift BC";
        instance.setSoftwareProductName(expResult);
        String result = instance.getSoftwareProductName();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSoftwareBinaryID and getSoftwareBinaryID method, of class com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties.
     */
    public void testSetGetSoftwareBinaryID() {
        System.out.println("Testing setSoftwareBinaryID and getSoftwareBinaryID");
        
        
        String expResult = "6.0";
        instance.setSoftwareBinaryID(expResult);
        String result = instance.getSoftwareBinaryID();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSoftwareProductInformation and getSoftwareProductInformation method, of class com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties.
     */
    public void testSetGetSoftwareProductInformation() {
        System.out.println("Testing setSoftwareProductInformation and getSoftwareProductInformation");
        
        
        String expResult = "It is a Swift Binding component";
        instance.setSoftwareProductInformation(expResult);
        String result = instance.getSoftwareProductInformation();
        assertEquals(expResult, result);
        
    }
    
    /**
     * Test of setSoftwareInstallDate and setSoftwareInstallDate method, of class com.sun.jbi.swiftbc.extensions.SwiftProtocolProperties.
     */
    public void testSetGetSoftwareInstallDate() {
        System.out.println("Testing setSoftwareInstallDate and setSoftwareInstallDate");
        
        
        String expResult = "011220062556";
        instance.setSoftwareInstallDate(expResult);
        String result = instance.getSoftwareInstallDate();
        assertEquals(expResult, result);
        
    }
    
    
    
    
    
    
    
    
}
