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
 * @(#)HL7ProtocolPropertiesTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extensions;

import junit.framework.*;
import java.io.Serializable;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.xml.namespace.QName;

/**
 *
 * @author Raghunadh
 */
public class HL7ProtocolPropertiesTest extends TestCase {
    HL7ProtocolProperties instance = new HL7ProtocolProperties();
    
    public HL7ProtocolPropertiesTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(HL7ProtocolPropertiesTest.class);
        
        return suite;
    }

    /**
     * Test of setElementType and getElementType method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetElementType() {
        System.out.println("Testing setElementType and getElementType");
        
        // 1. testing the default value of element type
        QName expResult = new QName("http://schemas.sun.com/jbi/wsdl-extensions/hl7/", "protocolproperties");
        QName result = instance.getElementType();
        assertEquals(expResult, result);

        // 2. testing setElementType
        QName val = new QName("http://my-hl7-protocolproperties-test", "protocolproperties");
        expResult = new QName("http://my-hl7-protocolproperties-test", "protocolproperties");
        instance.setElementType(val);
        result = instance.getElementType();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setElementType and getElementType");
    }

    /**
     * Test of setRequired and getRequired method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
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
     * Test of setAckMode and getAckMode method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetAckMode() {
        System.out.println("Testing setAckMode and getAckMode");
        
        // 1. testing the default value of Ack Mode
        String expResult = "original";
        String result = instance.getAckMode();
        assertEquals(expResult, result);

        // 2. testing setAckMode
        
        expResult = "originalAck";
        instance.setAckMode("originalAck");
        result = instance.getAckMode();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setAckMode and getAckMode");
    }

	/**
     * Test of setLLPType and getLLPType method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetLLPType() {
        System.out.println("Testing setLLPType and getLLPType");
        
        // 1. testing the default value of Ack Mode
        String expResult = "MLLPv1";
        String result = instance.getLLPType();
        assertEquals(expResult, result);

        // 2. testing setAckMode
        
        expResult = "HLLP";
        instance.setLLPType("HLLP");
        result = instance.getLLPType();
        assertEquals(expResult, result);

        System.out.println("Successfully tested setLLPType and getLLPType");
    }

	/**
     * Test of setEndDataChar and getEndDataChar method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
   public void testSetGetEndDataChar() {
        System.out.println("Testing setEndDataChar and getEndDataChar");
        
        Byte val = new Byte("28");
        instance.setEndDataChar(val);
        Byte result = instance.getEndDataChar();
        assertEquals(val, result);
        
        System.out.println("Successfully tested setEndDataChar and getEndDataChar");
    }

	/**
     * Test of setEndBlockChar and gsetEndBlockChar method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
   public void testSetGetEndBlockChar() {
        System.out.println("Testing setEndBlockChar and getEndBlockChar");
        
        Byte val = new Byte("13");
        instance.setEndBlockChar(val);
        Byte result = instance.getEndBlockChar();
        assertEquals(val, result);
        
        System.out.println("Successfully tested setEndBlockChar and getEndBlockChar");
    }

	/**
     * Test of setStartBlockChar and getStartBlockChar method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
   public void testSetGetStartBlockChar() {
        System.out.println("Testing setStartBlockChar and getStartBlockChar");
        
        Byte val = new Byte("11");
        instance.setStartBlockChar(val);
        Byte result = instance.getStartBlockChar();
        assertEquals(val, result);
        
        System.out.println("Successfully tested setStartBlockChar and getStartBlockChar");
    }

	 /**
     * Test of setHLLPChkSumEnabled and getHLLPChkSumEnabled method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetHLLPChkSumEnabled() {
        System.out.println("Testing setHLLPChkSumEnabled and getHLLPChkSumEnabled");
        
        Boolean val = Boolean.FALSE;
        instance.setHLLPChkSumEnabled(val);
        Boolean result = instance.getHLLPChkSumEnabled();
        assertEquals(val, result);
        
        System.out.println("Successfully tested setHLLPChkSumEnabled and getHLLPChkSumEnabled");
    }

	 /**
     * Test of setSeqNumEnabled and getSeqNumEnabled method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetSeqNumEnabled() {
        System.out.println("Testing setSeqNumEnabled and getSeqNumEnabled");
        
        Boolean val = Boolean.FALSE;
        instance.setSeqNumEnabled(val);
        Boolean result = instance.getSeqNumEnabled();
        assertEquals(val, result);
        
        System.out.println("Successfully tested setSeqNumEnabled and getSeqNumEnabled");
    }

	 /**
     * Test of setSeqNumEnabled and getSeqNumEnabled method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetValidateMSHEnabled() {
        System.out.println("Testing setValidateMSH and getValidateMSH");
        
        Boolean val = Boolean.FALSE;
        instance.setValidateMSHEnabled(val);
        Boolean result = instance.getValidateMSHEnabled();
        assertEquals(val, result);
        
        System.out.println("Successfully tested setValidateMSH and getValidateMSH");
    }

	/**
     * Test of getVersionID and setVersionID method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetVersionID() {
        System.out.println("Testing setLLPType and getLLPType");
        
       
        String expResult = "2.1";
        instance.setVersionID(expResult);
		String result = instance.getVersionID();
        assertEquals(expResult, result);
       
    }

	/**
     * Test of getProcessingID and setProcessingID method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetProcessingID() {
        System.out.println("Testing setLLPType and getLLPType");
        
       
        String expResult = "D";
        instance.setProcessingID(expResult);
		String result = instance.getProcessingID();
        assertEquals(expResult, result);
       
    }

	 /**
     * Test of setSFTEnabled and getSFTEnabled method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetSFTEnabled() {
        System.out.println("Testing setSFTEnabled and getSFTEnabled");
        
        Boolean val = Boolean.FALSE;
        instance.setSFTEnabled(val);
        Boolean result = instance.getSFTEnabled();
        assertEquals(val, result);
        
        System.out.println("Successfully tested setSFTEnabled and getSFTEnabled");
    }

	/**
     * Test of setSoftwareVendorOrganization and getSoftwareVendorOrganization method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetSoftwareVendorOrganization() {
        System.out.println("Testing setSoftwareVendorOrganization and getSoftwareVendorOrganization");
        
       
        String expResult = "Sun Microsystems, Inc.";
        instance.setSoftwareVendorOrganization(expResult);
		String result = instance.getSoftwareVendorOrganization();
        assertEquals(expResult, result);
       
    }

	/**
     * Test of setSoftwareCertifiedVersionOrReleaseNumber and getSoftwareCertifiedVersionOrReleaseNumber method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetSoftwareCertifiedVersionOrReleaseNumber() {
        System.out.println("Testing setSoftwareCertifiedVersionOrReleaseNumber and getSoftwareCertifiedVersionOrReleaseNumber");
        
       
        String expResult = "6.0";
        instance.setSoftwareCertifiedVersionOrReleaseNumber(expResult);
		String result = instance.getSoftwareCertifiedVersionOrReleaseNumber();
        assertEquals(expResult, result);
       
    }

	/**
     * Test of setSoftwareProductName and getSoftwareProductName method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetSoftwareProductName() {
        System.out.println("Testing setSoftwareProductName and getSoftwareProductName");
        
        
        String expResult = "Sun HL7 BC";
        instance.setSoftwareProductName(expResult);
		String result = instance.getSoftwareProductName();
        assertEquals(expResult, result);
       
    }

	/**
     * Test of setSoftwareBinaryID and getSoftwareBinaryID method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetSoftwareBinaryID() {
        System.out.println("Testing setSoftwareBinaryID and getSoftwareBinaryID");
        
       
        String expResult = "6.0";
        instance.setSoftwareBinaryID(expResult);
		String result = instance.getSoftwareBinaryID();
        assertEquals(expResult, result);
       
    }

	/**
     * Test of setSoftwareProductInformation and getSoftwareProductInformation method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetSoftwareProductInformation() {
        System.out.println("Testing setSoftwareProductInformation and getSoftwareProductInformation");
        
       
        String expResult = "It is a HL7 Binding component";
        instance.setSoftwareProductInformation(expResult);
		String result = instance.getSoftwareProductInformation();
        assertEquals(expResult, result);
       
    }

	/**
     * Test of setSoftwareInstallDate and setSoftwareInstallDate method, of class com.sun.jbi.hl7bc.extensions.HL7ProtocolProperties.
     */
    public void testSetGetSoftwareInstallDate() {
        System.out.println("Testing setSoftwareInstallDate and setSoftwareInstallDate");
        
       
        String expResult = "011220062556";
        instance.setSoftwareInstallDate(expResult);
		String result = instance.getSoftwareInstallDate();
        assertEquals(expResult, result);
       
    }







       
}
