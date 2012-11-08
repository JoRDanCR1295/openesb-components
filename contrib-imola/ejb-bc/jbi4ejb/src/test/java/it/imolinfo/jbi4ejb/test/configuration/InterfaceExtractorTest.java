/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.test.configuration;

import it.imolinfo.jbi4ejb.configuration.InterfaceExtractorUtil;
import it.imolinfo.jbi4ejb.exception.EJBWSDLGenerationException;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

/**
 * Tests the interface extraction.
 * @author marco
 *
 */
public class InterfaceExtractorTest extends TestCase {
    
    // Where the test EAR (wsdl and jbi.xml) are
    public static final String EAR_TEST_DIR = "src/test/etc/ear";
    
    public static final String EAR_TEST_NAME = "test-ear-0.1-SNAPSHOT.ear";
    
    /**
     * Tests the ejb remote interface extractor
     */
    public void testInterfaceExtraction() {        
        
        String earPath = EAR_TEST_DIR + File.separator + EAR_TEST_NAME;
        List<String> interfaces;
        try {
            interfaces = InterfaceExtractorUtil.extractRemoteInterfacesFromEar(earPath);
            assertEquals(2, interfaces.size());
            for (String interfaceName:interfaces) {
                System.out.println("Remote interface found: " + interfaceName);            
            }
            List<String> assertList = new ArrayList<String>();
            assertList.add("it.imolinfo.test14.TestSessionRemote");
            assertList.add("it.imolinfo.test14.complex.TestComplexSessionRemote");            
            assertEquals(assertList, interfaces);            
            
        } catch (EJBWSDLGenerationException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }

    }      
    

}
