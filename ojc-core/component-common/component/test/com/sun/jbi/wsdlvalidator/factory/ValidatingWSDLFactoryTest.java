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
 * @(#)ValidatingWSDLFactoryTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.wsdlvalidator.factory;

import junit.framework.*;
import java.io.File;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;
import javax.wsdl.xml.WSDLWriter;
import org.apache.xml.resolver.tools.CatalogResolver;
import org.apache.xml.resolver.CatalogManager;
import org.xml.sax.EntityResolver;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.jbi.wsdlvalidator.impl.ValidatingWSDLReaderImpl;

/**
 *
 * @author afung
 */
public class ValidatingWSDLFactoryTest extends TestCase {
    
    public ValidatingWSDLFactoryTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ValidatingWSDLFactoryTest.class);
        
        return suite;
    }

    /**
     * Test of newDefinition method, of class com.sun.jbi.wsdlvalidator.factory.ValidatingWSDLFactory.
     */
    public void testNewDefinition() {
        ValidatingWSDLFactory instance = new ValidatingWSDLFactory();
        Definition result = instance.newDefinition();
        assertNotNull(result);
    }

    /**
     * Test of newWSDLReader method, of class com.sun.jbi.wsdlvalidator.factory.ValidatingWSDLFactory.
     */
    public void testNewWSDLReader() {
        ValidatingWSDLFactory instance = new ValidatingWSDLFactory();
        WSDLReader result = instance.newWSDLReader();
        assertNotNull(result);
        assertEquals(ValidatingWSDLReaderImpl.class, result.getClass());
    }

    /**
     * Test of newWSDLReader method, of class com.sun.jbi.wsdlvalidator.factory.ValidatingWSDLFactory.
     */
    public void testNewWSDLReaderWithFileArgument() {
        ValidatingWSDLFactory instance = new ValidatingWSDLFactory();
        File emptyDataDir = new File("test/com/sun/jbi/wsdlvalidator/factory/emptyDataDir");
        WSDLReader result = instance.newWSDLReader(emptyDataDir);
        assertNotNull(result);
        assertEquals(ValidatingWSDLReaderImpl.class, result.getClass());
        
        File catalogDir = new File("test/com/sun/jbi/wsdlvalidator/factory/data");
        result = instance.newWSDLReader(catalogDir);
        assertNotNull(result);
        assertEquals(ValidatingWSDLReaderImpl.class, result.getClass());
    }
    
    /**
     * Test of newWSDLWriter method, of class com.sun.jbi.wsdlvalidator.factory.ValidatingWSDLFactory.
     */
    public void testNewWSDLWriter() {     
        ValidatingWSDLFactory instance = new ValidatingWSDLFactory();
        WSDLWriter result = instance.newWSDLWriter();
        assertNotNull(result);
    }

    /**
     * Test of newPopulatedExtensionRegistry method, of class com.sun.jbi.wsdlvalidator.factory.ValidatingWSDLFactory.
     */
    public void testNewPopulatedExtensionRegistry() {      
        ValidatingWSDLFactory instance = new ValidatingWSDLFactory();
        ExtensionRegistry result = instance.newPopulatedExtensionRegistry();
        assertNotNull(result);
    }
    
}
