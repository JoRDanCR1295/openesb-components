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
 * @(#)ValidatingWSDLReaderImplTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.wsdlvalidator.impl;

import junit.framework.*;
import java.io.File;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import org.w3c.dom.Element;
import org.xml.sax.EntityResolver;
import com.ibm.wsdl.util.xml.QNameUtils;
import com.ibm.wsdl.xml.WSDLReaderImpl;
import com.sun.jbi.wsdlvalidator.ValidatingWSDLReader;
import com.sun.jbi.wsdlvalidator.Validator;
import com.sun.jbi.wsdlvalidator.ValidatorRegistry;

/**
 *
 * @author afung
 */
public class ValidatingWSDLReaderImplTest extends TestCase {
    
    public ValidatingWSDLReaderImplTest(String testName) {
        super(testName);
    }

    protected void setUp() throws Exception {
    }

    protected void tearDown() throws Exception {
    }

    public static Test suite() {
        TestSuite suite = new TestSuite(ValidatingWSDLReaderImplTest.class);
        
        return suite;
    }

    /**
     * Test of setValidatorRegistry method, of class com.sun.jbi.wsdlvalidator.impl.ValidatingWSDLReaderImpl.
     */
    public void testGetSetValidatorRegistry() {
        ValidatorRegistry validatorRegistry = new ValidatorRegistry() {
                public void registerValidator(Class parentType,
                                              QName elementType,
                                              Validator validator) {
                    
                }
                
                public Validator queryValidator(Class parentType,
                                                QName elementType) {
                    return null;
                }
        };
        ValidatingWSDLReaderImpl instance = new ValidatingWSDLReaderImpl();
        
        instance.setValidatorRegistry(validatorRegistry);
        ValidatorRegistry result = instance.getValidatorRegistry();
        assertEquals(validatorRegistry, result);
    }

    /**
     * Test of readWSDL method, of class com.sun.jbi.wsdlvalidator.impl.ValidatingWSDLReaderImpl.
     */
    public void testReadWSDL() throws Exception {
        File currentDir = new File("test/com/sun/jbi/wsdlvalidator/impl/data");
        ValidatingWSDLReaderImpl instance = new ValidatingWSDLReaderImpl();
        
        Map result = instance.readWSDL(currentDir);
        Iterator it = result.values().iterator();
        int counter = 0;
        while (it.hasNext()) {
            Object def = it.next();
            assertNotNull(def);
            try {
                Definition someDef = (Definition)def;
            } catch (Exception ex) {
                fail("Result of readWSDL() method should return a collection " +
                     "of Definition objects.  It returned a collection with " +
                     "an object of type " + def.getClass().getName());
            }
            counter++;
        }
        assertEquals(2, counter);
    }
    
}
