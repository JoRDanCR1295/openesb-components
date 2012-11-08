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
 * @(#)JDBCExtSerializerTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.extensions;

import com.ibm.wsdl.Constants;
import com.ibm.wsdl.factory.WSDLFactoryImpl;
import com.sun.wsdl4j.ext.impl.WSDLFactoryEx;
import com.sun.wsdl4j.ext.impl.WSDLFactoryEx;
import com.sun.wsdl4j.ext.impl.WSDLReaderEx;
import junit.framework.*;

import org.apache.xml.resolver.CatalogManager;
import org.apache.xml.resolver.tools.CatalogResolver;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import org.xml.sax.InputSource;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import javax.wsdl.Definition;
import javax.wsdl.extensions.ExtensibilityElement;
import javax.wsdl.extensions.ExtensionRegistry;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLReader;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;


/**
 *
 *
 */
public class JDBCExtSerializerTest extends TestCase {
    Class parentType = null;
    Definition def = null;
    ExtensionRegistry extReg = null;
    JDBCExtSerializer instance = null;
    QName bindingElementType = null;
    QName bindingOperationElementType = null;
    QName addressElementType = null;
    Document doc = null;
    WSDLFactoryEx wsdlFactory = null;
    WSDLReaderEx reader = null;
    String outputFolder = null;
    String expectedFolder = null;

    public JDBCExtSerializerTest(final String testName) {
        super(testName);
    }

    //@Override
	public void setUp() throws Exception {
        instance = new JDBCExtSerializer();
        extReg = new ExtensionRegistry();
        bindingElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/jdbc/",
                Constants.ELEM_BINDING);
        bindingOperationElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/jdbc/",
                Constants.ELEM_OPERATION);
        addressElementType = new QName("http://schemas.sun.com/jbi/wsdl-extensions/jdbc/",
                "address");
        outputFolder = "test/org/glassfish/openesb/databasebc/extensions/output/";
        expectedFolder = "test/org/glassfish/openesb//databasebc/extensions/expected/";

        final BufferedReader br = new BufferedReader(new InputStreamReader(
                    new FileInputStream(
                        new File(
                            "test/org/glassfish/openesb/databasebc/packaging/wsdls/TestJdbc.wsdl")),
                    "UTF-8"));
        //wsdlFactory = WSDLFactory.newInstance();
        //reader = ((WSDLFactoryImpl) wsdlFactory).newWSDLReader(new CatalogResolver(
         //           new CatalogManager()));
        //def = reader.readWSDL(new File(
        //            "test/org/glassfish/openesb/databasebc/packaging/wsdls/TestJdbc.wsdl").getAbsolutePath());

        // above default Implementation changed with current sun extension
            wsdlFactory = new WSDLFactoryEx();
            reader = wsdlFactory.newWSDLReaderEx();
            reader.setEntityResolver(new CatalogResolver(new CatalogManager()));
            reader.setExtensionRegistry(new JDBCExtensionRegistry(instance));
            def = reader.readWSDL(new File(
                   "test/org/glassfish/openesb/databasebc/packaging/wsdls/TestJdbc.wsdl").getAbsolutePath());
        
        try {
            final InputSource is = new InputSource(br);
            final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db;

            synchronized (dbf) {
                dbf.setNamespaceAware(true);
                db = dbf.newDocumentBuilder();
            }

            doc = db.parse(is);
        } catch (final Exception e) {
            Assert.fail(
                "Something went wrong during parsing TestJdbc.wsdl, cannot proceed");
        }

        if (doc == null) {
            Assert.fail(
                "Something went wrong during parsing TestJdbc.wsdl, cannot proceed");
        }
    }

    //@Override
	public void tearDown() throws Exception {
    }

    public static Test suite() {
        final TestSuite suite = new TestSuite(JDBCExtSerializerTest.class);

        return suite;
    }

    /**
     * Test of marshall method, of class org.glassfish.openesb.databasebc.extensions.JdbcExtSerializer.
     */
    public void testMarshall() throws Exception {
        System.out.println("Testing marshall");
        System.out.println("Successfully tested marshal");
    }

    /**
     * Test of unmarshall method, of class org.glassfish.openesb.databasebc.extensions.JdbcExtSerializer.
     */
    public void testUnmarshall() throws Exception {
        System.out.println("Testing unmarshall");

        Element xelem = null;
        ExtensibilityElement result = null;

        // 1. testing file:binding element
        xelem = getElement(doc, "jdbc:binding");
        result = instance.unmarshall(null, bindingElementType, xelem, null,
                extReg);
        Assert.assertTrue(result instanceof JDBCBinding);

        // 2. testing file:operation element
        xelem = getElement(doc, "jdbc:operation");

        if (xelem != null) {
            result = instance.unmarshall(null, bindingOperationElementType,
                    xelem, null, extReg);
            Assert.assertTrue(result instanceof JDBCOperation);

            //JDBCOperation oper = (JDBCOperation)result;
            //assertEquals("insert", oper.getOperationType());
        } else {
            Assert.fail(
                "Something went wrong during parsing TestFile.wsdl, cannot proceed");
        }

        // 3. testing file:address element
        xelem = getElement(doc, "jdbc:address");

        if (xelem != null) {
            result = instance.unmarshall(null, addressElementType, xelem, null,
                    extReg);
            Assert.assertTrue(result instanceof JDBCAddress);

            final JDBCAddress address = (JDBCAddress) result;
            Assert.assertEquals("jdbc/__OrclPool", address.getJndiName());
        } else {
            Assert.fail(
                "Something went wrong during parsing TestJdbc.wsdl, cannot proceed");
        }

        System.out.println("Successfully tested unmarshal");
    }

    public Element getElement(final Node aNode, final String elementName) {
        Element theOne = null;

        if (aNode.getNodeName().equalsIgnoreCase(elementName)) {
            return (Element) aNode;
        }

        final NodeList children = aNode.getChildNodes();

        for (int ii = 0; ii < children.getLength(); ii++) {
            final Node child = children.item(ii);

            if (child.getNodeName().equalsIgnoreCase(elementName)) {
                theOne = (Element) child;

                break;
            } else {
                theOne = getElement(child, elementName);

                if (theOne != null) {
                    break;
                }
            }
        }

        return theOne;
    }
}
