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
 * @(#)TaskXmlWriterTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.crl.service.impl;

import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMSource;

import junit.framework.TestCase;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

import com.sun.jbi.crl.service.WSMessage;
import com.sun.jbi.crl.xml.XmlUtil;
import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 *
 * @author Kevan Simpson
 */
public class JbiMessageTest extends TestCase {
	private static WSDLReader mWsdlReader = WSDL4JExt.newWSDLReader(null);
    /**
     * Constructs a test case for {@link JbiMessage}.
     * @param testName The name of the test.
     */
    public JbiMessageTest(String testName) {
        super(testName);
    }

    public void testCreateWSMessage() throws Exception {
    	Definition wsdl = readWSDL("wsdl/jbimsg.wsdl");
    	Message msg = wsdl.getMessage(
    					QName.valueOf("{http://sun.com/JbiMsgTest}multipart-msg"));
    	assertNotNull("Missing wsdl msg!", msg);
    	assertEquals("Wsdl msg has wrong part count!", 3, msg.getParts().size());
    	Document doc = XmlUtil.readXml(getClass().getResourceAsStream("wsdl/multipart.xml"));
    	DOMSource src = new DOMSource(doc);
    	WSMessage wsmsg = XmlUtil.createWSMessage(src, msg);
    	assertNotNull("wsmessage is null!", wsmsg);
    	Element part1 = wsmsg.getPart("part1"), part2 = wsmsg.getPart("part2"), part3 = wsmsg.getPart("part3");
    	assertNotNull("wsmsg missing 1st part!", part1);
    	assertEquals("part1 wrong ns!", "http://sun.com/EmplInput", part1.getNamespaceURI());
    	assertEquals("part1 wrong type!", "EmployeeInput", part1.getLocalName());
    	assertNotNull("wsmsg missing 2nd part!", part2);
    	assertEquals("part2 wrong ns!", "http://sun.com/JbiMsgTest", part2.getNamespaceURI());
    	assertEquals("part2 wrong type!", "dblstr", part2.getLocalName());
    	assertNotNull("wsmsg missing 3rd part!", part3);
    	assertEquals("part3 wrong ns!", "http://sun.com/EmplOutput", part3.getNamespaceURI());
    	assertEquals("part3 wrong type!", "EmployeeOutput", part3.getLocalName());
    }
    
    public void testToSource() throws Exception { 
    	Definition wsdl = readWSDL("wsdl/jbimsg.wsdl");
    	Message msg = wsdl.getMessage(
    					QName.valueOf("{http://sun.com/JbiMsgTest}multipart-msg"));
    	assertNotNull("Missing wsdl msg!", msg);
    	assertEquals("Wsdl msg has wrong part count!", 3, msg.getParts().size());
    	Document doc = XmlUtil.readXml(getClass().getResourceAsStream("wsdl/multipart.xml"));
    	Element originalRoot = doc.getDocumentElement();
    	DOMSource src = new DOMSource(doc);
    	WSMessage wsmsg = XmlUtil.createWSMessage(src, msg);
    	Source actual = wsmsg.toSource();
    	assertNotNull("actual source is null!", actual);
    	assertTrue("actual is not dom source!", actual instanceof DOMSource);
    	DOMSource dom = (DOMSource) actual;
    	assertNotSame("same instance", originalRoot,//doc.getDocumentElement(), 
    								   ((Document) dom.getNode()).getDocumentElement());
    	assertTrue("xml is not correct!", compareXml(doc, dom.getNode().getOwnerDocument()));
    }
    
    private Definition readWSDL(String path) throws Exception {
    	return mWsdlReader.readWSDL(
    			getClass().getResource(path).toURI().toString(), 
    			new InputSource(getClass().getResourceAsStream(path)));
    }
    
	public static boolean compareXml(Document doc1, Document doc2) throws Exception {
		String xml1 = XmlUtil.print(new DOMSource(doc1)), xml2 = XmlUtil.print(new DOMSource(doc2));
		char[] c1 = xml1.trim().toCharArray(), c2 = xml2.trim().toCharArray();
		int len1 = c1.length, len2 = c2.length;
		assertFalse("Doc1 has no content!", len1 == 0);
		assertFalse("Doc2 has no content!", len2 == 0);
		for (int i1 = 0, i2 = 0; i1 < len1 && i2 < len2; i1++, i2++) {
			// ignore whitespace
			while (Character.isWhitespace(c1[i1])) i1++;
			while (Character.isWhitespace(c2[i2])) i2++;
			assertEquals("Unequal character at i1:"+ i1 +", i2:"+ i2, c1[i1], c2[i2]);
		}
		return true;
	}

}
