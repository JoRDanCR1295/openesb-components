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
 * @(#)WrapperUtilTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.nms.wsdl11wrapper.util;

import java.io.InputStream;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilderFactory;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;
import com.sun.jbi.nms.wsdl11wrapper.impl.WrapperBuilderImpl;

/**
 * 
 * @author Kevan Simpson
 */
public class WrapperUtilTest extends TestCase {
	private static DocumentBuilderFactory mFactory= DocumentBuilderFactory.newInstance();
	
	public static Test suite() {
        return new TestSuite(WrapperUtilTest.class);
    }
    
	/**
	 * @param name
	 */
	public WrapperUtilTest(String name) {
		super(name);
	}

	public void testExtractNamespaceDeclarations() throws Exception {
		/*
				  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
				  xmlns:xsd="http://www.w3.org/2001/XMLSchema" 
				  xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" 
				  xmlns:emp="http://sun.com/EmplInput"
		 */
		Document doc = readXml(getClass().getResourceAsStream("ns-decl-elem.xml"));
		Element elem = doc.getDocumentElement();
		Map map = WrapperUtil.extractNamespaceDeclarations(elem);
		runNSMapTest(map);
	}
	
	public void testCopyNamespaces() throws Exception {
		Document doc = readXml(getClass().getResourceAsStream("ns-decl-elem.xml"));
		Element elem = doc.getDocumentElement();
		Map map = WrapperUtil.extractNamespaceDeclarations(elem);
		runNSMapTest(map);
		assertEquals("elem missing ns", 4, tallyNSDecl(elem, map));
		// remove namespace declaration attributes
		for (Iterator iter = map.keySet().iterator(); iter.hasNext();) {
			elem.removeAttribute((String) iter.next());
		}
		// verify their removal
		assertEquals("elem still has ns", 0, tallyNSDecl(elem, map));
		CopyNSBuilder bldr = new CopyNSBuilder();
		// add them back and verify 
		bldr.copyNamespaces(elem, map);
		assertEquals("elem missing ns", 4, tallyNSDecl(elem, map));
	}
	
	private int tallyNSDecl(Element elem, Map map) {
		int tally = 0;
		for (Iterator iter = map.keySet().iterator(); iter.hasNext();) {
			if (elem.hasAttribute((String) iter.next())) tally += 1;
		}
		
		return tally;
	}
	
	private void runNSMapTest(Map map) throws Exception {
		assertNotNull("elem map is null", map);
		assertEquals("elem wrong size", 4, map.size());
		Set keys = map.keySet();
		assertTrue("elem keys missing xsi", keys.contains("xmlns:xsi"));
		assertTrue("elem keys missing xsd", keys.contains("xmlns:xsd"));
		assertTrue("elem keys missing soapenv", keys.contains("xmlns:soapenv"));
		assertTrue("elem keys missing emp", keys.contains("xmlns:emp"));
		assertEquals("elem ns1 wrong", "http://www.w3.org/2001/XMLSchema-instance", (String) map.get("xmlns:xsi"));
		assertEquals("elem ns2 wrong", "http://www.w3.org/2001/XMLSchema", (String) map.get("xmlns:xsd"));
		assertEquals("elem ns3 wrong", "http://schemas.xmlsoap.org/soap/envelope/", (String) map.get("xmlns:soapenv"));
		assertEquals("elem ns3 wrong", "http://sun.com/EmplInput", (String) map.get("xmlns:emp"));
	}
	
	private Document readXml(InputStream stream) throws Exception {
		return mFactory.newDocumentBuilder().parse(stream);
	}
	
	private static class CopyNSBuilder extends WrapperBuilderImpl {
	    public CopyNSBuilder() throws WrapperProcessingException {
	    }

		public void copyNamespaces(Element elem, Map namespaces) {
			super.copyNamespaces(elem, namespaces);
		}
	}
}
