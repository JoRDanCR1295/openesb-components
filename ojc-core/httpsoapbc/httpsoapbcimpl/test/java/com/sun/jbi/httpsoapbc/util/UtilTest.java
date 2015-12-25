package com.sun.jbi.httpsoapbc.util;

import org.w3c.dom.Node;

import junit.framework.TestCase;

public class UtilTest extends TestCase {

    public void testTextAsDomWithCheck() throws Exception {

	String x = "value";

	String y = "<a> <b/> </a>";

	String z = "<?xml  version=\"1.0\" encoding=\"UTF-8\" ?> <a> <b/> </a>";

	Node n1 = Util.textAsDomWithXMLCheck(x);

	assertNull(n1);

	Node n2 = Util.textAsDomWithXMLCheck(y);
	assertNotNull(n2);

	Node n3 = Util.textAsDomWithXMLCheck(z);
	assertNotNull(n3);

    }

}
