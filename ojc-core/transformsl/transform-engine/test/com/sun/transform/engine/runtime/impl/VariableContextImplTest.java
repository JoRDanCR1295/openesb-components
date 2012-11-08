package com.sun.transform.engine.runtime.impl;


import javax.wsdl.Definition;
import javax.wsdl.Message;
import javax.xml.namespace.QName;
import javax.xml.transform.dom.DOMSource;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.jbi.common.util.test.UtilTestCase;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.transform.engine.runtime.MessageUtil;
import com.sun.transform.engine.runtime.VariableContext;
import com.sun.transform.engine.runtime.WSMessage;

/**
 * Unit test for {@link VariableContextImpl}.
 * @author Kevan Simpson
 */
public class VariableContextImplTest extends UtilTestCase {
	/**
	 * Creates test suite.
	 * @return test suite.
	 * @throws Exception
	 */
    public static Test suite() throws Exception {
        return new TestSuite(VariableContextImplTest.class);
    }

	/**
	 * @param name
	 */
	public VariableContextImplTest(String name) {
		super(name);
	}

	public void testSetVariable() throws Exception {
    	Definition wsdl = readWSDL("wsdl/jbimsg.wsdl");
    	Message msg = wsdl.getMessage(
    					QName.valueOf("{http://sun.com/JbiMsgTest}multipart-msg"));
    	assertNotNull("Missing wsdl msg!", msg);
    	assertEquals("Wsdl msg has wrong part count!", 3, msg.getParts().size());
    	Document doc = XmlUtil.readXml(getClass().getResourceAsStream("wsdl/multipart.xml"));
    	DOMSource src = new DOMSource(doc);
    	WSMessage wsmsg = MessageUtil.createWSMessage(src, msg);
    	assertNotNull("wsmessage is null!", wsmsg);
    	Element part1 = wsmsg.getPart("part1"), 
    			part2 = wsmsg.getPart("part2"), 
    			part3 = wsmsg.getPart("part3");
    	WSMessage expected = 
    	        MessageUtil.createWSMessage(new DOMSource(XmlUtil.newDocument()), msg);
    	VariableContext varCtx = new VariableContextImpl();
    	// WSMessage exists in context prior to setting part values
    	varCtx.setVariable("foo", expected);
    	varCtx.setVariable("foo.part1", part1);
    	varCtx.setVariable("foo.part2", part2);
    	varCtx.setVariable("foo.part3", part3);
    	WSMessage actual = (WSMessage) varCtx.getVariable("foo");
    	assertNotNull("actual msg is null!", actual);
    	assertTrue("part1 is not same", getXmlTester().compareXml(
    	           part1.getOwnerDocument(), actual.getPart("part1").getOwnerDocument()));
    	assertTrue("part2 is not same", getXmlTester().compareXml(
    	           part2.getOwnerDocument(), actual.getPart("part2").getOwnerDocument()));
    	assertTrue("part3 is not same", getXmlTester().compareXml(
    	           part3.getOwnerDocument(), actual.getPart("part3").getOwnerDocument()));
    	DOMSource expectSrc = XmlUtil.toDOMSource(expected.toSource()),
    			  actualSrc = XmlUtil.toDOMSource(actual.toSource());
    	assertTrue("msg is not same", getXmlTester().compareXml(
    	           (Document) expectSrc.getNode(), (Document) actualSrc.getNode()));
	}

	public void testUndeclaredVariable() throws Exception {
		Definition wsdl = readWSDL("wsdl/jbimsg.wsdl");
    	Message msg = wsdl.getMessage(
    					QName.valueOf("{http://sun.com/JbiMsgTest}multipart-msg"));
    	assertNotNull("Missing wsdl msg!", msg);
    	assertEquals("Wsdl msg has wrong part count!", 3, msg.getParts().size());
    	Document doc = XmlUtil.readXml(getClass().getResourceAsStream("wsdl/multipart.xml"));
    	DOMSource src = new DOMSource(doc);
    	WSMessage wsmsg = MessageUtil.createWSMessage(src, msg);
    	assertNotNull("wsmessage is null!", wsmsg);
    	Element part1 = wsmsg.getPart("part1"), 
    			part2 = wsmsg.getPart("part2"), 
    			part3 = wsmsg.getPart("part3");
    	VariableContext varCtx = new VariableContextImpl();
    	// WSMessage does not exist in context prior to setting part values
    	varCtx.setVariable("foo", null);
    	varCtx.setVariable("foo.part1", part1);
    	varCtx.setVariable("foo.part2", part2);
    	varCtx.setVariable("foo.part3", part3);
    	
    	WSMessage actual = (WSMessage) varCtx.getVariable("foo");
    	// not declared yet, should be null
    	assertNull("undeclared variable exists!", actual);
    	WSMessage expected = 
    	    MessageUtil.createWSMessage(new DOMSource(XmlUtil.newDocument()), msg);
    	// declare variable to context
    	varCtx.setVariable("foo", expected);
    	actual = (WSMessage) varCtx.getVariable("foo");
    	assertNotNull("actual msg is null!", actual);
    	assertTrue("part1 is not same", getXmlTester().compareXml(
    	           part1.getOwnerDocument(), actual.getPart("part1").getOwnerDocument()));
    	assertTrue("part2 is not same", getXmlTester().compareXml(
    	           part2.getOwnerDocument(), actual.getPart("part2").getOwnerDocument()));
    	assertTrue("part3 is not same", getXmlTester().compareXml(
    	           part3.getOwnerDocument(), actual.getPart("part3").getOwnerDocument()));
    	DOMSource expectSrc = XmlUtil.toDOMSource(expected.toSource()),
    			  actualSrc = XmlUtil.toDOMSource(actual.toSource());
    	assertTrue("msg is not same", getXmlTester().compareXml(
    	           (Document) expectSrc.getNode(), (Document) actualSrc.getNode()));
	}
}
