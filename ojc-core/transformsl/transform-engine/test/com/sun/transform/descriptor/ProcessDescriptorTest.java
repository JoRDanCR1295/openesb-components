package com.sun.transform.descriptor;


import java.io.File;
import java.util.ArrayList;
import java.util.List;
import javax.xml.namespace.QName;
import junit.framework.Test;
import junit.framework.TestSuite;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.util.test.UtilTestCase;
import com.sun.jbi.common.xml.XmlUtil;
import com.sun.transform.engine.model.Activity;
import com.sun.transform.engine.model.Param;
import com.sun.transform.engine.model.ProcessDef;
import com.sun.transform.engine.model.Transform;
import com.sun.transform.engine.model.Param.Type;
import com.sun.transform.engine.xslt.XsltProcessFactory;
import com.sun.transform.engine.xslt.XsltTransform;

/**
 * Unit test for {@link XsltseDescriptor}.
 * @author Kevan Simpson
 */
public class ProcessDescriptorTest extends UtilTestCase {
	/**
	 * Creates test suite.
	 * @return test suite.
	 * @throws Exception
	 */
    public static Test suite() throws Exception {
        return new TestSuite(ProcessDescriptorTest.class);
    }

	/**
	 * @param name
	 */
	public ProcessDescriptorTest(String name) {
		super(name);
	}

	public void testParsingLiteral() throws Exception {
		TransformEndpoint endpt = createEndpt("literal", false);
		Transform[] tr = getTransforms(endpt, "copyEmpl");
		assertEquals("Entry has wrong transform count!", 1, tr.length);
		assertEquals("Transform - wrong file!", "Empl-Input2Output.xsl", tr[0].getFile());
		assertEquals("Transform - wrong inputPart!", "input-msg.empl-in", tr[0].getSource());
		assertEquals("Transform - wrong outputPart!", "output-msg.empl-out", tr[0].getResult());
		
		Param[] p = tr[0].getParams();
		assertEquals("Wrong param count!", 4, p.length);
		assertEquals("Param1 - wrong name!", "test1", p[0].getName());
		assertEquals("Param1 - wrong type!", Type.LITERAL, p[0].getType());
		assertEquals("Param1 - wrong source value!", "foo", p[0].getValue());
		assertEquals("Param2 - wrong name!", "test2", p[1].getName());
		assertEquals("Param2 - wrong type!", Type.LITERAL, p[1].getType());
		assertEquals("Param2 - wrong source value!", "bar", p[1].getValue());
		assertEquals("Param3 - wrong name!", "test3", p[2].getName());
		assertEquals("Param3 - wrong type!", Type.LITERAL, p[2].getType());
		assertEquals("Param3 - wrong source value!", "baz", p[2].getValue());
        assertEquals("Param4 - wrong name!", "test4", p[3].getName());
        assertEquals("Param4 - wrong type!", Type.LITERAL, p[3].getType());
        assertEquals("Param4 - wrong source value!", "whodat", p[3].getValue());
	}

	public void testParsingElementLiteral() throws Exception {
		TransformEndpoint endpt = createEndpt("elementliteral", false);
		Transform[] tr = getTransforms(endpt, "copyEmpl");
		assertEquals("Entry has wrong transform count!", 1, tr.length);
		assertEquals("Transform - wrong file!", "Empl-Input2Output.xsl", tr[0].getFile());
		assertEquals("Transform - wrong inputPart!", "input-msg.empl-in", tr[0].getSource());
		assertEquals("Transform - wrong outputPart!", "output-msg.empl-out", tr[0].getResult());
	
		// load xml element from file to compare against 2nd param
		Document expectedDoc = XmlUtil.readXml(
				getClass().getResourceAsStream("elementliteral/emplList.xml"));
		
		Param[] p = tr[0].getParams();
		assertEquals("Wrong param count!", 4, p.length);
		assertEquals("Param1 - wrong name!", "test1", p[0].getName());
		assertEquals("Param1 - wrong type!", Type.LITERAL, p[0].getType());
		assertEquals("Param1 - wrong source value!", "foo", p[0].getValue());
		assertEquals("Param2 - wrong name!", "test2", p[1].getName());
		assertEquals("Param2 - wrong type!", Type.LITERAL, p[1].getType());
		Object src2 = p[1].getValue();
		assertTrue("Param2 - source wrong type: "+ src2.getClass().getName(), 
				   src2 instanceof Element);
		Element param2 = (Element) src2;
		assertTrue("Param2 - wrong source value!", getXmlTester().compareXml(
		        expectedDoc.getDocumentElement(), param2)); 
		
		assertEquals("Param3 - wrong name!", "test3", p[2].getName());
		assertEquals("Param3 - wrong type!", Type.LITERAL, p[2].getType());
        Object src3 = p[2].getValue();
        assertTrue("Param3 - source wrong type: "+ src3.getClass().getName(), 
                   src3 instanceof DocumentFragment);
        DocumentFragment frag3 = (DocumentFragment) src3;
        DocumentFragment expectedFrag3 = XmlUtil.createDocumentFragment(
                expectedDoc.getDocumentElement().getChildNodes());
        assertTrue("Param3 - wrong source value!", 
                   getXmlTester().compareXml(expectedFrag3, frag3));
        assertEquals("Param4 - wrong name!", "test4", p[3].getName());
        assertEquals("Param4 - wrong type!", Type.LITERAL, p[3].getType());
        assertEquals("Param4 - wrong source value!", "bar", p[3].getValue());
	}

	public void testParsingFileParam() throws Exception {
		TransformEndpoint endpt = createEndpt("file", false);
		Transform[] tr = getTransforms(endpt, "copyEmpl");
		assertEquals("Entry has wrong transform count!", 1, tr.length);
		assertEquals("Transform - wrong file!", "Empl-Input2Output.xsl", tr[0].getFile());
		assertEquals("Transform - wrong inputPart!", "input-msg.empl-in", tr[0].getSource());
		assertEquals("Transform - wrong outputPart!", "output-msg.empl-out", tr[0].getResult());
	
		// load xml element from file to compare against 2nd param
		Document expectedDoc1 = XmlUtil.readXml(
				        getClass().getResourceAsStream("file/emplList.xml")),
				 expectedDoc2 = XmlUtil.readXml(
		                getClass().getResourceAsStream("file/param-empl.xml"));
		DocumentFragment expectedFrag = XmlUtil.createDocumentFragment(
		        expectedDoc2.getDocumentElement().getChildNodes());
		String expectedText = "This is my text content.";
		Param[] p = tr[0].getParams();
		assertEquals("Wrong param count!", 3, p.length);
		assertEquals("Param1 - wrong name!", "test1", p[0].getName());
		assertEquals("Param1 - wrong type!", Type.URI, p[0].getType());
		Object src1 = p[0].getValue();
        assertTrue("Param1 - source wrong type: "+ src1.getClass().getName(), 
                src1 instanceof Element);
		assertTrue("Param1 - wrong source value!", 
		           getXmlTester().compareXml(
		                   expectedDoc1.getDocumentElement(), (Element) src1));
		assertEquals("Param2 - wrong name!", "test2", p[1].getName());
		assertEquals("Param2 - wrong type!", Type.URI, p[1].getType());
		Object src2 = p[1].getValue();
		assertTrue("Param2 - source wrong type: "+ src2.getClass().getName(), 
				   src2 instanceof DocumentFragment);
		assertTrue("Param2 - wrong source value!", 
		           getXmlTester().compareXml(expectedFrag, (DocumentFragment) src2));
		assertEquals("Param3 - wrong name!", "test3", p[2].getName());
		assertEquals("Param3 - wrong type!", Type.URI, p[2].getType());
        Object src3 = p[2].getValue();
        assertTrue("Param3 - source wrong type: "+ src3.getClass().getName(), 
                src3 instanceof String);
		assertEquals("Param3 - wrong source value!", expectedText, (String) src3);
	}

	TransformEndpoint createEndpt(String rsrc, boolean reply) throws Exception {
		File path = new File(getClass().getResource(rsrc).toURI());
		ProcessDescriptor desc = ProcessDescriptor.parse(
		        path.getAbsolutePath(), new XsltProcessFactory());
		QName portType, 
		      srvcName = QName.valueOf("{http://sun.com/processDescriptor/"+ rsrc +"}xsltse");
		if (!reply) {
			portType = QName.valueOf("{http://sun.com/XsltRRTest}xsltRRPort");
		}
		else {
			portType = QName.valueOf("{http://sun.com/XsltFilterReplyTest}xsltReplyPort");
		}

		EndpointInfo info = new EndpointInfo(true, "TestService", portType, srvcName, null);
		return desc.lookupEndpointDef(info);
	}
	
	Transform[] getTransforms(TransformEndpoint endpt, String opName) throws Exception {
		List<Transform> list = new ArrayList<Transform>();
		ProcessDef procDef = endpt.getServiceDef(opName);
		for (int i = 0, n = procDef.countActivities(); i < n; i++) {
			Activity act = procDef.getActivity(i);
			if (act instanceof Transform) {
				assertTrue("Wrong transform type: "+ act.getClass().getName(), 
						   act instanceof XsltTransform);
				list.add((Transform) act);
			}
		}
		Transform[] tr = new Transform[list.size()];
		list.toArray(tr);
		return tr;
	}
}
