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
 * @(#)ServicesDescriptorTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor;

import java.io.File;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.xml.sax.InputSource;

import com.sun.jbi.common.descriptor.model.AssemblyUnit;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.descriptor.model.Identification;

/**
 * Tests {@link JbiDescriptor} implementations.
 * @author Kevan Simpson
 */
public class JbiDescriptorTest extends TestCase {

	/**
	 * @param name
	 */
	public JbiDescriptorTest(String name) {
		super(name);
	}

	public void testServices() throws Exception {
	    runServicesTest("services");
	}
	
	public void testInlineNSDecl() throws Exception {
	    runServicesTest("inline");
	}
	
	protected void runServicesTest(String rsrc) throws Exception {
		String suPath = (new File(this.getClass().getResource(rsrc).toURI())).getAbsolutePath();
		ServiceUnit su = ServicesDescriptor.parse("testSU", suPath);
		EndpointInfo[] endpts = su.getServices().getEndpoints();
		assertEquals("service count", 2, endpts.length);
		int toggle = 0;
		for (EndpointInfo info : endpts) {
			assertEquals("service name", 
						 QName.valueOf("{http://j2ee.netbeans.org/wsdl/CandidateEvaluation}CandidateEvaluator"), 
						 info.getServiceName());
			assertEquals("endpt name", "evaluator", info.getEndpointName());
			assertEquals("interface name", 
						 QName.valueOf("{http://j2ee.netbeans.org/wsdl/CandidateEvaluation}CandidateEvaluationPortType"), 
						 info.getInterfaceName());
			toggle += (info.isProvides()) ? 1 : -1;
		}
		assertEquals("provides/consumes count", 0, toggle);
	}
	
	public void testServiceAssembly() throws Exception {
        String path = "assembly" + File.separator + JbiDescriptor.META_INF_DIR
                + File.separator + JbiDescriptor.JBI_DESC_FILE_NAME;
        InputSource src = new InputSource(this.getClass().getResourceAsStream(path));
		ServiceAssembly sa = ServiceAssemblyDescriptor.parse(src);
        // test id
        Identification id = sa.getIdentification();
        assertNotNull("id is null", id);
        assertEquals("id name", "EmployeeCA", id.getName());
        assertEquals("id desc", "Represents the Service Assembly of EmployeeCA", id.getDescription());
        // test units
        AssemblyUnit[] units = sa.getServiceUnits();
        assertNotNull("units are null", units);
        assertEquals("units count", 2, units.length);
        // xslt unit
        assertEquals("xslt unit name", "EmployeeCA-EmployeeXslt", units[0].getId().getName());
        assertEquals("xslt unit desc", "Represents this Service Unit", units[0].getId().getDescription());
        assertEquals("xslt unit target artifact", "EmployeeXslt.jar", units[0].getTarget().getArtifactsZip());
        assertEquals("xslt unit target compname", "sun-xslt-engine", units[0].getTarget().getComponentName());
        // http unit
        assertEquals("http unit name", "EmployeeCA-sun-http-binding", units[1].getId().getName());
        assertEquals("http unit desc", "Represents this Service Unit", units[1].getId().getDescription());
        assertEquals("http unit target artifact", "sun-http-binding.jar", units[1].getTarget().getArtifactsZip());
        assertEquals("http unit target compname", "sun-http-binding", units[1].getTarget().getComponentName());
        
        // test connections
		Connection[] cons = sa.getConnections();
		assertEquals("connection count", 1, cons.length);
		EndpointInfo consumer = cons[0].getConsumer(), provider = cons[0].getProvider();
		assertNotNull("consumer", consumer);
		assertEquals("consumer service name", 
					 QName.valueOf("{http://j2ee.netbeans.org/wsdl/CandidateEvaluation}CandidateEvaluationService"), 
					 consumer.getServiceName());
		assertEquals("consumer endpt name", "CandidateEvaluationPort", consumer.getEndpointName());
		assertNull("consumer interface name", consumer.getInterfaceName());
		assertNotNull("provider", provider);
		assertEquals("provider service name", 
					 QName.valueOf("{http://j2ee.netbeans.org/wsdl/CandidateEvaluation}CandidateEvaluator"), 
					 provider.getServiceName());
		assertEquals("provider endpt name", "evaluator", provider.getEndpointName());
		assertNull("provider interface name", provider.getInterfaceName());
	}
	
}
