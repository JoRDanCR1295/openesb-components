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

package com.sun.jbi.common.qos.descriptor;

import java.io.File;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.Services;

/**
 * Tests {@link JbiDescriptor} implementations.
 * @author Kevan Simpson
 */
public class QosServicesTest extends TestCase {

	/**
	 * @param name
	 */
	public QosServicesTest(String name) {
		super(name);
	}

    public void testAppConfig() throws Exception {
        QosServices srvcs = QosServicesDescriptor.parse(getPath("services"));
        // verify JBI parsing is still working as expected
        runServicesTest(srvcs);
        // test app config
        assertEquals("wrong provider count", 1, srvcs.getProvides().length);
        assertEquals("wrong provider config", 
                     "myProviderConfig", 
                     srvcs.getApplicationConfiguration(srvcs.getProvides()[0]));
        assertEquals("wrong consumer count", 1, srvcs.getConsumes().length);
        assertEquals("wrong consumer config", 
                     "myConsumerConfig", 
                     srvcs.getApplicationConfiguration(srvcs.getConsumes()[0]));
    }

//    protected void runServicesTest(String rsrc) throws Exception {
//        String suPath = (new File(this.getClass().getResource(rsrc).toURI())).getAbsolutePath();
//        ServiceUnit su = ServicesDescriptor.parse("testSU", suPath);
    protected void runServicesTest(Services srvcs) throws Exception {
//        EndpointInfo[] endpts = su.getServices().getEndpoints();
        EndpointInfo[] endpts = srvcs.getEndpoints();
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

	protected String getPath(String path) throws Exception {
        File rsrc = new File(this.getClass().getResource(path).toURI());
        return rsrc.getAbsolutePath();
	}
}
