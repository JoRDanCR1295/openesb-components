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
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.xml.sax.InputSource;

import junit.framework.TestCase;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.Connection;
import com.sun.jbi.common.qos.ServiceQuality;
import com.sun.jbi.common.qos.messaging.tracking.TrackingConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.common.qos.throttling.ThrottlingConfig;

/**
 * Tests {@link JbiDescriptor} implementations.
 * @author Kevan Simpson
 */
public class QosDescriptorTest extends TestCase {

	/**
	 * @param name
	 */
	public QosDescriptorTest(String name) {
		super(name);
	}

	public void testMultipleConnections() throws Exception {
        QoSAssembly sa = QosAssemblyDescriptor.parse(
                getSource("assembly", "multiple.xml"));
        Connection[] cons = sa.getQoSConnections();
        assertEquals("connection count", 3, cons.length);
        Map<EndpointInfo, List<ServiceQuality>> map = sa.getServiceQualities();
        assertNotNull("qos map is null", map);
        assertEquals("qos map size", 3, map.size());
        /*
        <connection>
        <consumer endpoint-name="SubProcessPortTypeRole_partnerRole" service-name="ns1:PartnerLink2"/>
        <provider endpoint-name="SubProcessPortTypeRole_myRole" service-name="ns2:PartnerLink1"/>
        <redelivery xmlns="http://www.sun.com/jbi/qos/redelivery" maxAttempts="" waitTime="">
            <on-failure>
              <suspend/>
            </on-failure>
          </redelivery>
        </connection>
        */
        assertEquals("1st consumer info", 
                     new EndpointInfo(false, "SubProcessPortTypeRole_partnerRole", null, 
                                      QName.valueOf("{http://enterprise.netbeans.org/bpel/BPAtomicTest1/Main}PartnerLink2"), null),
                     cons[0].getConsumer());
        assertEquals("1st provider info", 
                     new EndpointInfo(true, "SubProcessPortTypeRole_myRole", null, 
                                      QName.valueOf("{http://enterprise.netbeans.org/bpel/BPAtomicTest1/SubProcess}PartnerLink1"), null),
                     cons[0].getProvider());
        List<ServiceQuality> qos = map.get(cons[0].getConsumer());
        assertNotNull("1st qos list is null", qos);
        assertEquals("1st qos size", 1, qos.size());
        // test redelivery
        ServiceQuality qual = qos.get(0);
        assertTrue("1st wrong quality: redelivery", (qual instanceof RedeliveryConfig));
        RedeliveryConfig rd = (RedeliveryConfig) qual;
        assertEquals("1st wrong failure", Failure.suspend, rd.getFailure());

        /*
        <connection>
        <consumer endpoint-name="Ejb1PortTypeRole_partnerRole" service-name="ns2:PartnerLink2"/>
        <provider endpoint-name="javaee_Ejb1Port" service-name="ns3:Ejb1Service"/>
        <throttling xmlns="http://www.sun.com/jbi/qos/throttling" maximumConcurrencyLimit="16"/>
        </connection>
        */
        assertEquals("2nd consumer info", 
                     new EndpointInfo(false, "Ejb1PortTypeRole_partnerRole", null, 
                                      QName.valueOf("{http://enterprise.netbeans.org/bpel/BPAtomicTest1/SubProcess}PartnerLink2"), null),
                     cons[1].getConsumer());
        assertEquals("2nd provider info", 
                     new EndpointInfo(true, "javaee_Ejb1Port", null, 
                                      QName.valueOf("{http://j2ee.netbeans.org/wsdl/Ejb1}Ejb1Service"), null),
                     cons[1].getProvider());
        qos = map.get(cons[1].getConsumer());
        assertNotNull("2nd qos list is null", qos);
        assertEquals("2nd qos size", 1, qos.size());
        qual = qos.get(0);
        assertTrue("2nd wrong quality: throttling", (qual instanceof ThrottlingConfig));
        assertEquals("2nd throttling - wrong max", 16, ((ThrottlingConfig) qual).getMaxConcurrencyLimit());
        /*
        <connection>
        <consumer endpoint-name="Ejb2PortTypeRole_partnerRole" service-name="ns2:PartnerLink3"/>
        <provider endpoint-name="javaee_Ejb2Port" service-name="ns5:Ejb2Service"/>
        <throttling xmlns="http://www.sun.com/jbi/qos/throttling" maximumConcurrencyLimit="16"/>
        </connection>
        */
        assertEquals("3rd consumer info", 
                     new EndpointInfo(false, "Ejb2PortTypeRole_partnerRole", null, 
                                      QName.valueOf("{http://enterprise.netbeans.org/bpel/BPAtomicTest1/SubProcess}PartnerLink3"), null),
                     cons[2].getConsumer());
        assertEquals("3rd provider info", 
                     new EndpointInfo(true, "javaee_Ejb2Port", null, 
                                      QName.valueOf("{http://j2ee.netbeans.org/wsdl/Ejb2}Ejb2Service"), null),
                     cons[2].getProvider());
       qos = map.get(cons[2].getConsumer());
       assertNotNull("3rd qos list is null", qos);
       assertEquals("3rd qos size", 1, qos.size());
       qual = qos.get(0);
       assertTrue("3rd wrong quality: throttling", (qual instanceof ThrottlingConfig));
       assertEquals("3rd throttling - wrong max", 13, ((ThrottlingConfig) qual).getMaxConcurrencyLimit());
	}
	
	public void testServiceQualities() throws Exception {
        QoSAssembly sa = QosAssemblyDescriptor.parse(
                getSource("assembly", JbiDescriptor.JBI_DESC_FILE_NAME));
        runQosConfigAssertions(sa);
    }

	public void testInlineNSDecls() throws Exception {
		QoSAssembly sa = QosAssemblyDescriptor.parse(
                getSource("assembly", "inline-ns.xml"));
        runQosConfigAssertions(sa);
    }
    
	protected InputSource getSource(String dir, String file) {
        String path = dir + File.separator + JbiDescriptor.META_INF_DIR
                + File.separator + file;
        return new InputSource(this.getClass().getResourceAsStream(path));
	}
	
    protected void runQosConfigAssertions(QoSAssembly sa) throws Exception {
		Connection[] cons = sa.getQoSConnections();
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

		Map<EndpointInfo, List<ServiceQuality>> map = sa.getServiceQualities();
		assertNotNull("qos map is null", map);
		assertEquals("map size", 1, map.size());
		List<ServiceQuality> qos = map.get(consumer);
		assertNotNull("qos list is null", qos);
		assertEquals("qos size", 3, qos.size());
        // test redelivery
		ServiceQuality qual = qos.get(0);
		assertTrue("wrong quality: redelivery", (qual instanceof RedeliveryConfig));
		RedeliveryConfig rd = (RedeliveryConfig) qual;
		assertEquals("wrong failure", Failure.redirect, rd.getFailure());
		assertNotNull("redirect endpoint is null", rd.getRedirect());
		assertEquals("redirect endpt service name", 
					 QName.valueOf("{http://j2ee.netbeans.org/wsdl/CandidateEvaluation}DeadLetterService"),
					 rd.getRedirect().getEndpoint().getServiceName());
		assertEquals("error endpt endpt name", "DeadLetterQueue", 
		             rd.getRedirect().getEndpoint().getEndpointName());
		assertEquals("redirect endpt operation", "throwAway", 
		             rd.getRedirect().getOperation().getLocalPart());
        // test throttling
        qual = qos.get(1);
        assertTrue("wrong quality: throttling", (qual instanceof ThrottlingConfig));
        ThrottlingConfig th = (ThrottlingConfig) qual;
        assertEquals("wrong max concurrency", 10, th.getMaxConcurrencyLimit());
        // test message tracking
        qual = qos.get(2);
        assertTrue("wrong quality: tracking", (qual instanceof TrackingConfig));
        TrackingConfig tc = (TrackingConfig) qual;
        /*
         *              <message-tracking xmlns="http://www.sun.com/jbi/qos/message-tracking" 
                        tracking="true"    store-payload="true"    externalize-payload="false">
                    <payload-query name="MyKPIField" xpath="/some/xpath/applied/against/payload" />
                </message-tracking>

         */
        assertTrue("tracking not enabled", tc.isTrackingEnabled());
        assertTrue("store payload not enabled", tc.doStorePayload());
        assertFalse("externalize payload enabled", tc.doExternalizePayload());
        Map<String, String> queries = tc.getPayloadQueries();
        assertNotNull("payload query map is null", queries);
        assertEquals("payload query count wrong", 1, queries.size());
        assertTrue("payload query missing", queries.containsKey("MyKPIField"));
        assertEquals("wrong payload query", 
                     "/some/xpath/applied/against/payload", 
                     queries.get("MyKPIField"));
        queries = tc.getMessageQueries();
        assertNotNull("message query map is null", queries);
        assertEquals("message query count wrong", 0, queries.size());
	}
}
