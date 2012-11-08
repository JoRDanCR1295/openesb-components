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
 * @(#)AspectSEEndpointFactoryTest.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.EndpointInfo.LinkType;
import com.sun.jbi.engine.aspect.endpoint.factory.AspectSEEndpointFactory;

public class AspectSEEndpointFactoryTest extends TestCase {

	public void testCreateEndpoint() throws Exception {
		AspectSEEndpointFactory f = new AspectSEEndpointFactory();

		EndpointInfo info = new EndpointInfo(true, "loggingPortTypeRole", new QName(
				"http://com.sun.jbi/aspect/logging", "loggingPortType"), new QName("http://com.sun.jbi/aspect/logging", "loggingPartner"), LinkType.standard);

		AspectSEEndpoint ept =   (AspectSEEndpoint) f.createEndpoint(info, "helloSU",
				"test/data");

		String pattern = ept.getAdvice().getAttribute("type");

		assertEquals(ept.getEntryType(), AspectSEEndpoint.EntryType.FILTER_REQUEST_REPLY);


	}

}
