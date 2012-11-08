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
 * @(#)AspectSEEndpointFactory.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.factory;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.management.DeploymentException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.sun.jbi.component.endpoint.Endpoint;
import com.sun.jbi.component.endpoint.EndpointFactory;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.crl.util.Util;
import com.sun.jbi.engine.aspect.endpoint.AspectSEAutoReconnectEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSECacheEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSEContentBasedRoutingEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSEEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSELoggingEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSEMessageTrackingEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSEQueuingEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSETeeEndpoint;
import com.sun.jbi.engine.aspect.endpoint.AspectSEThrottlingEndpoint;
import com.sun.jbi.engine.aspect.utils.AspectSEUtil;

/**
 * Factory class for aspectse endpoint, the generation of end point depends on
 * the type of aspect node in the aspect map entry
 *
 * @author Sujit Biswas
 *
 */
public class AspectSEEndpointFactory implements EndpointFactory {



	private static Logger logger = Logger
			.getLogger(AspectSEEndpointFactory.class.getName());

	public Endpoint createEndpoint(EndpointInfo info, String serviceUnitName,
			String rootPath) throws DeploymentException {

		AspectSEEndpoint ept = null;

		try {
			if (logger.isLoggable(Level.FINE)) {
				logger.fine("Initializing endpoint: " + String.valueOf(info));
			}

			File suDescriptorFile = new File(rootPath, AspectMap.DESCRIPTOR_FILE);
			// new File(serviceUnitRootPath + File.separator + META_INF_DIR,
			// JBI_DESC_FILE_NAME);
			if (!suDescriptorFile.exists()) {
				throw AspectSEUtil.error("Aspect SU descriptor file does not exist: "
						+ suDescriptorFile.getAbsolutePath(), null);
			}
			LinkedList<AspectSEEndpoint> chain = parseAspectMapForEndpoints(
					suDescriptorFile, info);
			ept = chain.getFirst();
			ept.setServiceUnitName(serviceUnitName);
			ept.setSURootPath(rootPath);

			formChaining(chain);
			initAllEndpoints(chain);
			if (logger.isLoggable(Level.FINE)) {
				logger.fine("Endpoint initialization complete: "
						+ String.valueOf(info));
			}
		} catch (Exception e) {
			throw AspectSEUtil.error("Unexpected error parsing service unit descriptor: "
					+ e.getMessage(), e);
		}

		return ept;
	}

	private Element getMatchingRootElement(Document doc, EndpointInfo info) {
		Element root = doc.getDocumentElement();

		NodeList nl = root.getElementsByTagName(AspectMap.ASPECT);
		Element rootElement = findRootElement(info, nl);
		if (rootElement != null) {
			return rootElement;
		}

		return rootElement;
	}

	private Element findRootElement(EndpointInfo info, NodeList nl) {
		Element advice = null;
		Element rootElement = null;
		for (int i = 0; i < nl.getLength(); i++) {
			Element e = (Element) nl.item(i);
			Element input = (Element) e.getElementsByTagName(
					AspectMap.INPUT_ELEMENT).item(0);

			boolean b = false;
			if (info.isProvides()) {
				b = isMatch(input, info);
			} else {
				NodeList outputs = e
						.getElementsByTagName(AspectMap.OUTPUT_ELEMENT);
				for (int j = 0; j < outputs.getLength(); j++) {
					Element output = (Element) outputs.item(j);
					b = isMatch(output, info);
					if (b)
						break;
				}
			}

			if (b) {
				advice = (Element) e.getElementsByTagName(AspectMap.ADVICE)
						.item(0);
				advice.getAttribute(AspectMap.ADVICE_ATTR_TYPE);

				rootElement = e;

				break;
			}
		}
		return rootElement;
	}

	boolean isMatch(Element input, EndpointInfo mInfo) {
		String plinkAttr = input.getAttribute(AspectMap.PARTNERLINK_ATTR); // service
		String portAttr = input.getAttribute(AspectMap.PORTTYPE_ATTR); // interface
		String roleAttr = input.getAttribute(AspectMap.ROLE_NAME_ATTR); // endpoint
		if (plinkAttr == null || roleAttr == null) {
			plinkAttr = input.getAttribute(AspectMap.SERVICE_NAME_ATTR);
			roleAttr = input.getAttribute(AspectMap.PORT_NAME_ATTR);
		}

		return (Util.equals(plinkAttr, AspectSEUtil.convert(mInfo
				.getServiceName()))
				&& Util.equals(portAttr, AspectSEUtil.convert(mInfo
						.getInterfaceName())) && Util.equals(roleAttr, mInfo
				.getEndpointName()));
	}




	private LinkedList<AspectSEEndpoint> parseAspectMapForEndpoints(
			File eipsuDescriptorFile, EndpointInfo info) {
		SortedMap<Integer, AspectSEEndpoint> aspectsMap = new TreeMap<Integer, AspectSEEndpoint>();
		Map<Integer, Element> adviceMap = new HashMap<Integer, Element>();
		try {
			DocumentBuilder builder = DocumentBuilderFactory.newInstance()
					.newDocumentBuilder();
			Document doc = builder.parse(eipsuDescriptorFile);
			Element aspect = getMatchingRootElement(doc, info);
			NodeList advices = aspect.getElementsByTagName(AspectMap.ADVICE);
			for (int j = 0; j < advices.getLength(); j++) {
				Element advice = (Element) advices.item(j);
				int orderNo = Integer.parseInt(advice
						.getAttribute(AspectMap.ORDER));
				adviceMap.put(orderNo, advice);
				aspectsMap
						.put(orderNo, getEndpointForType(advice, info, false));
			}

		} catch (IOException ex) {
			ex.printStackTrace();
		} catch (ParserConfigurationException ex) {
			ex.printStackTrace();
		} catch (SAXException ex) {
			ex.printStackTrace();
		}

		// now create an instance of AspectSEEndpoint with
		// info for the first aspect in the list.
		Element advice = adviceMap.get(aspectsMap.firstKey());
		aspectsMap.put(aspectsMap.firstKey(), getEndpointForType(advice, info,
				true));

		// create a linked list using the sorted map.
		LinkedList<AspectSEEndpoint> list = new LinkedList<AspectSEEndpoint>(
				aspectsMap.values());
		return list;
	}

	// TODO this should be done using a config file
	private AspectSEEndpoint getEndpointForType(Element advice,
			EndpointInfo info, boolean useInfo) {
		AspectSEEndpoint ept = null;
		String type = advice.getAttribute("type");

		// only the first node in the chain make use of the info

		if (type.equals(AspectMap.ADVICE_LOGGING)) {
			ept = new AspectSELoggingEndpoint(useInfo ? info : null);
		}
		if (type.equals(AspectMap.ADVICE_AUTO_RECONNECT)) {
			ept = new AspectSEAutoReconnectEndpoint(useInfo ? info : null);
		}
		if (type.equals(AspectMap.ADVICE_CACHE)) {
			ept = new AspectSECacheEndpoint(useInfo ? info : null);
		}
		if (type.equals(AspectMap.ADVICE_CONTENT_BASED_ROUTING)) {
			ept = new AspectSEContentBasedRoutingEndpoint(useInfo ? info : null);
		}
		if (type.equals(AspectMap.ADVICE_MESSAGE_TRACKING)) {
			ept = new AspectSEMessageTrackingEndpoint(useInfo ? info : null);
		}
		if (type.equals(AspectMap.ADVICE_QUEUEING)) {
			ept = new AspectSEQueuingEndpoint(useInfo ? info : null);
		}
		if (type.equals(AspectMap.ADVICE_TEE)) {
			ept = new AspectSETeeEndpoint(useInfo ? info : null);
		}
		if (type.equals(AspectMap.ADVICE_THROTTLING)) {
			ept = new AspectSEThrottlingEndpoint(useInfo ? info : null);
		}

		ept.setRoot((Element) advice.getParentNode());
		ept.setAdvice(advice);
		return ept;
	}

	private void formChaining(LinkedList<AspectSEEndpoint> chain) {
		Iterator it = chain.iterator();
		AspectSEEndpoint prev = null;
		while (it.hasNext()) {
			AspectSEEndpoint endPt = (AspectSEEndpoint) it.next();
			endPt.setPrevious(prev);
			if (prev != null) {
				prev.setNext(endPt);
			}
			prev = endPt;
		}
		// set the next endpoint as null for the last one in the chain.
		prev.setNext(null);
	}


	private void initAllEndpoints(LinkedList chain) {

		String suName = ((AspectSEEndpoint) chain.getFirst())
				.getServiceUnitName();
		String rootPath = ((AspectSEEndpoint) chain.getFirst()).getSURootPath();
		Iterator it = chain.iterator();
		while (it.hasNext()) {
			AspectSEEndpoint endPt = (AspectSEEndpoint) it.next();
			endPt.setServiceUnitName(suName);
			endPt.setSURootPath(rootPath);
			endPt.init();
		}
	}
}
