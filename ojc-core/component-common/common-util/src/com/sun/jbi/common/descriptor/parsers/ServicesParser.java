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
 * @(#)ServicesParser.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers;

import javax.jbi.management.DeploymentException;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.model.Services;
import com.sun.jbi.common.util.I18n;

/**
 * A {@link JbiParser} to read service unit descriptors.
 * @author Kevan Simpson
 */
public class ServicesParser extends AbstractJbiParser<Services> {
	/** @see com.sun.jbi.common.descriptor.parsers.JbiParser#parse(org.w3c.dom.Element) */
	public Services parse(Element elem) throws DeploymentException {
		if (elem != null) {
			// expects service unit descriptor
			try {
				NodeList consumes = (NodeList) getXPath().evaluate(
						build("//", null, JbiDescriptor.CONSUMES_ELEM), 
						elem, XPathConstants.NODESET);
				int len = consumes.getLength();
				EndpointInfo[] consInfo = new EndpointInfo[len];
				for (int i = 0; i < len; i++) {
					Element entry = (Element) consumes.item(i);
					consInfo[i] = resolveEndpoint(entry, false);
				}

				NodeList provides = (NodeList) getXPath().evaluate(
						build("//", null, JbiDescriptor.PROVIDES_ELEM), 
						elem, XPathConstants.NODESET);

				len = provides.getLength();
				EndpointInfo[] provInfo = new EndpointInfo[len];
				for (int i = 0; i < len; i++) {
					Element entry = (Element) provides.item(i);
					provInfo[i] = resolveEndpoint(entry, true);
				}
				
				return new Services(provInfo, consInfo);
			}
			catch (XPathExpressionException xpee) {
				throw error(xpee, I18n.loc(
						"UTIL-6002: Failed to parse service unit descriptor: {0}",
						xpee.getMessage()));
			}
		}
		
		return null;
	}
}
