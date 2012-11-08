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
 * @(#)DescriptorHandler.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.descriptor.parsers;

import javax.jbi.management.DeploymentException;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.common.descriptor.parsers.AbstractJbiParser;
import com.sun.jbi.common.qos.redelivery.Redelivery;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig;
import com.sun.jbi.common.qos.redelivery.Redirect;
import com.sun.jbi.common.qos.redelivery.RedeliveryConfig.Failure;
import com.sun.jbi.common.util.I18n;
import com.sun.jbi.common.util.Util;

/**
 * Parses JBI endpoints in a service unit descriptor.
 * @author Kevan Simpson
 */
public class RedeliveryParser extends AbstractJbiParser<RedeliveryConfig> {
	public static final String REDELIVERY_ELEM 	   = "redelivery";
	public static final String MAX_ATTEMPTS_ATTR   = "maxAttempts";
	public static final String WAIT_TIME_ATTR 	   = "waitTime";
	public static final String ON_FAILURE_ELEM	   = "on-failure";
	public static final String REDIRECT_ELEM	   = "redirect";
	public static final String SUSPEND_ELEM		   = "suspend";
	public static final String DELETE_ELEM		   = "delete";
    public static final String ERROR_ELEM          = "error";
	public static final String OPERATION_ATTR      = "operation";
	
	private static final String RD_PREFIX = "rd";
	

	public RedeliveryParser(QosConnectionParser cp) {
		super(cp);
		getNSContext().addNamespace(RD_PREFIX, Redelivery.REDELIVERY_NS);
	}
	
	/** @see com.sun.jbi.common.descriptor.parsers.JbiParser#parse(org.w3c.dom.Element) */
	public RedeliveryConfig parse(Element elem) throws DeploymentException {
		try {
			NodeList list = elem.getElementsByTagNameNS(
					Redelivery.REDELIVERY_NS, REDELIVERY_ELEM);
			if (list != null && list.getLength() == 1) {
				Element rd = (Element) list.item(0);
				int maxAttempts = Util.parseInt(rd.getAttribute(MAX_ATTEMPTS_ATTR), 0);
				long waitTime = Util.parseLong(rd.getAttribute(WAIT_TIME_ATTR), 0);
				Redirect redirect = null;
				Failure failure = Failure.error;
				
				// on-failure
				for (Failure f : Failure.values()) {
					Element fail = hasFailure(rd, f);
					if (fail != null) {
						failure = f;
						if (failure == Failure.redirect) {
							redirect = new Redirect(
									resolveEndpoint(fail, false), 
									fail.getAttribute(OPERATION_ATTR));
						}
						break;
					}
				}
				
				return Redelivery.createRedeliveryConfig(
						maxAttempts, waitTime, failure, redirect);
			}
		}
		catch (Exception e) {
			throw error(e, I18n.loc(
					"QOS-6061: Failed to parse systemic redelivery configuration: {0}",
					e.getMessage()));

		}
		
		return null;
	}

	protected Element hasFailure(Element rd, Failure failure) throws XPathExpressionException {
		return (Element) getXPath().evaluate(
				build("./", RD_PREFIX, ":", ON_FAILURE_ELEM, "/",
						RD_PREFIX, ":", failure.toString()), 
				rd, XPathConstants.NODE);
	}
}
