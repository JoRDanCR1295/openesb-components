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
 * @(#)JbiParser.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.descriptor.parsers;

import javax.jbi.management.DeploymentException;
import javax.xml.namespace.NamespaceContext;
import javax.xml.xpath.XPath;

import org.w3c.dom.Element;

/**
 * Defines contract for JBI descriptor parsers.
 * @author Kevan Simpson
 */
public interface JbiParser<T> {
	/**
	 * Fetches the {@link XPath} evaluator for this parser.
	 * @return the XPath evaluator for this parser.
	 */
	public XPath getXPath();
	
	/**
	 * Fetches the {@link NamespaceContext} for this parser, 
	 * which is not guaranteed to be used by this parser's XPath evaluator.
	 * @return the <code>NamespaceContext</code> for this parser.
	 */
	public JbiNamespaceContext getNSContext();
	
	/**
	 * Parses the specified element into a JBI descriptor model.
	 * @param elem The specified element.
	 * @return An object representing some part of a JBI descriptor.
	 * @throws DeploymentException
	 */
	public T parse(Element elem) throws DeploymentException;
}
