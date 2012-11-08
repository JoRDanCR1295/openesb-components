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
 * @(#)$Id: CopyUnitImpl.java,v 1.5 2010/02/15 19:24:13 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.runtime.model.impl;

import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.model.beans.BeanPointer;
import org.apache.commons.jxpath.ri.model.dom.DOMAttributePointer;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.sun.jbi.engine.workflow.runtime.model.VariableCopy;
import com.sun.jbi.engine.workflow.util.I18n;
import com.sun.jbi.engine.workflow.util.Util;
import com.sun.jbi.engine.workflow.util.XPathUtil;
import com.sun.jbi.engine.workflow.util.XmlUtil;
import com.sun.jbi.engine.workflow.xpath.ExpressionInfo;
import com.sun.jbi.engine.workflow.xpath.XpathException;
import com.sun.jbi.workflow.model.Copy;

public class CopyUnitImpl implements VariableCopy {

	private static final String TEXT_NODE = "text()";

	private static final char PREDICATE_END = ']';

	private static final char PREDICATE_BEGIN = '[';

	private Copy mCopy;

	private JXPathContext mContext;

	private static Logger mLogger = Logger.getLogger(CopyUnitImpl.class
			.getName());

	public CopyUnitImpl(Copy copy, JXPathContext context) {
		mCopy = copy;
		mContext = context;
	}

	public CopyUnitImpl() {

	}

	private Object evaluateFrom() throws Exception {
		String fromExp = mCopy.getFromExpr();
		return XPathUtil.evaluateExpression(fromExp, mContext);
	}

	public ExpressionInfo getToExpression(String toQuery) throws Exception {

		return getToExpression (toQuery, mContext);
	}

	public ExpressionInfo getToExpression(String toQuery,
			JXPathContext jxpathContext) throws Exception {
		List<ExpressionInfo> expressions = XPathUtil
				.parseExprForVariables(toQuery);
		if (expressions.size() != 1) {
			throw new XpathException(I18n.loc(
					"WLM-6065: The query : {0} is not valid", toQuery));
		}
		ExpressionInfo exp = expressions.iterator().next();
		String xpathVar = exp.getXpathVariableName();
		// If not declared, then only var by itself is accepted, no partName or
		// Query is allowed
		if (!jxpathContext.getVariables().isDeclaredVariable(xpathVar)) {
			if (!Util.isEmpty(exp.getXpathQuery())) {
				throw new XpathException(I18n.loc(
						"WLM-6065: The query : {0} is not valid", toQuery));
			}
		}
		return exp;
	}

	public void executeTo(ExpressionInfo toExpr, JXPathContext jxpathContext,
			Object fromVal) throws Exception {
		String xpathVar = toExpr.getXpathVariableName();
		String part = toExpr.getPartName();
		String xpathQuery = toExpr.getXpathQuery();
		if (Util.isEmpty(xpathQuery)) {
			// It is either $var or $var.part, so update it with a new value
			Object toUpdate = updateNewValue(fromVal);
			// toUpdate may be an Iterator pointing to a collection
			jxpathContext.getVariables().declareVariable(xpathVar, toUpdate);
		} else {
			String targetQuery = toExpr.getPath();
			if (xpathQuery.endsWith(TEXT_NODE)) {
				int position = xpathQuery.indexOf(TEXT_NODE);
				targetQuery = xpathQuery.substring(0, position - 1);
			}
			Pointer targetPtr = jxpathContext.createPath(targetQuery);
			if (fromVal instanceof Node) {
				/*
				 * This happens for the form <from variable="NCName
				 * "part="NCName"/> in case of WSDL Messages and for the form
				 * <from variable="NCName"/> in case of variables defined using
				 * XSD types
				 */
				updateNode((Node) targetPtr.getNode(), fromVal);

			} else if ((fromVal instanceof String)
					|| (fromVal instanceof Number)
					|| (fromVal instanceof Boolean)) {
				// Used for the form using literals -
				// <from><literal>literal value</literal></from>
				targetPtr.setValue(fromVal);

			} else if ((targetPtr instanceof DOMAttributePointer)) {
				/*
				 * When target is an attribute. Only one attribute can be
				 * selected by using the form <to
				 * queryLanguage="anyURI"?>$var.part/query</to>, so we get only
				 * the first value from the fromVal and set it to the target
				 * attribute.
				 */
				Pointer sourcePtr = (Pointer) ((Iterator) fromVal).next();
				/*
				 * The source can be an DOMAttributePointer, Object (string,
				 * etc), or a Node represent an element with simple context.
				 * sourcePtr.getValue().toString()) will work for all three
				 * cases.
				 */
				targetPtr.setValue(sourcePtr.getValue());

			} else {
				/*
				 * For the variants of the form <from
				 * expressionLanguage="anyURI"?>expression</from>. This
				 * expression could result in multiple nodes, so we iterator and
				 * set the values on the target multiple times (repeating nodes)
				 */
				Iterator sourcePtrIterator = (Iterator) fromVal;

				for (int predicatePos = 1; sourcePtrIterator.hasNext(); predicatePos++) {
					Pointer currentSourcePtr = (Pointer) sourcePtrIterator
							.next();

					if (predicatePos > 1) {
						String targetQueryWithPred = targetQuery
								+ PREDICATE_BEGIN + predicatePos
								+ PREDICATE_END;
						targetPtr = jxpathContext
								.createPath(targetQueryWithPred);
					}

					// Source is an attribute or a result of an expression like
					// concat('var1.part' + 'some string') or
					// var1.part/query + some number
					if (currentSourcePtr instanceof BeanPointer
							|| currentSourcePtr instanceof DOMAttributePointer) {
						targetPtr.setValue(currentSourcePtr.getValue());

					} else {
						// Source as well as target are elements
						Object currentSourcePtrValue = currentSourcePtr
								.getNode();
						updateNode((Node) targetPtr.getNode(),
								currentSourcePtrValue);
					}
				}
			}
		}
	}

	private static Object updateNewValue(Object newObj) throws Exception {
		Document doc = XmlUtil.createDocument(true);
		Object returnObj = updateNode(doc, newObj);
		if (returnObj instanceof Document) {
			returnObj = Document.class.cast(returnObj).getDocumentElement();
		}
		return returnObj;
	}

	private static Object updateNode(Node targetNode, Object fromVal) {

		fromVal = XPathUtil.getSingleValueObject(fromVal);

		if (fromVal instanceof Iterator && targetNode instanceof Document) {
			// If fromVal is an Iterator pointing a collection, return the
			// Iterator object
			return fromVal;
		}
		// remove child elements, it is prerequisite
		removeChildNodes(targetNode);

		// add value node as a child node
		Document ownerDoc = null;
		if (targetNode instanceof Document) {
			ownerDoc = Document.class.cast(targetNode);
		} else {
			ownerDoc = targetNode.getOwnerDocument();
		}
		if (fromVal instanceof Text) {
			if (targetNode instanceof Document) {
				return Text.class.cast(fromVal).getNodeValue();
			}
			Node tmpNode = ownerDoc.importNode((Node) fromVal, true);
			targetNode.appendChild(tmpNode);
		} else if (fromVal instanceof Node) {
			Node replacingNode = ownerDoc.importNode(Node.class.cast(fromVal),
					true);
			targetNode.appendChild(replacingNode);
		} else {

			// value is literal
			// (either a constant or xpath expression resulted in constant)
			if (targetNode instanceof Document) {
				return fromVal;
			}
			setNodeValue(targetNode, fromVal);
		}
		return targetNode;
	}

	private static void removeChildNodes(Node node) {
		NodeList children = node.getChildNodes();
		int count = children.getLength();
		for (int i = count; --i >= 0;) {
			Node child = children.item(i);
			node.removeChild(child);
		}
	}

	private static void setNodeValue(Node targetNode, Object value) {
		Text txt = targetNode.getOwnerDocument().createTextNode(
				value.toString());
		targetNode.appendChild(txt);
	}

	public void doCopy() throws Exception {
		try {
			// TODO Auto-generated method stub
			Object fromVal = evaluateFrom();
			ExpressionInfo expression = getToExpression(mCopy.getToExpr());
			executeTo(expression, mContext, fromVal);
		} catch (Exception e) {
			mLogger.log(Level.WARNING, I18n
					.loc("WLM-6066: Exception occured in copy"), e);
			throw e;
		}

	}

}
