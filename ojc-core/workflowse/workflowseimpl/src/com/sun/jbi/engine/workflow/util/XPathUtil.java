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
 * @(#)$Id: XPathUtil.java,v 1.11 2010/02/15 19:24:45 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.namespace.QName;

import org.apache.commons.jxpath.AbstractFactory;
import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.JXPathContextFactory;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.Variables;
import org.apache.commons.jxpath.ri.JXPathContextFactoryReferenceImpl;
import org.apache.commons.jxpath.ri.JXPathContextReferenceImpl;
import org.apache.commons.jxpath.ri.NamespaceResolver;
import org.apache.commons.jxpath.ri.model.beans.BeanPointer;
import org.apache.commons.jxpath.ri.model.dom.DOMAttributePointer;
import org.apache.commons.jxpath.ri.model.dom.DOMNodePointer;
import org.apache.xmlbeans.SchemaField;
import org.apache.xmlbeans.SchemaType;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;


import com.sun.jbi.engine.workflow.process.LDAPConfig;
import com.sun.jbi.engine.workflow.runtime.model.RuntimeTask;
import com.sun.jbi.engine.workflow.runtime.model.impl.RuntimeVariablesImpl;
import com.sun.jbi.engine.workflow.xpath.ExpressionInfo;
import com.sun.jbi.engine.workflow.xpath.LdapXpathContext;
import com.sun.jbi.engine.workflow.xpath.XPath2Functions;
import com.sun.jbi.engine.workflow.xpath.XPathExpressionVisitor;
import com.sun.jbi.engine.workflow.xpath.XpathException;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperParser;
import com.sun.jbi.workflow.model.RuntimeVariables;
import com.sun.jbi.workflow.model.Task;
import com.sun.xpath.AbstractXPathModelHelper;
import com.sun.xpath.XPathExpression;
import com.sun.xpath.XPathModel;

public class XPathUtil {
    
    private static final String TASK_INPUT_WRAPPER = "taskInputWrapper";

	private static final String XSD_NS = "http://www.w3.org/2001/XMLSchema";

	/**
	 * JXPathContext Factory: be careful to use this instead of
	 * JXPathContext.newContext(). That creates a context factory internally,
	 * which, in turn, reads from the file system. By creating our own factory
	 * we only pay that penalty once.
	 */
	private static final JXPathContextFactory JXPATH_FACTORY = new WLMXPathContextFactory();

	private static XPath2Functions xpathFunctions = new XPath2Functions();

	private static final Logger LOGGER = Messages.getLogger(XPathUtil.class);

	public static JXPathContext newJXPathContextFromTask(
			RuntimeTask runtimeTask, AbstractFactory factory)  {
		JXPathContext jxpathContext = JXPATH_FACTORY.newContext(null, null);
		jxpathContext.setLenient(true);
		jxpathContext.setFunctions(xpathFunctions);
        //TODO: change to get the config from EngineContext
        try {
            if (runtimeTask != null) {
                LDAPConfig ldapConfig = runtimeTask.getLDAPConfig ();
                ((LdapXpathContext) jxpathContext).setLdapConfig(ldapConfig);
            } else {
                ((LdapXpathContext) jxpathContext).setLdapConfig(new LDAPConfig ());
            }
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

		if (runtimeTask != null) {
			jxpathContext.setVariables(runtimeTask.getRuntimeVariables());
			// Register namespaces
			NamespaceResolver resolver = new WLMNamespaceResolver(Task.class
					.cast(runtimeTask.getTaskMeta()));
			WLMXPathContextImpl.class.cast(jxpathContext).setNamespaceResolver(
					resolver);
		}

		if (factory != null) {
			jxpathContext.setFactory(factory);
		}
		return jxpathContext;
	}
    

	public static JXPathContext newJXPathContextFromVar(Variables vars,
			AbstractFactory factory) {
		JXPathContext jxpathContext = JXPATH_FACTORY.newContext(null, null);
		jxpathContext.setLenient(true);
		jxpathContext.setFunctions(xpathFunctions);

        try {
            ((LdapXpathContext) jxpathContext).setLdapConfig(new LDAPConfig ());
        } catch (Exception e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
		if (vars != null) {
			jxpathContext.setVariables(vars);
		}

		if (factory != null) {
			jxpathContext.setFactory(factory);
		}
		return jxpathContext;
	}

	public static List<ExpressionInfo> parseExprForVariables(String expr)
			throws Exception {
		List varInfoSet = new ArrayList<ExpressionInfo>();

		List xpathExpressionsList = getXPathExpressionList(expr);

		for (Iterator iterator = xpathExpressionsList.iterator(); iterator
				.hasNext();) {

			String expr1 = (String) iterator.next();
			varInfoSet.addAll(getVariables(expr1));
		}
		return varInfoSet;
	}

	/**
	 * Evaluate the expression to a pointer
	 * 
	 * @param expr
	 * @param jxpathContext
	 * @return
	 * @throws Exception
	 */
	public static Object evaluateExpression(String expr,
			JXPathContext jxpathContext) throws Exception {
		Iterator it = jxpathContext.iteratePointers(expr);
		Object fromTemp = null;
		int i =0;

		for (i = 0; it.hasNext(); i++) {
			if (i == 0) {

				// Maybe only 1 node is selected
				Pointer sourcePtr = (Pointer) it.next();
				if (sourcePtr instanceof BeanPointer
						|| sourcePtr instanceof DOMAttributePointer) {
					fromTemp = sourcePtr.getValue();
				} else if (sourcePtr instanceof DOMNodePointer){
					if (isLeaf(sourcePtr.getNode())) {
						fromTemp =  sourcePtr.getValue();
					} 
					else {
						fromTemp = sourcePtr.getNode();
					}
				} else {
					fromTemp = sourcePtr.getNode();
				}
				
			} else {
				// A collection is selected, return the Iterator as-is
				it.next();
				fromTemp = null;

			}

		}
		if (fromTemp == null) {
			return  jxpathContext.iteratePointers(expr);
		}
		return fromTemp;

	}

	private static boolean isLeaf(Object node) {
		// TODO Auto-generated method stub
		Node xmlNode = (Node) node;
		NodeList nodeList = xmlNode.getChildNodes();
		for (int i=0 ; i < nodeList.getLength(); i++) {
			if (nodeList.item(i) instanceof Element) {
				return false;
			}
		}
		return true;
	}

	private static Set<ExpressionInfo> getVariables(String expr) {
		Set<ExpressionInfo> varInfoSet = new HashSet();
		char exprArray[] = expr.toCharArray();
		int exprLength = exprArray.length;
		boolean inVariable = false;
		int dotPos = -1;
		int varStartPos = -1;
		int cursor = 0;

		for (; cursor < exprLength; cursor++) {
			if (inVariable) {
				if (isPartVariable(exprArray[cursor])) {
					dotPos = cursor;
				} else if (isVariableEndIndicator(exprArray[cursor])) {
					inVariable = false;
					ExpressionInfo varInfo = new ExpressionInfo(expr,
							varStartPos, dotPos, cursor);
					varInfoSet.add(varInfo);
					dotPos = -1;
				}
			} else {
				if (isVariableBeginIndicator(exprArray[cursor])) {
					inVariable = true;
					varStartPos = cursor + 1;
				}
			}
		}
		// for a variable defined in from/to construct, just by itself
		if (inVariable) {
			ExpressionInfo varInfo = new ExpressionInfo(expr, varStartPos,
					dotPos, cursor);
			varInfoSet.add(varInfo);
		}
		return varInfoSet;
	}

	private static boolean isPartVariable(char cursorChar) {
		if (cursorChar == '.') {
			return true;
		}
		return false;
	}

	public static List getXPathExpressionList(String expr) throws Exception {

		List list = new ArrayList();

		try {
			XPathModel model = AbstractXPathModelHelper.getInstance()
					.newXPathModel();
			XPathExpression xPathExpr = model.parseXpathExpression(expr);

			if (xPathExpr != null) {
				XPathExpressionVisitor visitor = new XPathExpressionVisitor();
				xPathExpr.accept(visitor);
				list = visitor.getExpressionList();
			}
		} catch (Exception e) {
			LOGGER.log(Level.WARNING,
					I18n.loc("WLM-6072: Exception during parsing xpath expression :{0}", expr), e);
			throw new Exception(e);
		}
		return list;
	}

	private static boolean isVariableBeginIndicator(char cursorChar) {
		if (cursorChar == '$') {
			return true;
		}
		return false;
	}

	private static boolean isVariableEndIndicator(char cursorChar) {
		if ((cursorChar == ' ') || (cursorChar == '/') || (cursorChar == '[')
				|| (cursorChar == ']') || (cursorChar == ',')
				|| (cursorChar == '(') || (cursorChar == ')')) {
			return true;
		}
		return false;
	}

	private static class WLMXPathContextFactory extends
			JXPathContextFactoryReferenceImpl {
		public JXPathContext newContext(JXPathContext parentContext,
				Object contextBean) {
			return new WLMXPathContextImpl(parentContext, contextBean);
		}
	}

	static class WLMXPathContextImpl extends JXPathContextReferenceImpl implements LdapXpathContext {
        
        private LDAPConfig mLdapConfig = null;
        
		WLMXPathContextImpl(JXPathContext parentContext, Object contextBean) {
			super(parentContext, contextBean);
		}

		public void setNamespaceResolver(NamespaceResolver resolver) {
			namespaceResolver = resolver;
		}

        public LDAPConfig getLdapConfig() {
            // TODO Auto-generated method stub
            return mLdapConfig;
        }

        public void setLdapConfig(LDAPConfig config) {
            // TODO Auto-generated method stub
            mLdapConfig = config;
        }
	}

	static class WLMNamespaceResolver extends NamespaceResolver {

		WLMNamespaceResolver(Map<String, String> nsMap) {

			if (nsMap instanceof HashMap) {
				namespaceMap = (HashMap) nsMap;
			} else {
				namespaceMap = new HashMap(nsMap);
			}
		}

		WLMNamespaceResolver(Task task) {
			this(task.getTotalNamespaces());
		}
	}

	public static RuntimeVariables setPartsAsVariables(String varName, Element source,
			Message msg, RuntimeVariables runtimeVars)
			throws Exception {
		try {

			Document normalDoc = source.getOwnerDocument();

			Element normalRoot = normalDoc.getDocumentElement();

			WrapperParser wrapperParser = HelperFactory.createParser();

			wrapperParser.parse(normalDoc, msg);

			Map parts = msg.getParts();
			Iterator it = parts.values().iterator();
			while (it.hasNext()) {
				Part part = (Part) it.next();

				QName elementQName = part.getElementName();
				QName typeQName = part.getTypeName();
				RuntimeVariables.VariableType varType = null;

				if (elementQName == null && typeQName == null) {
					String msgEx = I18n
							.loc("WLM-6073: Missing element or type on Message Part");
					throw new MessagingException(msgEx);
				}
				Element element = null;
				NodeList unwrappedList = wrapperParser.getPartNodes(part
						.getName());
				if (elementQName != null) {
					for (int j = 0; j < unwrappedList.getLength(); j++) {
						Node unwrapped = (Node) unwrappedList.item(j);
						if (unwrapped.getNodeType() == Node.ELEMENT_NODE
								&& unwrapped.getLocalName() != null
								&& unwrapped.getLocalName().equals(
										elementQName.getLocalPart())) {
							element = (Element) unwrapped;
							runtimeVars.declareVariable(varName + "." + part.getName(), element, RuntimeVariables.VariableType.Element);
						}
					}
				} else if (typeQName != null) {
					if (isSimpleType(typeQName)) {
						StringBuffer buffer = new StringBuffer();
						if (unwrappedList.getLength() > 0) {
							for (int i = 0; i < unwrappedList.getLength(); i++) {
								Node unwrapped = (Node) unwrappedList.item(i);
								if (unwrapped.getNodeType() == Node.TEXT_NODE) {
									Text text = (Text) unwrapped;
									buffer.append(text.getNodeValue());
								}
							}

						}
						runtimeVars.declareVariable(varName + "." + part.getName(), buffer
								.toString().trim(), RuntimeVariables.VariableType.Text);						
					}else {
                           List<Node> childrenNodes = new ArrayList<Node> ();
                            for (int i = 0; i < unwrappedList.getLength(); i++) {
                                Node node = unwrappedList.item(i);
                                childrenNodes.add(node);
                            }
                            
                            for (int i = 0; i <childrenNodes.size(); i ++) {     
                                Node node = childrenNodes.get(i);
                                if (element == null) {
                                    element = (Element) node.getOwnerDocument().importNode(node.getOwnerDocument().getDocumentElement(), false);
                                }    
                                element.appendChild(node);
                            }
                            runtimeVars.declareVariable(varName + "." + part.getName(), element, RuntimeVariables.VariableType.Element);
                        }               
                    }
			}
		} catch (Throwable th) {
			String errormsg = I18n.loc("WLM-6071: Failed to normalize the message");
			throw new XpathException("XPathUtil.FAILEDTONORMALIZE",
					errormsg, th);
		}
		return runtimeVars;
	}

	private static boolean isSimpleType(QName typeQName) {
		// TODO Auto-generated method stub
		if (typeQName.getNamespaceURI().equals(XSD_NS)) {
			return true;
		}
		return false;
	}

	public static void updateNode(Node targetNode, Object fromVal) {

		if (fromVal instanceof Iterator) {
			Pointer sourcePtr = (Pointer) ((Iterator) fromVal).next();
			if (sourcePtr instanceof BeanPointer
					|| sourcePtr instanceof DOMAttributePointer) {
				fromVal = sourcePtr.getValue();
			} else {
				fromVal = sourcePtr.getNode();
			}
		}

		// remove child elements, it is prerequisite
		removeChildNodes(targetNode);

		// add value node as a child node
		if (fromVal instanceof Text) {
			Node tmpNode = targetNode.getOwnerDocument().importNode(
					(Node) fromVal, true);
			targetNode.appendChild(tmpNode);
		} else if (fromVal instanceof Node) {

			Document targetDoc = targetNode.getOwnerDocument();

			NodeList childNodes = ((Node) fromVal).getChildNodes();
			int noChildren = childNodes.getLength();
			for (int i = 0; i < noChildren; i++) {
				Node child = childNodes.item(i);
				if (child != null) {
					Node replacingNode = targetDoc.importNode(child, true);
					targetNode.appendChild(replacingNode);
				}
			}
		} else {
			// value is literal
			// (either a constant or xpath expression resulted in constant)
			setNodeValue(targetNode, fromVal);
		}
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
		boolean isTargetNodeFloat = false;
		if (value instanceof Double) {
			Object schemaType = targetNode.getUserData("schemaType");
			if (schemaType != null) {
				SchemaType schType = ((SchemaField) schemaType).getType();
				if ((schType.getBuiltinTypeCode() == SchemaType.BTC_DECIMAL)
						|| (schType.getBuiltinTypeCode() == SchemaType.BTC_DOUBLE)
						|| (schType.getBuiltinTypeCode() == SchemaType.BTC_FLOAT)) {
					isTargetNodeFloat = true;
				}
			}
			// remove trailing zero
			if ((!isTargetNodeFloat)
					&& ((((Double) value).doubleValue() - Math
							.floor((Double) value)) == 0)) {
				value = ((Double) value).longValue();
			}
		}
		Text txt = targetNode.getOwnerDocument().createTextNode(
				value.toString());
		targetNode.appendChild(txt);
	}

	public static class DOMFactory extends AbstractFactory {
		/**
		 * creates DOM object
		 * 
		 * @param context
		 *            jxpath context
		 * @param pointer
		 *            node pointer
		 * @param parent
		 *            parent object
		 * @param name
		 *            object name
		 * @param index
		 *            object index
		 * @return boolean: on successful execution, returns true; otherwise,
		 *         returns false
		 */
		public boolean createObject(JXPathContext context, Pointer pointer,
				Object parent, String name, int index) {
			addDOMElement(context, (Node) parent, index, name);
			return true;
		}

		/**
		 * declares variable
		 * 
		 * @param context
		 *            jxpath context
		 * @param name
		 *            variable name
		 * @return boolean: on successful execution, returns true; otherwise,
		 *         returns false
		 */
		public boolean declareVariable(JXPathContext context, String name) {
			return false;
		}

		/**
		 * adds DOM element
		 * 
		 * @param parent
		 *            parent node
		 * @param index
		 *            index position
		 * @param tag
		 *            node tag
		 */
		private void addDOMElement(JXPathContext context, Node parent,
				int index, String tag) {
			String tagToCreate = null;
			String nodeNS = null;
			String nodeLocalName = null;

			int position = tag.indexOf(':');
			Document doc = parent.getOwnerDocument();
			/*******************************************************************
			 * if queryPrefix is not null, node will be created is either
			 * corresponds to global element or local element with schema
			 * defined with elementFormDefault= "unqualified"
			 */
			if (position > 0l) {
				String queryPrefix = tag.substring(0, position);
				nodeLocalName = tag.substring(position + 1, tag.length());
				// transform query prefix to document prefix. they may not be
				// same
				nodeNS = context.getNamespaceURI(queryPrefix);
				String nodePrefix = parent.lookupPrefix(nodeNS);
				if (nodePrefix == null) {
					// calculate unique prefix
					int counter = 2;
					nodePrefix = queryPrefix;
					while (parent.lookupPrefix(nodePrefix) != null) {
						nodePrefix = queryPrefix + counter++;
					}
				}
				tagToCreate = nodePrefix + ":" + nodeLocalName;
			} else {
				nodeLocalName = tag;
				tagToCreate = nodeLocalName;
				boolean qualifed = "qualified".equals(doc.getDocumentElement()
						.getUserData("qualification"));
				if (qualifed) {
					nodeNS = parent.getNamespaceURI();
				}
			}

			// check if this node already exists
			Node child = parent.getFirstChild();
			int count = 0;
			while (child != null) {
				String childNodeNS = child.getNamespaceURI();
				String childNodeLocalName = child.getLocalName();
				if (areEqual(nodeNS, childNodeNS)
						&& areEqual(nodeLocalName, childNodeLocalName)) {
					count++;
				}
				child = child.getNextSibling();
			}
			// Keep inserting new elements until we have index + 1 of them
			while (count <= index) {
				Node newElement = null;
				newElement = doc.createElementNS(nodeNS, tagToCreate);
				parent.appendChild(newElement);
				count++;
			}
		}
	}

	/**
	 * Return either the iterator if it points to a collection or the node/value
	 * that the single pointer points to
	 * 
	 * @param fromVal
	 * @return Either a node/value or the iterator if it points to a collection
	 */
	public static Object getSingleValueObject(Object fromVal) {
		Object fromTemp = null;
		if (fromVal instanceof Pointer) {
			Pointer sourcePtr = (Pointer) fromVal;
			if (sourcePtr instanceof BeanPointer
					|| sourcePtr instanceof DOMAttributePointer) {
				fromTemp = sourcePtr.getValue();
			} else {
				fromTemp = sourcePtr.getNode();
			}
		}
		if (fromTemp != null) {
			fromVal = fromTemp;
		}
		return fromVal;
	}

	/**
	 * Return a string deliminated with the delim param if a collection of value
	 * is contained
	 * 
	 * @param ptr
	 * @param delim
	 * @return
	 */
	public static String getStringValue(Object ptr, String delim) {
		

		Object temp = getSingleValueObject(ptr);
		String returnStr = null;
	
		StringBuffer buffer = new StringBuffer();
		if (temp instanceof Text) {
			returnStr = ((Text) temp).getNodeValue();
		} else if (temp instanceof Element) {
			NodeList children = ((Element) temp).getChildNodes();
			for (int i = 0; i < children.getLength(); i++) {
				Node child = children.item(i);
				if (child.getNodeType() == Node.TEXT_NODE) {
					buffer.append(((Text) child).getNodeValue());
				}
				if (i != children.getLength() - 1) {
					buffer.append(delim);
				}
			}
			return buffer.toString().trim();
		} else if (temp instanceof Iterator) {
			boolean isFirst = true;
			for (; ((Iterator) temp).hasNext();) {
				if (isFirst) {
					buffer.append(getStringValue(((Iterator) temp).next(),
							delim));
					isFirst = false;
				} else {
					buffer.append(delim);
					buffer.append(getStringValue(((Iterator) temp).next(),
							delim));
				}
			}
			return buffer.toString().trim();
		} else {
			returnStr = temp.toString();
		}

		return returnStr;
	}

	/**
	 * Test if two objects are equal.
	 * 
	 * @param s1
	 *            First object.
	 * @param s2
	 *            Second object.
	 * 
	 * @return <code>true</code> if objects are equal.
	 */
	private static boolean areEqual(Object o1, Object o2) {
		return ((o1 == null) ? (o2 == null) : o1.equals(o2));
	}

}
