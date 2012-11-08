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
 * @(#)ModelUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.workflow.model.utl;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.transform.dom.DOMSource;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import net.sf.saxon.s9api.ItemType;
import net.sf.saxon.s9api.Processor;
import net.sf.saxon.s9api.XPathCompiler;
import net.sf.saxon.s9api.XPathExecutable;
import net.sf.saxon.s9api.XPathSelector;
import net.sf.saxon.s9api.XdmAtomicValue;
import net.sf.saxon.s9api.XdmItem;
import net.sf.saxon.s9api.XdmNode;

import org.apache.commons.jxpath.JXPathContext;
import org.apache.commons.jxpath.Pointer;
import org.apache.commons.jxpath.ri.model.beans.BeanPointer;
import org.apache.commons.jxpath.ri.model.dom.DOMAttributePointer;
import org.apache.commons.jxpath.util.BasicTypeConverter;
import org.apache.xmlbeans.GDuration;
import org.apache.xmlbeans.XmlDateTime;
import org.apache.xmlbeans.XmlDuration;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.sun.jbi.workflow.model.ModelElement;
import com.sun.jbi.workflow.model.ModelException;
import com.sun.jbi.workflow.model.RuntimeVariables;
import com.sun.jbi.workflow.model.Task;
import com.sun.jbi.workflow.model.XPathInfo;
import com.sun.jbi.workflow.model.RuntimeVariables.VariableType;
import com.sun.jbi.workflow.model.xmlbeans.TDeadlineExpr;
import com.sun.jbi.workflow.model.xmlbeans.TDurationExpr;
import com.sun.jbi.workflow.model.xmlbeans.TExpression;

/**
 * 
 * 
 */
public class ModelUtil {

	/** Creates a new instance of ModelUtil */
    private static Pattern QNAME_PATTERN = Pattern.compile(":");
    public static final String XMLNS = "xmlns";
    public static final String DEFAULT_NS = "DEFAULT_NS";
    private static Pattern p2 = Pattern.compile("[.]*(\\$[^/]*/)([^/$]+)");
    
    private static final Messages MESSAGES = 
        Messages.getMessages(ModelUtil.class);


	public static Date getDeadlineObject(TDeadlineExpr deadlineExp, JXPathContext jxpathContext, Task task)
			throws ModelException {
		Date deadline = null;
		try {
			if (deadlineExp != null) {
				Node domNode = deadlineExp.getDomNode();
				if (domNode != null) {
					String plainVal = getText (domNode);
				//	String modified = getStringValue(jxpathContext, plainVal, "");
                    String modified = getExpressionContent(jxpathContext, deadlineExp, task, "");
					XmlDateTime dateTime = null;
					if (plainVal.trim().equals(modified)) {
						dateTime = XmlDateTime.Factory
								.parse(domNode);
					}else {
						Node newNode = createNewNode (domNode, modified);
						dateTime = XmlDateTime.Factory
							.parse(newNode);
					}
					deadline = dateTime.getDateValue();
				}
			}

		} catch (Exception ex) {
			throw new ModelException(ex);
		}
		return deadline;
	}
    
    public static  String getExpressionContent(JXPathContext context, TExpression expression, Task task, String delim) {
        boolean isXpath20 = false;
        if (expression.getExpressionLanguage() != null && expression.getExpressionLanguage().equals(ModelElement.XPATH20)) {
            isXpath20 = true;
        }
        Node domNode = expression.getDomNode();
        String text = null;
        if (domNode != null) {
            text = ModelUtil.getText(domNode);
            String strVal = null;
            try {
                if (text != null && text.trim().length() > 0) {
                    if (isXpath20) {
                        strVal = ModelUtil.getStringValueXpath20(context, text, task, delim);
                    } else {
                        strVal = ModelUtil.getStringValue(context, text, delim);
                    }
                }
            } catch (Exception e) {
                // TODO Auto-generated catch block
                throw new ModelException(e);
            }
            return strVal;
        }
        return null;
    }

	public static String getText(Node domNode) {
		// TODO Auto-generated method stub
		NodeList children = domNode.getChildNodes();
		StringBuffer buffer = new StringBuffer ();
		for (int i = 0; i<children.getLength(); i++) {
			Node node = children.item(i);
			int type = node.getNodeType();
			switch (type) {
				case Node.CDATA_SECTION_NODE:
					buffer.append(node.getNodeValue());
					break;
				case Node.TEXT_NODE:
					buffer.append(node.getNodeValue());
			}
		}
		return buffer.toString().trim();
	}

	public static GDuration getDurationObject(TDurationExpr durationExp, JXPathContext jxpathContext, Task task)
			throws ModelException {
		GDuration duration = null;
		try {
			if (durationExp != null) {
				Node durationNode = durationExp.getDomNode();

				if (durationNode != null) {
					String plainVal = getText (durationNode);
                    String modified = getExpressionContent(jxpathContext, durationExp, task, "");
					XmlDuration xmlDuration = null;
					if (plainVal.trim().equals(modified)) {
						 xmlDuration = XmlDuration.Factory
								.parse(durationNode);
					}else {
						Node newNode = createNewNode (durationNode, modified);
						 xmlDuration = XmlDuration.Factory
							.parse(newNode);
					}
					duration = xmlDuration.getGDurationValue();
				}
			}
		} catch (Exception ex) {
			throw new ModelException(ex);
		}

		return duration;
	}

	private static Node createNewNode(Node node, String text) {
		// TODO Auto-generated method stub
		Node newNode = node.getOwnerDocument().createElementNS(node.getNamespaceURI(), node.getNodeName());
		newNode.appendChild(node.getOwnerDocument().createTextNode(text));
		return newNode;
	}

	public static Date getDurationDate(GDuration duration)
			throws ModelException {
		Calendar date = Calendar.getInstance();

		try {
			if (duration != null) {
				int year = duration.getYear();
				int month = duration.getMonth();
				int dayOfMonth = duration.getDay();
				int hour = duration.getHour();
				int minute = duration.getMinute();
				int second = duration.getSecond();
				BigDecimal fraction = duration.getFraction();

				date.add(Calendar.YEAR, year);
				date.add(Calendar.MONTH, month);
				date.add(Calendar.DAY_OF_MONTH, dayOfMonth);
				date.add(Calendar.HOUR_OF_DAY, hour);
				date.add(Calendar.MINUTE, minute);
				date.add(Calendar.SECOND, second);
				date.add(Calendar.MILLISECOND, fraction.intValue());

			}
		} catch (Exception ex) {
			throw new ModelException(ex);
		}

		return date.getTime();
	}

	public static XPathInfo generateXpathInfo(ModelElement element) {
		String parentXPath = "";
		Map<String, String> prefixToNS = new HashMap<String, String>();

		if (element.getParent() != null) {
			XPathInfo parentInfo = generateXpathInfo(element.getParent());
			parentXPath = parentInfo.getXPath();
		}

		String xpath = parentXPath + "/"
				+ getLocalXPathName(element, prefixToNS);
		XPathInfo info = new XPathInfo(xpath, prefixToNS);

		return info;
	}

	public static String getLocalXPathName(ModelElement element,
			Map<String, String> prefixToNS) {
		StringBuffer localXPathBuf = new StringBuffer(50);
		QName elementQName = element.getQualifiedName();
		String localName = elementQName.getLocalPart();
		String namespace = elementQName.getNamespaceURI();
		String prefix = getExistingPrefix(prefixToNS, namespace);
		if (prefix == null) {
			prefix = generatePrefix(prefixToNS, namespace);
			prefixToNS.put(prefix, namespace);
		}

		localXPathBuf.append(prefix);
		localXPathBuf.append(":");
		localXPathBuf.append(localName);
		ModelElement parent = element.getParent();
		if (parent != null) {
			List children = parent.getChildren();
			if (children.size() > 1) {
				int total = children.size();
				int ind = 0;
				int j = 0;
				for (int i = 0; i < total; i++) {
					ModelElement child = (ModelElement) children.get(i);
					if (child == element) {
						ind = j;
						j++;
					} else if (child.getQualifiedName().equals(
							element.getQualifiedName())) {
						j++;
					}
				}
				if (j == 1) {
					return localXPathBuf.toString();
				} else {
					localXPathBuf.append("[");
					localXPathBuf.append(ind + 1);
					localXPathBuf.append("]");
				}
			}
		}
		return localXPathBuf.toString();
	}

	public static NodeList executeXPath(String expression, Node source,
			Map<String, String> prefixToNS) throws Exception {
		XPathFactory factory = XPathFactory.newInstance();
		XPath xPath = factory.newXPath();
		if (prefixToNS != null) {
			NamespaceContext context = new DefaultNamespaceContext(prefixToNS);
			xPath.setNamespaceContext(context);
		}
		NodeList nodeList = (NodeList) xPath.evaluate(expression, source,
				XPathConstants.NODESET);

		return nodeList;
	}

	private static String generatePrefix(Map<String, String> prefixToNS,
			String namespace) {
		String prefix = null;

		String defaultPrefix = "ns";
		int counter = 1;
		prefix = defaultPrefix + counter;

		while (prefixToNS.keySet().contains(prefix)) {
			counter++;
			prefix = defaultPrefix + counter;
		}

		return prefix;
	}

	private static String getExistingPrefix(Map<String, String> prefixToNS,
			String namespace) {
		String prefix = null;

		Iterator<String> it = prefixToNS.keySet().iterator();
		while (it.hasNext()) {
			String p = it.next();
			String ns = prefixToNS.get(prefix);
			if (ns != null && ns.equals(namespace)) {
				prefix = p;
				break;
			}
		}

		return prefix;
	}

	static class DefaultNamespaceContext implements NamespaceContext {

		private Map<String, String> mPrefixToNSMap;

		DefaultNamespaceContext(Map<String, String> prefixToNSMap) {
			this.mPrefixToNSMap = prefixToNSMap;
		}

		public String getNamespaceURI(String prefix) {
			return mPrefixToNSMap.get(prefix);
		}

		public String getPrefix(String namespaceURI) {
			Iterator<String> it = mPrefixToNSMap.keySet().iterator();
			while (it.hasNext()) {
				String prefix = it.next();
				String ns = mPrefixToNSMap.get(prefix);
				if (namespaceURI.equals(ns)) {
					return prefix;
				}
			}

			return null;
		}

		public Iterator getPrefixes(String namespaceURI) {
			List prefixes = new ArrayList();
			Iterator<String> it = mPrefixToNSMap.keySet().iterator();
			while (it.hasNext()) {
				String prefix = it.next();
				String ns = mPrefixToNSMap.get(prefix);
				if (namespaceURI.equals(ns)) {
					prefixes.add(prefix);
				}
			}

			return prefixes.iterator();
		}

	}

	/**
	 * Evaluate the xpath expression in the given xpath context and return a
	 * string value, using the delim param as delimiter if a collection of value
	 * is returned
	 * 
	 * @param context
	 * @param expression
	 * @param delim
	 * @return The string value as the result of the evaluation
	 * @throws Exception
	 */
	public static String getStringValue(JXPathContext context,
			String expression, String delim) throws Exception {
		Object ptrObj = evaluateExpression(expression, context);
        if (ptrObj instanceof Double) {
            return new BasicTypeConverter ().convert(ptrObj, Integer.class).toString();
        }
		return getStringValue(ptrObj, delim);
	}
    
    //This method is to add a "/" to the xpath expression, so that the xpath that starts
    // from the subtree of the root will be recognized, a difference in the implementation of xpath
    // evalation between JXpath and Saxon.
    private static String fixXpath20Expression (String expression) {
        Matcher m2 = p2.matcher(expression);
        StringBuffer buffer = new StringBuffer ();
        int init = 0;
        while (m2.find()) {
            int endStart    = m2.end(1);
            buffer.append (expression.substring(init, endStart));
            buffer.append("/");
            init = endStart;
        }    
        if (init < expression.length()) {
            buffer.append(expression.substring(init));
        }
       return buffer.toString();
    
    }
    
    private static java.util.Iterator<XdmItem> evaluateXpath20Expression(
            JXPathContext context, String xpathExpression, Task task)
            throws Exception {
        java.util.Iterator<XdmItem> result = null;
        xpathExpression = fixXpath20Expression(xpathExpression);
        if (! (context.getVariables() instanceof RuntimeVariables))
            return null;
        RuntimeVariables vars = RuntimeVariables.class.cast(context
                .getVariables());
        Map<String, Object> varMap = vars.getVarMap();
        Processor proc = new Processor(false);
        net.sf.saxon.s9api.DocumentBuilder builder = proc.newDocumentBuilder();
        XPathCompiler compiler = proc.newXPathCompiler();
        compiler.setBackwardsCompatible(true);
        Map<String, String> totalNameSpaces = task.getTotalNamespaces();
        for (Map.Entry<String, String> nsEntry : totalNameSpaces.entrySet()) {
            compiler.declareNamespace(nsEntry.getKey(), nsEntry.getValue());
        }
        Map<net.sf.saxon.s9api.QName, XdmItem> xpath20Vars = new HashMap<net.sf.saxon.s9api.QName, XdmItem>();
        for (Map.Entry<String, Object> varEntry : varMap.entrySet()) {
            Object var = varEntry.getValue();
            RuntimeVariables.VariableType type = vars.getVariableType(varEntry
                    .getKey());
            net.sf.saxon.s9api.QName varName = new net.sf.saxon.s9api.QName(
                    varEntry.getKey());
            if (type == RuntimeVariables.VariableType.Element) {
                xpath20Vars.put(varName, builder
                        .build(new DOMSource((Node) var)));
            } else {
                xpath20Vars.put(varName, new XdmAtomicValue((String) var));
            }
            compiler.declareVariable(varName);
        }
        XPathExecutable executable = compiler.compile(xpathExpression);
        XPathSelector selector = executable.load();
        for (Map.Entry<net.sf.saxon.s9api.QName, XdmItem> varEntry : xpath20Vars
                .entrySet()) {
            selector.setVariable(varEntry.getKey(), varEntry.getValue());
        }
        result = selector.iterator();

        return result;
    }

    public static String getStringValueXpath20(JXPathContext context,
            String xpathExpression, Task task, String delim) throws Exception {
        StringBuffer buffer = new StringBuffer();
        java.util.Iterator<XdmItem> iter = evaluateXpath20Expression(context,
                xpathExpression, task);
        List<XdmItem> list = new ArrayList<XdmItem>();
        if (iter != null) {
            while (iter.hasNext()) {
                list.add(iter.next());
            }
        }
        if (list.size() == 1) {
            XdmItem item = list.get(0);
            return item.getStringValue();
        } else if (list.size() > 1) {
            boolean isFirst = true;
            for (XdmItem item : list) {
                if (isFirst) {
                    buffer.append((item.getStringValue()));
                    isFirst = false;
                } else {
                    buffer.append(delim);
                    buffer.append((item.getStringValue()));
                }
            }
        }
        return buffer.toString();
    }

	/**
	 * Return a string deliminated with the delim param if a collection of value
	 * is contained
	 * 
	 * @param ptr
	 * @param delim
	 * @return
	 */
	private static String getStringValue(Object ptr, String delim) {

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
	 * Evaluate the expression to a pointer
	 * 
	 * @param expr
	 * @param jxpathContext
	 * @return
	 * @throws Exception
	 */
	private static Object evaluateExpression(String expr,
			JXPathContext jxpathContext) throws Exception {
		Iterator it = jxpathContext.iteratePointers(expr);
		Object fromTemp = null;
		int i = 0;

		for (i = 0; it.hasNext(); i++) {
			if (i == 0) {

				// Maybe only 1 node is selected
				Pointer sourcePtr = (Pointer) it.next();
				if (sourcePtr instanceof BeanPointer
						|| sourcePtr instanceof DOMAttributePointer) {
					fromTemp = sourcePtr.getValue();
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
			return jxpathContext.iteratePointers(expr);
		}
		return fromTemp;

	}

	/**
	 * Return either the iterator if it points to a collection or the node/value
	 * that the single pointer points to
	 * 
	 * @param fromVal
	 * @return Either a node/value or the iterator if it points to a collection
	 */
	private static Object getSingleValueObject(Object fromVal) {
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

    public static QName getQName (String prefixedString, Map nameSpaceMap) throws ModelException {
        if (nameSpaceMap == null) {
            throw new ModelException (MESSAGES.getString("ModelUtil.Invalid_QName", new String [] {prefixedString, nameSpaceMap.toString()}));
        }
        String [] qn = QNAME_PATTERN.split(prefixedString, 2);
        String prefix = "";
        String localName = "";
        if (qn.length == 2) {
            prefix = qn[0];
            localName = qn[1];
        } else if (qn.length == 1) { 
            localName = qn[0];
        }
        String uri = (String) nameSpaceMap.get(prefix);
        if (uri == null) {
            throw new ModelException (MESSAGES.getString("ModelUtil.Invalid_QName", new String [] {prefixedString, nameSpaceMap.toString()}));
        }
        return new QName (uri, localName);
     }

     public static void  addToNSMap (String prefixedName, String value,  Map<String, String> nameSpaceMap) {
        if (!prefixedName.startsWith(XMLNS)) 
            return;
        
         String [] qn = QNAME_PATTERN.split(prefixedName, 2);
         String prefix = null;
         if (qn.length == 2) {
             prefix = qn[1];
           }  
         
         if (prefix != null) {
            nameSpaceMap.put(prefix, value);
         } else {
            nameSpaceMap.put(DEFAULT_NS, value);
         }
     }    
}
