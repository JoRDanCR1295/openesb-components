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
 * @(#)XPathElement.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.util;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.namespace.QName;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.sun.jbi.common.descriptor.JbiDescriptor;
import com.sun.jbi.common.descriptor.parsers.JbiNamespaceContext;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.common.xml.XmlUtil;

/**
 * 
 * @author Kevan Simpson
 */
public class XPathElement {
    // jbi ns already added to JbiNamespaceContext
    public enum NS { 
        JBI("jbi", JbiDescriptor.JBI_NS),
        CDK("cdk", "http://www.sun.com/jbi/cdk/project"),
        CFG("cfg", "http://www.sun.com/jbi/Configuration/V1.0"),
        LOG("log", "http://www.sun.com/jbi/descriptor/logging"),
        WSDL("wsdl", "http://schemas.xmlsoap.org/wsdl/"),
        BPEL("bpel", "http://docs.oasis-open.org/wsbpel/2.0/process/executable"),
        TMAP("tmap", "http://www.sun.com/jbi/xsltse/transformmap");
        
        private String mPrefix, mUri;
        private NS(String prefix, String uri) {
            mPrefix = prefix;
            mUri = uri;
        }
        
        public String getPrefix() {
            return mPrefix;
        }

        public String getUri() {
            return mUri;
        }
        
        public QName newQName(String localPart) {
            return new QName(getUri(), localPart, getPrefix());
        }
    }

    private static XPathFactory mFactory = XPathFactory.newInstance();
    private static Logger mLogger = Logger.getLogger(XPathElement.class.getName());
    
    private XPath mXPath;
    private JbiNamespaceContext mNSCtx;
    private Element mElement;
    private Map<String, XPathExpression> mExprCache;
    private File mFile;
    
    private XPathElement() {
        mXPath = newXPath();
        mNSCtx = (JbiNamespaceContext) mXPath.getNamespaceContext();
        mExprCache = new HashMap<String, XPathExpression>();
        mLogger = Logger.getLogger(this.getClass().getName());
    }

    protected XPathElement(File file, Element elem, String... exprs) {
        this();
        mFile = file;
        mElement = elem;
        precompile(exprs);
    }
    
    protected XPathElement(XPathElement elem, String... exprs) {
        this(elem.getFile(), elem.getElement(), exprs);
    }
    
    public XPathElement(Element elem, String... exprs) {
        this(null, elem, exprs);
    }
    
    public Node getNode(String xpath) {
        return (Node) getValue(xpath, XPathConstants.NODE);
    }
    
    public NodeList getNodeSet(String xpath) {
        return (NodeList) getValue(xpath, XPathConstants.NODESET);
    }

    public QName getQName(String xpath) {
        String raw = getString(xpath);
        if (!Util.isEmpty(raw) && raw.contains(":")) {
            int colon = raw.indexOf(":");
            String prefix = raw.substring(0, colon);
            return new QName(getElement().lookupNamespaceURI(prefix), 
                             raw.substring(colon + 1),
                             prefix);
        }
        
        return (raw == null) ? null : QName.valueOf(raw);
    }
    
    public String getString(String xpath) {
        return (String) getValue(xpath, XPathConstants.STRING);
    }

    public Object getValue(String xpath, QName type) {
        return evaluate(getElement(), xpath, type);
    }
    
    public Object evaluate(Element elem, String xpath, QName type) {
        if (!Util.isEmpty(xpath) && type != null && elem != null) {
            try {
                XPathExpression expr = mExprCache.get(xpath);
                if (expr == null) {
                    expr = getXPath().compile(xpath);
                    mExprCache.put(xpath, expr);
                }
                return expr.evaluate(elem, type);
            }
            catch (XPathExpressionException xpee) {
                String msg = I18n.loc(  // XXX
                        "Failed to evaluate xpath {0}: {1}", 
                        xpath, xpee.getMessage());
                log().log(Level.WARNING, msg, xpee);
            }
        }
        
        return null;
    }
    
    public boolean setValue(String xpath, String value) {
        XPathExpression expr = mExprCache.get(xpath);
        if (expr == null) {
            precompile(xpath);
            expr = mExprCache.get(xpath);
        }
        
        if (expr != null) {
            try {
                Object node = expr.evaluate(getElement(), XPathConstants.NODE);
//                System.out.println("setting value on "+ node);
                if (node instanceof Element) {
                    ((Element) node).setTextContent(value);
                    return true;
                }
                else if (node instanceof Attr) {
                    ((Attr) node).setValue(value);
                    return true;
                }
                else if (node instanceof NodeList) {
                    return true;
                }
                else if (node == null && xpath.startsWith("@")) {
                    // create attribute
                    getElement().setAttribute(xpath.substring(1), value);
                    return true;
                }
            }
            catch (XPathExpressionException xpee) {
                String msg = I18n.loc(  // XXX
                        "Failed to evaluate xpath {0}: {1}", 
                        xpath, xpee.getMessage());
                log().log(Level.WARNING, msg, xpee);
            }
        }
        
        return false;
    }

    /** 
     * Returns the xPath.
     * @return the xPath. 
     */
    public XPath getXPath() {
        return mXPath;
    }

    /** 
     * Returns the nSCtx.
     * @return the nSCtx. 
     */
    public JbiNamespaceContext getNSCtx() {
        return mNSCtx;
    }

    /** 
     * Returns the element.
     * @return the element. 
     */
    public Element getElement() {
        return mElement;
    }

    /** 
     * Returns the file.
     * @return the file. 
     */
    public File getFile() {
        return mFile;
    }

    public String toXml(boolean omitDeclaration) {
        return toXml(getElement().getOwnerDocument().getDocumentElement(), omitDeclaration);
    }
    
    public String toXml(Node node, boolean omitDeclaration) {
        return (new XmlWriter(omitDeclaration)).write(node); 
    }

    protected ProjectException error(String msg, Exception e) {
        if (e == null) {
            log().warning(msg);
            return new ProjectException(msg);
        }
        else {
            // XXX
            e.printStackTrace();
            log().log(Level.WARNING, msg, e);
            return new ProjectException(msg, e);
        }
    }
    
    protected Logger log() {
        return mLogger;
    }
    
    protected void precompile(String... exprs) {
        if (exprs != null) {
            for (String str : exprs) {
                try {
                    mExprCache.put(str, getXPath().compile(str));
                }
                catch (XPathExpressionException xpee) {
                    throw new ProjectException(I18n.loc(   // XXX
                            "Failed to intialize xpath expression ([0]): {1}", 
                            str, xpee.getMessage()), 
                            xpee);
                }
            }
        }
    }

    /**
     * Calls {@link XmlUtil#readXml(File)} and creates an <code>XPathElement</code>.
     *  
     * @param file The xml file containing the element to load.
     * @param exprs Optional set of xpath expressions to precompile.
     */
    public static XPathElement loadXmlFile(File file, String... exprs) {
        if (file == null || !file.exists()) {
            return null;
        }
        
        try {
            Document doc = XmlUtil.readXml(file);
            return (doc == null) ? null 
                    : new XPathElement(file, doc.getDocumentElement(), exprs);
        }
        catch (Exception e) {
            throw new ProjectException(I18n.loc(   // XXX
                    "Failed to load XPathElement from [0]: {1}", 
                    file.getAbsolutePath(), e.getMessage()), 
                    e);
        }
    }

    private static XPath newXPath() {
        XPath xp = mFactory.newXPath();
        JbiNamespaceContext ctx = new JbiNamespaceContext();
        // add additional CDK-related namespaces
        for (NS ns : NS.values()) {
            ctx.addNamespace(ns.getPrefix(), ns.getUri());
        }
        xp.setNamespaceContext(ctx);
        
        return xp;
    }
}
