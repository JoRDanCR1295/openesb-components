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
 * @(#)XmlTester.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.util.test;

import java.util.ArrayList;
import java.util.List;

import javax.xml.transform.dom.DOMSource;

import org.custommonkey.xmlunit.Diff;
import org.custommonkey.xmlunit.XMLUnit;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

//import com.sun.jbi.common.util.Util;
//import com.sun.jbi.common.xml.XmlUtil;

/**
 * Utility to compare XML nodes of various types for equality.
 * @author Kevan Simpson
 */
public class XmlTester {
    public boolean compareXml(Node node1, Node node2) throws Exception {
        if (node1 == null) {
            return (node2 == null);
        }
        else if (node2 == null) {
            return false;
        }
        else if (!node1.getClass().isAssignableFrom(node2.getClass()) || 
                 !node2.getClass().isAssignableFrom(node1.getClass())) {
            return false;
        }
        
        if (node1 instanceof Document && node2 instanceof Document) {
            return compareXml(((Document) node1).getDocumentElement(), 
                              ((Document) node2).getDocumentElement());
        }
        else if (node1 instanceof DocumentFragment && 
                 node2 instanceof DocumentFragment) {
            return compareXml((DocumentFragment) node1, 
                              (DocumentFragment) node2);
        }
        else if (node1 instanceof Element && node2 instanceof Element) {
            return compareXml((Element) node1, (Element) node2);
        }
        
        return false;
    }
    
    public boolean compareXml(Element e1, Element e2) throws Exception {
        if (e1 == null || e2 == null) return false;
        
        return compareXml(Util.print(new DOMSource(e1)), 
                          Util.print(new DOMSource(e2)));
    }

    public boolean compareXml(DocumentFragment frag1, 
                                      DocumentFragment frag2) throws Exception {
        if (frag1 == null || frag2 == null) return false;
        
        NodeList nodes1 = frag1.getChildNodes(), nodes2 = frag2.getChildNodes();
        if (nodes1.getLength() == nodes2.getLength()) {
            for (int i = 0, n = nodes1.getLength(); i < n; i++) {
                if (!compareXml(nodes1.item(i), nodes2.item(i))) {
                    return false;
                }
            }
            
            return true;
        }
        else {
            List<Element> list1 = grabElements(nodes1), list2 = grabElements(nodes2);
            if (list1.size() == list2.size()) {
                for (int i = 0, n = list1.size(); i < n; i++) {
                    if (!compareXml(list1.get(i), list2.get(i))) {
                        return false;
                    }
                }
                
                return true;
            }
        }

        return false;
    }

    public boolean compareXml(String xml1, String xml2) throws Exception {
        if (Util.isEmpty(xml1) || Util.isEmpty(xml2)) return false;
        
        XMLUnit.setIgnoreWhitespace(true);
        Diff diff = XMLUnit.compareXML(xml1, xml2);
        return diff.similar();
    }
    
    public List<Element> grabElements(NodeList nodes) {
        List<Element> list = new ArrayList<Element>();
        for (int i = 0, n = nodes.getLength(); i < n; i++) {
            Node nd = nodes.item(i);
            if (nd instanceof Element) {
                list.add((Element) nd);
            }
        }
        
        return list;
    }

}
