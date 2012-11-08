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
 * @(#)Regex.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.model;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.w3c.dom.Attr;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import com.sun.jbi.component.toolkit.project.util.Selection;
import com.sun.jbi.component.toolkit.project.util.XPathElement;

/**
 * 
 * @author Kevan Simpson
 */
public enum Regex {
    element,
    attribute {
        /** @see com.sun.jbi.component.toolkit.project.model.Regex#find(java.lang.String, java.lang.String, com.sun.jbi.component.toolkit.project.xml.XmlObject) */
        @Override
        public Selection find(String xml, String xpath, XPathElement xelem /*XmlObject<Expr> xo*/) {
            Attr attr = (Attr) xelem.getNode(xpath);
//                    xo.evaluate(xpath, xo.getElement(), XPathConstants.NODE);
            Element elem = attr.getOwnerElement();
            Matcher m = Pattern.compile(attr.getName()
                    +"\\s*=(\"[^\"]*\"|\'[^\']*\')").matcher(xml);

            Selection elemSel = findElement(xml, elem, false), selection = null;
            if (elemSel != null) {
                int pos = 0;
                while (m.find(pos)) {
                    if (within(m, elemSel)) {
                        selection = new Selection(m);
                        break;
                    }
                    else {
                        pos = m.end();
                    }
                }
            }
            else if (m.find()) {
                selection = new Selection(m);
            }
            
            return selection;
        }
    },
    add,
    copy,
    read_only;
    
    public Selection find(String xml, String xpath, XPathElement xelem /*XmlObject<Expr> xo*/) {
        Element elem = (Element)
                xelem.getNode(xpath);
//                xelem.evaluate(xpath, xelem.getElement(), XPathConstants.NODE);
        Selection selection = findElement(xml, elem, true);
        return selection;
    }
    
    protected Selection findElement(String xml, Element elem, boolean triangulate) {
        if (elem != null) {
            
            // use satellite nodes to triangulate element position
            Selection[] selSats = null;
            int have = 0, need = 0;
            if (triangulate) {
                Node[] sats = new Node[] { 
                        elem.getParentNode(), sibling(elem, false), sibling(elem, true)
                };
                int len = sats.length;
                selSats = new Selection[len];
                for (int i = 0; i < len; i++) {
                    Node n = sats[i];
                    if (n instanceof Element) {
                        selSats[i] = findElement(xml, (Element) n, false);
                        if (selSats[i] != null) {
                            ++need;
                        }
                    }
                }
            }
            
            String text = (elem.getElementsByTagName("*").getLength() > 0)
                    ? ".*" : elem.getTextContent();
            // escape backslashes, dollar signs, and braces
            text = text.replace("\\", "\\\\").replace("$", "\\$")
                       .replace("{", "\\{").replace("}", "\\}");
            StringBuffer re = new StringBuffer();
            re.append("<").append(elem.getNodeName()).append("[^>]*>")
              .append(text)   // windows paths
              .append("</").append(elem.getNodeName()).append(">");
            Matcher m = Pattern.compile(re.toString()).matcher(xml);
            int pos = 0;
            while (m.find(pos)) {
                if (triangulate){
                    have = 0;   // reset
                    for (int i = 0, n = selSats.length; i < n; i++) {
                        if (selSats[i] != null) {
                            switch (i) {
                                case 0: {   // parent
                                    if (within(m, selSats[i])) {
                                        ++have;
                                    }
                                    break;
                                }
                                case 1: {   // previous sibling
                                    if (m.start() > selSats[i].getEnd()) {
                                        ++have;
                                    }
                                    break;
                                }
                                case 2: {   // next sibling
                                    if (m.end() < selSats[i].getOffset()) {
                                        ++have;
                                    }
                                    break;
                                }
                            }
                        }
                    }
                    
                    if (have == need) {
                        return new Selection(m);
                    }
                    
                    pos = m.end();  // keep looking
                }
                else {  // return first find
                    return new Selection(m);
                }
            }
        }

        return null;
    }
    
    protected boolean within(Matcher m, Selection sel) {
        if (m != null && sel != null) {
            return (m.start() > sel.getOffset() 
                    && m.end() < sel.getEnd());
        }
        
        return false;
    }
    
    protected Element sibling(Element elem, boolean next) {
        if (elem != null) {
            Node sib = elem;
            do {
                sib = (next) ? sib.getNextSibling() : sib.getPreviousSibling();
                if (sib instanceof Element) {
                    return (Element) sib;
                }
            }
            while (sib != null);
        }
        return null;
    }
}