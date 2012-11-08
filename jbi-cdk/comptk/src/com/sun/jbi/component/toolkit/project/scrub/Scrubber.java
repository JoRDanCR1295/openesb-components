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
 * @(#)Scrubber.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.component.toolkit.project.scrub;

import java.io.File;
import java.io.IOException;
import javax.xml.xpath.XPathConstants;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.sun.jbi.common.util.Util;
import com.sun.jbi.component.toolkit.project.model.Expr;
import com.sun.jbi.component.toolkit.project.model.Regex;
import com.sun.jbi.component.toolkit.project.util.I18n;
import com.sun.jbi.component.toolkit.project.util.ProjectException;
import com.sun.jbi.component.toolkit.project.util.XPathElement;
import com.sun.jbi.component.toolkit.project.util.XmlWriter;

/**
 * 
 * @author Kevan Simpson
 */
public class Scrubber extends XPathElement {//XmlObject<ScrubExpr> {
    public static final String DEFAULT_PROFILE = "ojc-core";
    
    public enum Mode { profile, file, unit, token, custom }
    
    private int mMode;
    private Profile mProfile;
    private Element mTarget;
    private File mTargetFile;
    
    public Scrubber(Element scrubFileElem, File scrubConfig,  
                    File targetFile, Element targetElem, Profile profile) {
        super(scrubConfig, scrubFileElem);
        init(targetFile, targetElem, profile);
    }
    
    public void save() {
        if (mMode == Mode.profile.ordinal()) {
            String xml = (new XmlWriter(false)).write(mTarget); 
            boolean testing = mProfile.getName().equals("test");
            if (testing) {
                System.out.println("Writing content to file: "+ mTargetFile.getAbsolutePath());
                System.out.println("********************************************************");
                System.out.println(xml);
                System.out.println("********************************************************");
            }
            else {
                try {
                    Util.writeFile(mTargetFile, xml);
                }
                catch (IOException ioe) {
                    throw error(I18n.loc(   // XXX
                            "Failed to save scrubbed file {0}: {1}", 
                            mTargetFile.getAbsolutePath(), ioe.getMessage()), ioe);
                }
            }
        }
        else {
            System.out.println("WRONG MODE TO SAVE: "+ mMode);
        }
    }
    
    public void scrub() {
        scrub(getElement());
    }
    
    protected void scrub(Element elem) {
        try {
            ++mMode;
//            System.out.println("Scrubbing "+ Mode.values()[mMode].toString());
//            System.out.println("Elem = "+ elem.getNodeName());
            switch (Mode.values()[mMode]) {
                case file: {
                    fileScrub(elem);
                    break;
                }
                case unit: {
                    unitScrub(elem);
                    break;
                }
                case token: {
                    tokenScrub(elem);
                    break;
                }
                case custom: {
                    customScrub(elem);
                    break;
                }
                default: {
                    System.out.println("NOT SCRUBBING: "+ Mode.values()[mMode]);
                }
            }
        }
        finally {
            --mMode;
        }
    }

    protected void childScrub(Element elem, ScrubExpr expr) {
        NodeList kids = (NodeList) 
                evaluate(elem, expr.getXPath(), XPathConstants.NODESET);
        if (kids != null) {
            for (int i = 0, n = kids.getLength(); i < n; i++) {
                Element child = (Element) kids.item(i);
                scrub(child);
            }
        }
    }
    
    protected void customScrub(Element tkn) {
//        String xpath = tkn.getAttribute("xpath");
//        System.out.println("Scrubbing custom token: "+ xpath);
//        Regex re = null;
//        try { re = Regex.valueOf(tkn.getAttribute("regex")); }
//        catch (Exception e) { e.printStackTrace(); }

        throw new UnsupportedOperationException("Custom Scrub not supported!");
    }

    protected void fileScrub(Element elem) {
        childScrub(elem, ScrubExpr.units);
    }

    protected void tokenScrub(Element tkn) {
        // <token expr="artifact_id" value="$@PROJ_NAME@-top"/>
        if (!Util.isEmpty(tkn.getAttribute("xpath"))) {
            scrub(tkn); // custom
        }
        else {
            ScrubExpr expr = ScrubExpr.valueOf(tkn.getAttribute("expr"));
            System.out.println("Scrubbing token: "+ expr.toString());            
            String pred = tkn.getAttribute("predicate");
            Regex re = null;
            try { re = Regex.valueOf(tkn.getAttribute("regex")); }
            catch (Exception e) { re = expr.getRegex(); }
            
//            XmlObject<Expr> xo = new XmlObject<Expr>(mTarget, ScrubExpr.values());
            XPathElement xelem = new XPathElement(mTarget);
            String value = tkn.getAttribute("value");
            Node node = xelem.getNode(expr.getXPath()), newNode = null;
            switch (re) {
                case element: {
                    newNode = (Node) xelem.evaluate(mTarget, 
                            expr.predicate(pred), XPathConstants.NODE);
                    if (newNode != null) {
                        setText(newNode, tkn, value);
                    }
                    break;
                }
                case add: {
                    String duplicate = resolveValue(tkn.getAttribute("duplicate"));
                    if (isDuplicate(duplicate, xelem, expr)) {
                        newNode = null;
                        break;
                    }
                    
                    if (Util.isEmpty(pred)) {
                        // not allowed unless value is absent
                        if (Util.isEmpty(value)) {
                            NodeList kids = tkn.getChildNodes();
                            if (kids != null) {
                                for (int i = 0, n = kids.getLength(); i < n; i++) {
                                    Node kid = kids.item(i);
                                    kid = node.getOwnerDocument().importNode(kid, true);
                                    if (newNode == null && kid instanceof Element) {
                                        newNode = kid;
                                    }
                                    node.appendChild(kid);
                                }
                            }
                        }
                        else {
                            throw new ProjectException("FUBAR");// XXX
                        }
                    }
                    else {  // append as last
                        int ix = pred.indexOf(":");
                        String ns = null, tag = null;
                        if (ix < 0) {   // no namespace
                            tag = pred;
                        }
                        else {
                            // TODO support namespaces
                            throw new ProjectException("FUBAR");// XXX
                        }
                        
                        pred = resolveValue(pred);
                        if (pred.startsWith("'")) {
                            pred = (String) evaluate(tkn, pred, XPathConstants.STRING);
                            tag = pred;
                            newNode = (ns == null)
                                    ? node.getOwnerDocument().createElement(tag)
                                    : node.getOwnerDocument().createElementNS(ns, tag);
                            node.appendChild(newNode);
                        }
                        else {
                            Element parent = (Element) node;
                            Node sib = (Node) evaluate(parent, pred, XPathConstants.NODE);
                            newNode = sib.cloneNode(true);
                            parent.replaceChild(newNode, sib);
                            parent.insertBefore(sib, newNode);
                        }
                        
                        setText(newNode, tkn, value);
                    }
                    break;
                }
            }

            if (newNode != null) {
                String commentText = tkn.getAttribute("comment");
                if (!Util.isEmpty(commentText)) {
                    Node comment = newNode.getOwnerDocument().createComment(commentText);
                    newNode.getParentNode().insertBefore(comment, newNode);
                }
            }
        }
    }

    protected boolean isDuplicate(String duplicate, XPathElement xelem, Expr expr) {
        if (!Util.isEmpty(duplicate)) {
            Node dupe = (Node) xelem.evaluate(
                    mTarget,
                    expr.predicate(resolveValue(duplicate)), 
                    XPathConstants.NODE);
            if (dupe != null) {
                return true;
            }
        }
        
        return false;
    }
    
    protected String resolveValue(String raw) {
        if (raw.startsWith("%")) {  // replacement token
            return mProfile.getProperty(raw);
        }
        
        int pos = -1;
        String value = raw;
        while ((pos = value.indexOf("$%")) >= 0) {
            int start = pos + 1, end = value.indexOf("%", start + 1) + 1;
            
            value = value.substring(0, pos) + 
                    resolveValue(value.substring(start, end)) +
                    value.substring(end);
        }
        
        return value;
    }
    
    protected boolean setText(Node node, Element tkn, String value) {
        // resolve value
        if (!Util.isEmpty(value)) { 
            value = resolveValue(value);
            value = (String) evaluate(tkn, value, XPathConstants.STRING);
            node.setTextContent(value);
            return true;
        }
        
        return false;
    }
    protected void unitScrub(Element elem) {
        Element unit = mProfile.lookupUnitDef(elem);
        childScrub(unit, ScrubExpr.tokens);
    }

    protected void init(File targetFile, Element targetElem, Profile profile) {
        // init xpath
        mTargetFile = targetFile;
        mTarget = targetElem;
        mProfile = profile;
        mMode = Mode.profile.ordinal();
    }
}
