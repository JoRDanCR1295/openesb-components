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
 * @(#)CloneSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.common.visitor;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Stack;
import java.util.Map.Entry;
import java.util.logging.Logger;

import com.sun.bpel.model.common.EInsightModelException;
import com.sun.bpel.model.common.MessageManager;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.VisitorSupport;

/**
 * Supports cloning of BPEL/WSDL documents.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CloneSupport implements VisitorSupport {
    
    /** The logger. */
    private static final Logger mLogger = Logger.getLogger(CloneSupport.class.getName());
    
    /** MessageManager for localized strings. */    
    private static MessageManager mMsg = MessageManager.getManager(CloneSupport.class);
    
    /** Hold stack of cloned parents */
    private Stack clones = new Stack();
    
    /** Hold stack of original parents */
    private Stack originals = new Stack();
    
    /** Holds destination document */
    private XMLDocument destDoc;
    
    /** Holds results of cloning */
    private XMLNode[] results;
    
    /** Creates a new instance of SAXWriterSupport.
     */
    public CloneSupport() {
    }
    
    /** Describes how to instantiate a new element for cloning
     */
    public interface Instantiable {
        /** Instantiate a new element.
         * @return  new element.
         */
        XMLElement create();
        
        /** Runs any code after cloning.
         * @param   clone   Cloned element.
         */
        void postCloneRun(XMLElement clone);
    }
    
    /** Peek at current parent cloned element.
     * @return  Current parent clone at top of stack.
     */
    public XMLElement peekCurrentClone() {
        return (clones.isEmpty() ? null : (XMLElement) clones.peek());
    }
    
    /** Push a clone parent element as the current one.
     * @param   parent  Element to be the current parent clone.
     */
    public void pushClone(XMLElement parent) {
        clones.push(parent);
    }
    
    /** Pop off a clone parent element.
     * @return  Clone parent element that is the previous current parent.
     */
    public XMLElement popClone() {
        XMLElement top = (XMLElement) clones.pop();
        if (top != null) {
            setCloned(top);
        }
        return top;
    }
    
    /** Gets the deep cloned element.
     * @return  Deep cloned element.
     */
    public XMLNode getCloned() {
        return results[0];
    }
    
    /** Sets the deep cloned element.
     * @param   cloned  Deep cloned element.
     */
    public void setCloned(XMLNode cloned) {
        results[0] = cloned;
    }
    
    /** Gets the clone results holder.
     * @return clone results holder.
     */
    public XMLNode[] getResults() {
        return results;
    }
    
    /** Sets the clone results holder.
     * @param   results Clone results holder.
     */
    public void setResults(XMLNode[] results) {
        this.results = results;
    }

    /** Determines if an element is being started or ended.
     * @param   e   The element.
     * @return  <tt>true</tt> if element is being started.
     */
    public boolean isElementStart(XMLElement e) {
        boolean start = false;
        if (originals.isEmpty() || !originals.peek().equals(e)) {
            originals.push(e);
            start = true;
        } else {
            originals.pop();
            start = false;
        }
        return start;
    }
     
    /** Copies list of attributes for an element.
     * @param   xmlAttrs    XML attribute array.
     * @param   copy        Copy element
     */
    protected void copyToAttributeList(List xmlAttrs, XMLElement copy) {
        if (xmlAttrs != null) {
            for (int i = 0, n = xmlAttrs.size(); i < n; i++) {
                XMLAttribute attr = (XMLAttribute) xmlAttrs.get(i);
                copy.setAttribute(attr.getQualifiedName(), attr.getValue());
            }
        }
    }
    
    /** Copies all the key/value pairs other attributes.
     * @param   m       The <tt>Map</tt> object to convert.
     * @param   copy    Copy element
     */
    protected void copyOtherToAttributeList(Map m, XMLElement copy) {
        if ((m != null) && !m.isEmpty()) {
            Iterator iter = m.entrySet().iterator();
            while (iter.hasNext()) {
                Entry me = (Entry) iter.next();
                copy.setOtherAttributes((String) me.getKey(), (String) me.getValue()); 
            }
        }
    }
    
    /** Copies all the XML namespace attributes.
     * @param   m       The <tt>Map</tt> object to convert.
     * @param   copy    Copy element
     */
    protected void copyNamespaceToAttributeList(Map m, XMLElement copy) {
        if ((m != null) && !m.isEmpty()) {
            Iterator iter = m.entrySet().iterator();
            while (iter.hasNext()) {
                Entry me = (Entry) iter.next();
                String key = (String) me.getKey();
                if (XMLElement.WellKnownAttr.XMLNS.equals(key)) {
                    // as-is
                } else {
                    key = XMLElement.WellKnownAttr.XMLNS_COLON + key;
                }
                copy.setOtherAttributes(key, (String) me.getValue());
            }
        }
    }
    
    /** Clone element's attributes.
     * @param   orig    Original element.
     * @param   copy    Copy of original.
     */
    public void cloneAttributes(XMLElement orig, XMLElement copy) {
        try {
            copyToAttributeList(orig.getOrderedAttributes(), copy);
            if (orig.getRawPresentationMap(false) != null) {
                copy.setPresentationMap(orig.getRawPresentationMap(false));
            }
            copyOtherToAttributeList(orig.getOtherAttributes(), copy);
            copyNamespaceToAttributeList(orig.getNamespaces(), copy);
        } catch (Throwable trw) {
            throw new XMLWriteVisitorException(
                "Cannot clone attributes", trw);
        }
    }
    
    /** Clones an element.
     * @param   original    Original element.
     * @param   factory     Factory to instantiate clone element.
     */
    public void cloneElement(XMLElement original, Instantiable factory) {
        if (isElementStart(original)) {
            XMLElement clone = doCloning(original, factory);
            pushClone(clone);
            factory.postCloneRun(clone);
        } else {
            popClone();
        }
    }
    
    /** Clones an empty element.
     * @param   original    Original element.
     * @param   factory     Factory to instantiate clone element.
     */
    public void cloneEmptyElement(XMLElement original, Instantiable factory) {
        XMLElement clone = doCloning(original, factory);
        setCloned(clone);
        factory.postCloneRun(clone);
    }
    
    /** Do the cloning.
     * @param   original    Original element.
     * @param   factory     Factory to instantiate clone element.
     * @return  Cloned element.
     */
    private XMLElement doCloning(XMLElement original, Instantiable factory) {
        XMLElement clone = factory.create();
        clone.setQualifiedName(original.getQualifiedName());
        cloneAttributes(original, clone);
        if (peekCurrentClone() != null) {
            peekCurrentClone().addChild(clone);
        }
        return clone;
    }
    
    /** Gets the owner document to use for instantiating a clone element.
     * @return  Owner document to use.
     */
    public XMLDocument getInstantiatingDocument() {
       return destDoc;
    }
    
    /** Sets the owner document to use for instantiating a clone element.
     * @param   doc Owner document to use.
     */
    public void setInstantiatingDocument(XMLDocument doc) {
        destDoc = doc;
    }
    
    /** Propagate all visible namespaces from original node to cloned node.
     * @param   origNode    Original node.
     * @param   dupNode     Cloned node.
     */
    public static void propagateNamespaces(XMLNode origNode, XMLNode dupNode) {
        if (origNode instanceof XMLElement) {
            XMLElement origElem = (XMLElement) origNode;
            XMLElement dupElem = (XMLElement) dupNode;
            
            // Special case for BPWSProperty & BPWSPropertyAlias in WSDL, WSDLMessage in BPEL
            XMLDocument dupElemTopOwnerDoc = dupElem.getOwnerDocument();
            while (dupElemTopOwnerDoc.getOwnerDocument() != null) {
                dupElemTopOwnerDoc = dupElemTopOwnerDoc.getOwnerDocument();
            }
            
            if (dupElemTopOwnerDoc.getDocumentElement() != null) {
                Map origTotalNSMap = origElem.getTotalNamespaces();
                if ((origTotalNSMap != null) && !origTotalNSMap.isEmpty()) {
                    Map dupLocalNSMap = dupElem.getNamespaces();
                    Iterator iter = origTotalNSMap.entrySet().iterator();
                    while (iter.hasNext()) {
                        boolean addTopLevel = false;
                        Entry me = (Entry) iter.next();
                        String key = (String) me.getKey();
                        String dupNS = dupElem.getNamespace(key);

                        // Duplicate doesn't have prefix so can add at top level except if
                        // it's either tns or default namespace prefix; those should be localized in the clone
                        if (null == dupNS) {
                            addTopLevel = !XMLElement.WellKnownAttr.XMLNS.equals(key);
                        // See if clone has the key at the local level
                        } else if ((dupLocalNSMap != null) && (dupLocalNSMap.containsKey(key))) {
                            continue;   // local version has precedence so ignore                            
                        }
                        
                        if (addTopLevel) {
                            dupElemTopOwnerDoc.setNamespace(key, (String) me.getValue());
                        } else {
                            dupElem.setNamespace(key, (String) me.getValue());
                        }
                    }
                }
            } else {
                throw new EInsightModelException("Add root element to owning XML document first!");
            }
        }
    }
}
