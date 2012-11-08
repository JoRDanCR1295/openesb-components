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
 * @(#)XMLElementImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.wsdlmodel.impl;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;
import javax.xml.namespace.QName;

import com.sun.bpel.model.common.EInsightModelException;
import com.sun.bpel.xml.NamespaceUtility;
import com.sun.bpel.xml.common.model.Documentation;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLAttributeEvent;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLElementListener;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.model.XMLElement.WellKnownAttr;
import com.sun.bpel.xml.common.visitor.AutonomousVisitor;
import com.sun.bpel.xml.common.visitor.ChildrenParentVisitor;
import com.sun.bpel.xml.common.visitor.ParentChildrenParentVisitor;
import com.sun.bpel.xml.common.visitor.ParentChildrenVisitor;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Provides the base implementation for a XML element.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class XMLElementImpl extends XMLNodeImpl implements XMLElement {
    
    /** Holds the logger */
    private static final Logger mLogger = Logger.getLogger(XMLElementImpl.class.getName());

    /** serialVersionUID for this class */
    static final long serialVersionUID = 4389756956479854919L;
    
    /** Holds all legal attributes of the element. */
    protected XMLAttribute[] xmlAttrs;
    
    /** Holds map of other attributes found but not prescribed in XSD. */
    protected Map otherAttributes;
    
    /** Holds map of namespaces. */
    private Map namespaces = new HashMap();
    
    /** Holds map of presentation attributes. */
    private Map presentationAttributes;
    
    /** Holds all legal children element tags. */
    protected String[] childrenTags;
    
    /** Holds ordered list of attributes. */
    private List orderedAttrs;
    
    /** Holds the potential documentation element. */
    private Documentation documentation;
    
    /** Object for synchonization. */    
    private Object totalNamspacesSyncObj = new Object();
    
    /** 
     * Map that holds all the namespace declarations visible to this element 
     * - including those inherited from ancestors 
     * */    
    private Map totalNamespaces;

    /**
     * Constructs a new instance of <tt>XMLElement</tt>.
     */
    protected XMLElementImpl() {
        super();
    }
    
    /**
     * Constructs a new instance of <tt>XMLElement</tt>.
     * @param   d   Owner document.
     */
    protected XMLElementImpl(XMLDocument d) {
        super(d);
    }

    /** @see XMLElement#getXmlAttributes
     */
    public XMLAttribute[] getXmlAttributes() {
        return xmlAttrs;
    }
        
    /** @see XMLElement#getOtherAttributes
     */
    public Map getOtherAttributes() {
        return otherAttributes;
    }
    
    /** @see XMLElement#setOtherAttributes(java.lang.String, java.lang.String)
     */
    public void setOtherAttributes(String name, String value) {
        if ((null == name) && (null == value)) {
            if (otherAttributes != null) {
                otherAttributes.clear();
            }
            return;
        } else if (null == name) {
            return;
        }
        
        if (name.equalsIgnoreCase(WellKnownAttr.XMLNS)) {
            setDefaultNamespace(value);
            return;
        } else if (name.startsWith(WellKnownAttr.XMLNS_COLON)) {
            name = name.substring(name.indexOf(':') + 1);
            setNamespace(name, value);
            return;
        }
        
        if (name.startsWith(XMLAttribute.SBYNPXP_COLON)) {
            if (value != null) {
                getRawPresentationMap(true).put(name, value);
            } else {
                getRawPresentationMap(true).remove(name);
            }
        } else {
            if (null == otherAttributes) {
                otherAttributes = new HashMap();
            }
            QName attrQName = NamespaceUtility.resolveAndGetQName(name, this);
        	String oldValue = (String) otherAttributes.get(name);
        	
            if (value != null) {
            	otherAttributes.put(name, value);
            
            	//event handling
                if(oldValue != null) {
                	this.fireXMLAttributedModified(attrQName, oldValue, value);
                } else {
                	this.fireXMLAttributeAdded(attrQName, value);
                }
    	    	
            } else {
                otherAttributes.remove(name);
            	this.fireXMLAttributeRemoved(attrQName, oldValue);
    	    	
            }
        }
    }
    
    /** @see XMLElement#setOtherAttributes(com.sun.bpel.model.common.model.QName, java.lang.String)
     */
    public void setOtherAttributes(QName qName, String value) {
        if (getOwnerDocument() == null) {
            throw new EInsightModelException("Associate element with owner XML document first!");
        } else if (getOwnerDocument().getDocumentElement() == null) {
            throw new EInsightModelException("Add root element to owning XML document first!");
        }
        
        String qualifiedName;
        if (qName.getPrefix() != null) {
            qualifiedName = qName.getPrefix() + ":" + qName.getLocalPart();
        } else {
            qualifiedName = qName.getLocalPart();
        }
        if ((qName.getPrefix() != null) && (qName.getNamespaceURI() != null)) {
            getOwnerDocument().getDocumentElement()
                .setNamespace(qName.getPrefix(), qName.getNamespaceURI());
        }
        setOtherAttributes(qualifiedName, value);
    }
    
    /**
     * @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Documentation) {
            setDocumentation((Documentation) c);
        } else {
            super.addChild(c);
        }
    }
    
    /**
     * @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Documentation) {
            setDocumentation(null);
        } else {
            super.removeChild(c);
        }
    }
    
    public void supperAddChild(XMLNode c) {
    	super.addChild(c);
    }
    
    public void supperRemoveChild(XMLNode c) {
    	super.removeChild(c);
    }
    
    /** @see XMLElement#getChildrenTags
     */
    public String[] getChildrenTags() {
        return childrenTags;
    }
    
    /** Tests if visitor should traverse parent first.
     * @param   v   The visitor.
     * @return  <tt>true</tt> if it should.
     */
    protected boolean traverseParentFirst(Visitor v) {
        return ((v instanceof ParentChildrenVisitor)
             || (v instanceof ParentChildrenParentVisitor)
             || (v instanceof AutonomousVisitor));
    }
    
    /** Tests if visitor should traverse children.
     * @param   v   The visitor.
     * @return  <tt>true</tt> if it should.
     */
    protected boolean traverseChildren(Visitor v) {
        return ((v instanceof ParentChildrenVisitor)
             || (v instanceof ChildrenParentVisitor)
             || (v instanceof ParentChildrenParentVisitor));
    }
    
    /** Tests if visitor should traverse parent last.
     * @param   v   The visitor.
     * @return  <tt>true</tt> if it should.
     */
    protected boolean traverseParentLast(Visitor v) {
        return ((v instanceof ChildrenParentVisitor)
             || (v instanceof ParentChildrenParentVisitor));
    }
    
    /** @see XMLElement#setDefaultNamespace(java.lang.String)
     */
    public void setDefaultNamespace(String uri) {
        setNamespace(WellKnownAttr.XMLNS, uri);
    }
    
    /** @see XMLElement#setNamespace(java.lang.String, java.lang.String)
     */
    public void setNamespace(String prefix, String uri) {
        if (null == namespaces) {
            namespaces = new HashMap();
        }
        
        String ns = (String) namespaces.get(prefix);
        if(ns != null && ns.equals(uri)) {
        	return;
        }
        
        Map oldMap = new HashMap(namespaces);
        
        if (uri != null) {
        	namespaces.put(prefix, uri);
            
            //fire event
            fireXMLAttributedModified(NamespaceUtility.resolveAndGetQName(WellKnownAttr.PREFIX_TO_NAMESPACE_MAP, this), 
            						  oldMap, 
            						  namespaces);
        } else {
            namespaces.remove(prefix);
            //fire event
            fireXMLAttributedModified(NamespaceUtility.resolveAndGetQName(WellKnownAttr.PREFIX_TO_NAMESPACE_MAP, this),
            						  oldMap, 
            						  namespaces);
        }
    }
    
    /** @see XMLElement#getDefaultNamespace()
     */
    public String getDefaultNamespace() {
        return getNamespace(WellKnownAttr.XMLNS);
    }
    
    /** @see XMLElement#getNamespace(java.lang.String)
     */
    public String getNamespace(String prefix) {
        String ns = null;
        if (namespaces != null) {
            if ((ns = (String) namespaces.get(prefix)) != null) {
                return ns;
            }
        }
        
        if ((getParent() != null) && !(getParent() instanceof XMLDocument)) {
            // none here, if not root element, try parent
            if ((ns = ((XMLElement) getParent()).getNamespace(prefix)) != null) {
                return ns;
            }
        } else if (getOwnerDocument() != null) {
            // skip right to the front and see if the document has the namespace
            if (!this.equals(getOwnerDocument().getDocumentElement())
                    && (ns = getOwnerDocument().getNamespace(prefix)) != null) {
                return ns;
            }
            
            if (getOwnerDocument().getOwnerDocument() != null) {
                // none here, try the owner document of the current document
                if ((ns = getOwnerDocument().getOwnerDocument().getNamespace(prefix)) != null) {
                    return ns;
                }
            }
        }
        
        return ns;
    }
    
    /** @see XMLElement#getNamespacePrefix
     */
    public String getNamespacePrefix(String namespaceURI) {
        String prefix = null;
        if (namespaces != null) {
            if ((prefix = getPrefix(namespaceURI)) != null) {
                return prefix;
            }
        }
        
        if (getOwnerDocument() != null) {
            if ((getParent() != null) && !(getParent() instanceof XMLDocument)) {
                // none here, if not root element, try parent
                if ((prefix = ((XMLElement) getParent()).getNamespacePrefix(namespaceURI)) != null) {
                    return prefix;
                }
            } else if (!this.equals(getOwnerDocument().getDocumentElement())
                       && (prefix = getOwnerDocument().getNamespacePrefix(namespaceURI)) != null) {
                // the document has it!
                return prefix;
            } else if (getOwnerDocument().getOwnerDocument() != null) {
                // none here, try the owner document of the current document
                if ((prefix = getOwnerDocument().getOwnerDocument().getNamespacePrefix(namespaceURI)) != null) {
                    return prefix;
                }
            }
        }
        
        return prefix;
    }
    
    /** Gets a prefix corresponding to a URI in the namespace map.
     * @param   uri     Namespace URI to match
     * @return  Corresponding prefix or <code>null</code> if none.
     */
    private String getPrefix(String uri) {
        String prefix = null;
        if ((namespaces != null) && !namespaces.isEmpty()) {
            Iterator iter = namespaces.entrySet().iterator();
            while (iter.hasNext()) {
                Map.Entry me = (Map.Entry) iter.next();
                if (((String) me.getValue()).equals(uri)) {
                    prefix = (String) me.getKey();
                    break;
                }
            }
        }
        return prefix;
    }
    
    /** @see XMLElement#getNamespaceSize
     */
    public int getNamespaceSize() {
        if (namespaces != null) {
            return namespaces.size();
        }
        return 0;
    }
    
    /** @see XMLElement#getNamespacePrefixes
     */
    public String[] getNamespacePrefixes() {
        if (namespaces != null) {
            Set ns = namespaces.keySet();
            return (String[]) ns.toArray(new String[ns.size()]);
        }
        return null;
    }
    
    /** @see XMLElement#getNamespaces
     */
    public Map getNamespaces() {
        return namespaces;
    }
    
    /** @see XMLElement#setNamespaces
     */
    public void setNamespaces(Map m) {
    	Map oldMap = new HashMap(namespaces);
        namespaces = m;
        
        if(getOwnerDocument() != null && getOwnerDocument().isEnableEvents()) {
        	fireXMLAttributedModified(NamespaceUtility.resolveAndGetQName(WellKnownAttr.PREFIX_TO_NAMESPACE_MAP, this),
        							  oldMap, 
        							  namespaces);

	        //find out if a prefix name has changed and if so
	        //fire a PROP_PREFIX_NAME_CHANGED event
	        Set newPrefixSet = new HashSet(m.keySet());
	        Set oldPrefixSet = new HashSet(oldMap.keySet());
	        newPrefixSet.removeAll(oldMap.keySet());
	        oldPrefixSet.removeAll(m.keySet());
			Map oldPrefixMap = new HashMap();
			
			Iterator it = oldPrefixSet.iterator();
			while(it.hasNext()) {
				String prefix = (String) it.next();
				oldPrefixMap.put(prefix, oldMap.get(prefix));
			}
	        
	        it = newPrefixSet.iterator();
	        while(it.hasNext()) {
	        	String newPrefix = (String) it.next();
	        	String ns = (String) m.get(newPrefix);
	        	if(ns != null) {
	        		if(oldPrefixMap.containsValue(ns)) {
	        			Iterator oit = oldPrefixMap.keySet().iterator();
	        			while(oit.hasNext()) {
	        				String oldPrefix = (String) oit.next();
	        				String ons = (String) oldPrefixMap.get(oldPrefix);
	        				if(ons.equals(ns)) {
	        					PropertyChangeEvent pEvent = new PropertyChangeEvent(this, 
	        																		 PROP_PREFIX_NAME_CHANGED,
																					 oldPrefix,
																					 newPrefix);
	        					firePropertyChangeEvent(pEvent);
	        				}
	        			}
	        		}
	        	}
	        }
        }
        
    }
    
    /**
     * @see XMLElement#getTotalNamespaces
     */
    public Map getTotalNamespaces() {
        
        synchronized (totalNamspacesSyncObj) {
            
            if (totalNamespaces != null) {
                return totalNamespaces;
            }
            
            totalNamespaces =  new HashMap();
            
            if ((getParent() != null) && !(getParent() instanceof XMLDocument)) {
                
                Map parentNamespaces = ((XMLElement) getParent()).getTotalNamespaces();
                totalNamespaces.putAll(parentNamespaces);
            }
            
            /*
             * Now include the namespace declarations from this element. Note this
             * will override any prefix declared in an ancestor element which is the 
             * correct behavior 
             */            
            totalNamespaces.putAll(namespaces);
            return totalNamespaces;
        } 
    }    
    
    /**
     * @see XMLElement#setQualifiedName(QName)
     */
    public void setQualifiedName(QName qName) {
        String qualifiedName;
        if (qName.getPrefix() != null) {
            qualifiedName = qName.getPrefix() + ":" + qName.getLocalPart();
        } else {
            qualifiedName = qName.getLocalPart();
        }
        
        if (getOwnerDocument() == null) {
            mLogger.warning("Associate element with owner XML document first!");
        } else if (getOwnerDocument().getDocumentElement() == null) {
            mLogger.warning("Add root element to owning XML document first!");
        } else if ((qName.getPrefix() != null) && (qName.getNamespaceURI() != null)) {
            getOwnerDocument().getDocumentElement()
                .setNamespace(qName.getPrefix(), qName.getNamespaceURI());
        }
        setQualifiedName(qualifiedName);
    }
    
    /** Set the qualified name for this element according to a Namespace.
     * @param   ns          Namespace the element belongs to.
     * @param   defPrefix   Default prefix to use.
     */
    protected void setQualifiedName(String ns, String defPrefix) {
        String nsPrefix = getNamespacePrefix(ns);
        if ((nsPrefix != null) && (nsPrefix.length() > 0)) {
            setQualifiedName(nsPrefix + ":" + getLocalName());
        } else {
            nsPrefix = defPrefix;
            int ring = 0;
            while (getNamespace(nsPrefix) != null) {
                nsPrefix = defPrefix + (++ring);
            }
            QName qName = NamespaceUtility.getQName(ns, getLocalName(), nsPrefix);
            setQualifiedName(qName);
        }
    }

    /** @see XMLElement#getOrderedAttributes
     */
    public List getOrderedAttributes() {
        return (orderedAttrs != null ? Collections.unmodifiableList(orderedAttrs) : null);
    }
    
    /** @see XMLElement#getAttribute
     */
    public XMLAttribute getAttribute(String n) {
        if (xmlAttrs != null) {
            for (int i = 0; i < xmlAttrs.length; i++) {
                if (n.endsWith(xmlAttrs[i].getLocalName())) {
                    return xmlAttrs[i];
                }
            }
        }
        return null;
    }
    
    /** Gets the attribute value by name.
     * @param   n   Name of attribute.
     * @return  value of the attribute.
     */
    public Object getAttributeValue(String n) {
    	if(WellKnownAttr.PREFIX_TO_NAMESPACE_MAP.equals(n)) {
    		return this.getNamespaces();
    	} else {
    		XMLAttribute attr = getAttribute(n);
    		if(attr != null) {
    			return attr.getValue();
    		}
    	}
    	
    	return null;
    }
    
    /** @see XMLElement#setAttribute(int, java.lang.String)
     */
    public void setAttribute(int i, String v) {
        setAttribute(i, null, v);
    }
    
    /** @see XMLElement#setAttribute(int, java.lang.String, java.lang.String)
     */
    public void setAttribute(int i, String q, String v) {
        if (q != null) {
            xmlAttrs[i].setQualifiedName(q);
        }
        String oldValue = xmlAttrs[i].getValue();
        
        xmlAttrs[i].setValue(v);
        xmlAttrs[i].setParent(this);
        if (null == v) {
            if (orderedAttrs != null) {
                orderedAttrs.remove(xmlAttrs[i]);
            }
        } else {
            if (null == orderedAttrs) {
                orderedAttrs = new ArrayList(xmlAttrs.length);
                orderedAttrs.add(xmlAttrs[i]);
            } else {
                orderedAttrs.clear();
                for (int j = 0; j < xmlAttrs.length; j++) {
                    if (xmlAttrs[j].getValue() != null) {
                        orderedAttrs.add(xmlAttrs[j]);
                    }
                }
            }
        }
        
        // Alert listeners of change
        fireStateChanged();
        
        //fire AttributeModifiedEvent
        QName attrQName = NamespaceUtility.resolveAndGetQName(
        		xmlAttrs[i].getQualifiedName(), this);
        fireXMLAttributedModified(attrQName, oldValue, v);
    }
    
    /** @see XMLElement#setAttribute(java.lang.String, java.lang.String)
     */
    public void setAttribute(String n, String v) {
        boolean found = false;
        if (xmlAttrs != null) {
            for (int i = 0; i < xmlAttrs.length; i++) {
                if (n.endsWith(xmlAttrs[i].getLocalName())) {
                    found = true;
                    if (n.indexOf(':') != -1) {
                        setAttribute(i, n, v);
                    } else {
                        setAttribute(i, v);
                    }
                    break;
                }
            }
        }
        if (!found) {
            setOtherAttributes(n, v);
        }
    }
    
    /** Sets the attribute by name.
     * @param   n   Name of attribute; can be either local name or qualified
     *              name.
     * @param   v   Value of attribute.
     */
    public void setAttribute(String n, Object v) {
    	if(WellKnownAttr.PREFIX_TO_NAMESPACE_MAP.equals(n)) {
    		setNamespaces((Map) v);
    	} else if(v instanceof String || v == null) {
    		setAttribute(n, (String) v);
    	} 
    }
    /**
     * @see XMLElement#getDocumentation
     */
    public Documentation getDocumentation() {
        return documentation;
    }
    
    /**
     * @see XMLElement#setDocumentation
     */
    public void setDocumentation(Documentation doc) {
        Documentation oldDoc = documentation;
        documentation = doc;
        super.replaceChild(oldDoc, doc);
        
    }
    
    /**
     * @see XMLElement#getPresentationMap
     */
    public Map getPresentationMap() {
        Map retMap = null;
        if (presentationAttributes != null) {
            retMap = new HashMap(presentationAttributes.size());
            Iterator iter = presentationAttributes.entrySet().iterator();
            while (iter.hasNext()) {
                Map.Entry me = (Map.Entry) iter.next();
                String key = (String) me.getKey();
                if (key.startsWith(XMLAttribute.SBYNPXP_COLON)) {
                    key = key.substring(XMLAttribute.SBYNPXP_COLON.length());
                }
                retMap.put(key, me.getValue());
            }
        }
        return retMap;
    }
    
    /**
     * @see XMLElement#setPresentationMap
     */
    public void setPresentationMap(Map map) {
        if (map != null) {
            getRawPresentationMap(true).clear();
            Iterator iter = map.entrySet().iterator();
            while (iter.hasNext()) {
                Map.Entry me = (Map.Entry) iter.next();
                String key = (String) me.getKey();
                if (!key.startsWith(XMLAttribute.SBYNPXP_COLON)) {
                    key = XMLAttribute.SBYNPXP_COLON + key;
                }
                getRawPresentationMap(false).put(key, me.getValue());
            }
        }
    }
    
    /**
     * @see XMLElement#getRawPresentationMap
     */
    public Map getRawPresentationMap(boolean create) {
        if ((null == presentationAttributes) && create) {
            if (getOwnerDocument() == null) {
                throw new EInsightModelException("Associate element with owner XML document first!");
            } else if (getOwnerDocument().getDocumentElement() == null) {
                throw new EInsightModelException("Add root element to owning XML document first!");
            }
            getOwnerDocument().getDocumentElement()
                .setNamespace(XMLAttribute.SBYNPXP, XMLAttribute.SBYNPXP_NAMESPACE);
            presentationAttributes = new HashMap();
        }
        return presentationAttributes;
    }
    
    /**
     * @see XMLElement#getLineLabel
     */
    public String getLineLabel() {
        String retLabel = null;
        if ((getOwnerDocument() != null) && (getOwnerDocument().getDocumentElement() != null)
                && (getOtherAttributes() != null)) {
            retLabel = (String) getOtherAttributes().get(XMLAttribute.SBYNPX_COLON + PrivateAttr.LINE_LABEL);
        }
        return retLabel;
    }
    
    /**
     * @see XMLElement#setLineLabel
     */
    public void setLineLabel(String label) {
        setOtherAttributes(SBYNPX_LINE_LABEL_QNAME, label);
    }
    
    /** @see XMLNode#accept
     */
    public boolean accept(Visitor v) {
        return superAccept(v);
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   v   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean superAccept(Visitor v) {
        if (traverseChildren(v)) {
            List kids = getChildren();
            if (kids != null) {
                for (int k = 0, nk = kids.size(); k < nk; k++) {
                    XMLNode kid = (XMLNode) kids.get(k);
                    if (!kid.accept(v)) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    
    /**
     * Added a <code> XMLElementListener </code> to the XMLElement. 
     * @param l the <code> XMLElementListener </code> to add.
     */
    public void addXMLElementListener(XMLElementListener   l) {
    	listenerSet.add(l);
    }
    
    /**
     * Removed a a <code> XMLElementListener </code> from the XMLElement.
     * @param l the <code> XMLElementListener </code> to remove.
     */
    public void removeXMLElementListener(XMLElementListener l) {
    	listenerSet.remove(l);
    }
    
    protected void fireXMLAttributeAdded(QName attrQName, String value) {
    	XMLDocument owner = getOwnerDocument();
    	if(owner != null && !owner.isEnableEvents()) {
    		return;
    	}
    	
    	XMLAttributeEvent xmlAttributeEvent = 
        	new XMLAttributeEvent(this, 
					  attrQName, 
					  value, 
					  null);
    	
    	Iterator it = listenerSet.iterator();
        while(it.hasNext()) {
	      	Object listener = it.next();
	      	if(listener instanceof XMLElementListener) {
	      	    ((XMLElementListener)listener).attributeAdded(xmlAttributeEvent);
	      	}
	      	
        }
            
        
        //propogate event to parent
        if(this.getParent() != null 
		   && this.getParent() instanceof XMLElement) {
        		((XMLElement) this.getParent()).fireXMLAttributeAdded(xmlAttributeEvent);
        }
     }
    
    
    protected void fireXMLAttributeRemoved(QName attrQName, String value) {
    	XMLDocument owner = getOwnerDocument();
    	if(owner != null && !owner.isEnableEvents()) {
    		return;
    	}
    	
    	XMLAttributeEvent xmlAttributeEvent = 
    		new XMLAttributeEvent(this, 
					  attrQName, 
					  value, 
					  null);
    	
    	Iterator it = listenerSet.iterator();
        while(it.hasNext()) {
	      	Object listener = it.next();
	      	if(listener instanceof XMLElementListener) {
	      	    ((XMLElementListener)listener).attributeRemoved(xmlAttributeEvent);
	      	}
	      	
        }
        
        //propogate event to parent
        if(this.getParent() != null 
		   && this.getParent() instanceof XMLElement) {
        		((XMLElement) this.getParent()).fireXMLAttributeRemoved(xmlAttributeEvent);
        }
     }
    
    protected void fireXMLAttributedModified(QName attrQName, Object oldValue, Object newValue) {
    	XMLDocument owner = getOwnerDocument();
    	if(owner != null && !owner.isEnableEvents()) {
    		return;
    	}
    	
    	XMLAttributeEvent xmlAttributeEvent = new XMLAttributeEvent(this, 
				  attrQName, 
				  newValue, 
				  oldValue);
    	
    	Iterator it = listenerSet.iterator();
        while(it.hasNext()) {
	      	Object listener = it.next();
	      	if(listener instanceof XMLElementListener) {
	      	    ((XMLElementListener)listener).attributeModified(xmlAttributeEvent);
	      	}
	      	
        }
        
        
        //propogate event to parent
        if(this.getParent() != null 
		   && this.getParent() instanceof XMLElement) {
        		((XMLElement) this.getParent()).fireXMLAttributedModified(xmlAttributeEvent);
        }
     }
    
   public void fireXMLAttributeAdded(XMLAttributeEvent evt) {
	   	Iterator it = listenerSet.iterator();
	    while(it.hasNext()) {
	      	Object listener = it.next();
	      	if(listener instanceof XMLElementListener) {
	      	    ((XMLElementListener)listener).attributeAdded(evt);
	      	}
	      	
	    } 
	    
	    //propogate event to parent
        if(evt != null 
           && this.getParent() != null 
		   && this.getParent() instanceof XMLElement) {
        		((XMLElement) this.getParent()).fireXMLAttributeAdded(evt);
        }
   }
     
   public void fireXMLAttributeRemoved(XMLAttributeEvent evt) {
	   	Iterator it = listenerSet.iterator();
	    while(it.hasNext()) {
	      	Object listener = it.next();
	      	if(listener instanceof XMLElementListener) {
	      	    ((XMLElementListener)listener).attributeRemoved(evt);
	      	}
	      	
	    } 
        
	    //propogate event to parent
        if(evt != null 
           && this.getParent() != null 
		   && this.getParent() instanceof XMLElement) {
        		((XMLElement) this.getParent()).fireXMLAttributeRemoved(evt);
        }
        
   }
   
   public void fireXMLAttributedModified(XMLAttributeEvent evt) {
	   	Iterator it = listenerSet.iterator();
	    while(it.hasNext()) {
	      	Object listener = it.next();
	      	if(listener instanceof XMLElementListener) {
	      	    ((XMLElementListener)listener).attributeModified(evt);
	      	}
	      	
	    }
	    
	    //propogate event to parent
	    if(evt != null 
           && this.getParent() != null 
		   && this.getParent() instanceof XMLElement) {
        		((XMLElement) this.getParent()).fireXMLAttributedModified(evt);
        }
   }
   
   
   /**
    * @see XMLElement#setDocumentation
    */
   public void setDocumentationAtTail(Documentation doc) {
//       Documentation oldDoc = documentation;
//       documentation = doc;
//       super.addChildAtTheEnd(doc, 0);
   		setDocumentation(doc);
   }
   
   
   public void merge(XMLNode target) {
       if(target == null) {
           return;
       }
       
       XMLAttribute[] attributes = this.getXmlAttributes();
       XMLAttribute[] targetAttributes = ((XMLElement)target).getXmlAttributes();
       //we have attributes and target does not have attribute, then we need to delete our attibute
       if(attributes != null && (targetAttributes == null || targetAttributes.length == 0)) {
           for(int i = 0; i < attributes.length; i++) {
               attributes[i].setValue(null);
           }
       }
       
       int attrCount = 0;
       
       //if we have attributes and target also has attribute
       if(attributes != null) {
           for(int i = 0; i < attributes.length; i++) {
               XMLAttribute attr = attributes[i];
               XMLAttribute targetAttr = null;
               if(i < targetAttributes.length) {
                    targetAttr = targetAttributes[i];
               }
               
               //if target attribute is null, then remove our attribute
               if(targetAttr == null) {
                   attr.setValue(null);
               } else if(targetAttr.getQualifiedName().equals(attr.getQualifiedName())) {
                   //if target attribute is same as our attrbute and it has new value then update
                   //our attribute value
                   String targetValue = targetAttr.getValue();
                   String value = attr.getValue();
                   
                   //if target attribute value is different then our current value
                   if (targetValue == null) {
                       if(value != null) {
                    	   this.setAttribute(i, attr.getQualifiedName(), null);
                       }
                   } else if(!targetValue.equals(value)) {
                	   this.setAttribute(i, attr.getQualifiedName(), targetValue);
                   } 
               }
               
               attrCount++;
           }
           
           //now need to add any new attributes which might have been added in target
           if(attrCount < targetAttributes.length) {
               for(int j = attrCount; j < targetAttributes.length; j++) {
                   XMLAttribute targetAttr = targetAttributes[j];
                   if(targetAttr != null) {
                       this.setAttribute(targetAttr.getQualifiedName(), targetAttr.getValue());
                   }
               }
           }
       }
       
       //now merge child nodes recursively
       super.merge(target);
   }
   
   /**
     * first looks for existing prefix for given namespace. If exist then return this first prefix
     * otherwise create a new unique prefix set it to the map of prefix to namespace and return it.
     * A preferredPrefix is used for the new created prefix if it is not used by other namespaces
     * 
     * @return new prefix.
     */
    public String createAndSetNamespacePrefix(String namespaceURI, String preferredPrefix) {
        String prefix = getNamespacePrefix(namespaceURI);
        if (prefix != null && !prefix.equals("")) {
            return prefix;
        }

        Map prefixToNs = getNamespaces();
        if (!prefixToNs.containsKey(preferredPrefix)) {
            setNamespace(preferredPrefix, namespaceURI);
            return preferredPrefix;
        }

        int prefixCounter = 0;

        String prefixStr = "ns";
        while (true) {
            prefix = prefixStr + prefixCounter;
            if (!prefixToNs.containsKey(prefix)) {
                setNamespace(prefix, namespaceURI);
                break;
            }
            prefixCounter++;
        }

        return prefix;
    }
   
}
