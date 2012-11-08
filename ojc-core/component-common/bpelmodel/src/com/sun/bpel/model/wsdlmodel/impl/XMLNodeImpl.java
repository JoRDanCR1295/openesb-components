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
 * @(#)XMLNodeImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.wsdlmodel.impl;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.EventListenerList;

import org.xml.sax.Locator;
import org.xml.sax.helpers.LocatorImpl;

import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.model.XMLNodeEvent;
import com.sun.bpel.xml.common.model.XMLNodeListener;

/**
 * Implements a XML node.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class XMLNodeImpl implements XMLNode {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 8572740185518294034L;
    
    /** Natural (as in creation) sequence order */
    private static final int NATURAL_SEQUENCE_ORDER = -1;
    
    /** Holds value of property localName. */
    private String localName = "";
    
    /** Holds value of property qualifiedName. */
    private String qualifiedName = "";
    
    /** Holds the value */
    private String value = null;
    
    /** Holds value of property leadingWhitespace. */
    private char[] leadingWhitespace = null;
    
    /** Holds value of property trailingWhitespace. */
    private char[] trailingWhitespace = null;
    
    /** Holds value of locator for XML event. */
    private Locator locator;
    
    /** Holds parent node. */
    private XMLNode parentNode = null;
    
    /** Holds children nodes. */
    protected List nodeChildren = null;
    
    /** Holds the flag whether the qualified name has been explicitly specified */
    protected boolean qNameExplicitlySet = false;
    
    /** Holds the namespace owning this node */
    protected String owningNamespace = null;
    
    /** Holds the owning namespace default prefix */
    protected String owningNamespacePrefix = null;
    
    /** Holds the sibling sequence order: lower number up front;
     *  -1 means leave wherever it is.
     */
    private int siblingSequenceOrder = NATURAL_SEQUENCE_ORDER;
    
    /** Holds flag whether CDATA form is to be used. */
    protected boolean cdataForm = false;
    
    /** Holds owner document. */
    private XMLDocument ownerDocument;
    
    
    /** A list of event listeners for this component. */
    protected Set listenerSet = new HashSet();

    protected PropertyChangeSupport pSupport = new PropertyChangeSupport(this);
    
    /**
     * Only one <code>ChangeEvent</code> is needed per instance since the
     * event's only interesting property is the immutable source, which
     * is the xml element
     */
    private transient ChangeEvent changeEvent = null;
    
    protected EventListenerList listenerList = new EventListenerList();
   
    /** Creates a new instance of XMLNodeImpl */
    protected XMLNodeImpl() {
    }
    
    /** Creates a new instance of XMLNodeImpl */
    protected XMLNodeImpl(XMLDocument d) {
    	setOwnerDocument(d);
    }
    
    /** @see XMLElement#getOwnerDocument
     */
    public XMLDocument getOwnerDocument() {
        return ownerDocument;
    }
    
    /** @see XMLElement#setOwnerDocument
     */
    public void setOwnerDocument(XMLDocument d) {
        ownerDocument = d;
    }
    
    /** @see XMLNode#getLocalName
     */
    public String getLocalName() {
        return localName;
    }
    
    /** @see XMLNode#setLocalName
     */
    public void setLocalName(String localName) {
        this.localName = localName;
    }
    
    /** @see XMLNode#getQualifiedName
     */
    public String getQualifiedName() {
        if (((null == qualifiedName) || (qualifiedName.length() == 0))
                 && (localName != null)) {
            qualifiedName = localName;
        }
        return qualifiedName;
    }
    
    /** @see XMLNode#setQualifiedName
     */
    public void setQualifiedName(String qualifiedName) {
    	String oldQName = this.qualifiedName;
    	
        qNameExplicitlySet = true;
        this.qualifiedName = qualifiedName;
        int cIndex = qualifiedName.indexOf(':');
        localName = (cIndex != -1) ? qualifiedName.substring(cIndex + 1) : qualifiedName;
        
        if(getOwnerDocument() != null && getOwnerDocument().isEnableEvents()) {
	        PropertyChangeEvent pEvent = new PropertyChangeEvent(this, 
	        													 PROP_QUALIFIED_NAME,
																 oldQName,
																 qualifiedName);
	        firePropertyChangeEvent(pEvent);
        }
    }
    
    /** @see XMLNode#getValue
     */
    public String getValue() {
        return value;
    }
    
    /** @see XMLNode#setValue
     */
    public void setValue(String v) {
        value = v;
    }
    
    /** @see XMLNode#getLeadingWhitespace
     */
    public char[] getLeadingWhitespace() {
        return leadingWhitespace;
    }
    
    /** @see XMLNode#setLeadingWhitespace
     */
    public void setLeadingWhitespace(char[] leadingWhitespace) {
        this.leadingWhitespace = leadingWhitespace;
    }
    
    /** @see XMLNode#getTrailingWhitespace
     */
    public char[] getTrailingWhitespace() {
        return trailingWhitespace;
    }
    
    /** @see XMLNode#setTrailingWhitespace
     */
    public void setTrailingWhitespace(char[] trailingWhitespace) {
        this.trailingWhitespace = trailingWhitespace;
    }
    
    /**
     * @see XMLNode#getLocator
     */
    public Locator getLocator() {
        return locator;
    }
    
    /**
     * @see XMLNode#setLocator
     */
    public void setLocator(Locator locator) {
        if (locator != null) {
            this.locator = new LocatorImpl(locator);
        }
    }
    
    /** @see XMLNode#getParent
     */
    public XMLNode getParent() {
        return parentNode;
    }
    
    /** @see XMLNode#setParent
     */
    public void setParent(XMLNode p) {
        parentNode = p;
    }
    
    /** @see XMLNode#hasChildren
     */
    public boolean hasChildren() {
        return ((nodeChildren != null) && (nodeChildren.size() > 0));
    }
    
    /** @see XMLNode#getChildren
     */
    public List getChildren() {
        return (nodeChildren != null ? Collections.unmodifiableList(nodeChildren) : null);
    }
    
    /**
	 * add a node at the end of a collection.
	 * This will be called while parsing this XMLNode.
	 * This method should always add this child at the end of 
	 * the collection because we want to preserve the order
	 * in which this node occured in the xml.
	 * This method should also set the sibliing sequence order
	 * before adding to collection if it different than the
	 * default Interger.MAX_VALUE
	 * @param c
	 */
	public void addChildAtTail(XMLNode c) {
		addChild(c);
	}
	
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        superAddChild(NATURAL_SEQUENCE_ORDER, c);
    }
    
    /** @see XMLNode#addChild(int, XMLNode)
     */
    public void addChild(int s, XMLNode c) {
        superAddChild(s, c);
    }
    
    /** @see XMLNode#addChild(int, XMLNode, XMLNode)
     */
    public void addChild(int s, XMLNode b, XMLNode c) {
        if ((null == nodeChildren) || (null == b)) {
            superAddChild(s, c);
        } else {
            int index = nodeChildren.indexOf(b);
            nodeChildren.add(index, c);
            c.setSiblingSequenceOrder(s);
            c.setParent(this);
            
            //fire event
            fireXMLNodeAdded(c);
            
            fireStateChanged();
        }
    }

   /** @see XMLNode#indexOfChild(XMLNode)
    */
    public int indexOfChild(XMLNode node) {
        if (hasChildren()) {
            return nodeChildren.indexOf(node);
        }
        return -1;
    }
    
   /** @see XMLNode#swapChild(XMLNode, XMLNode)
    */
    public boolean swapChildren(XMLNode a, XMLNode b) {
        int indexA = nodeChildren.indexOf(a);
        int indexB = nodeChildren.indexOf(b);
        if (indexA == -1 || indexB == -1) {
            return false;
        }
        nodeChildren.set(indexA, b);
        nodeChildren.set(indexB, a);
        return true;
    }

    /** Adds a child of the element.
     * @param   s   Sibling sequence order.
     * @param   c   Child.
     */
    private void superAddChild(int s, XMLNode c) {
        c.setSiblingSequenceOrder(s);
        if (null == nodeChildren) {
            nodeChildren = new ArrayList();
            nodeChildren.add(c);
            c.setParent(this);
        } else if (!nodeChildren.contains(c)) {
            int le = -1;
            boolean addAtEnd = true;
            if (s != NATURAL_SEQUENCE_ORDER) {
                for (int i = 0, n = nodeChildren.size(); i < n; i++) {
                    XMLNode node = (XMLNode) nodeChildren.get(i);
                    if (NATURAL_SEQUENCE_ORDER
                            == node.getSiblingSequenceOrder()) {
                        addAtEnd = true;
                        continue;
                    } else if (s >= node.getSiblingSequenceOrder()) {
                        le = i;
                        addAtEnd = false;
                    } else {
                        addAtEnd = false;
                        break;
                    }
                }
            }
            if (addAtEnd) {
                nodeChildren.add(c);
            } else {
                nodeChildren.add(le + 1, c);    // insert after
            }
            c.setParent(this);
            
        }
        
        //fire event
        fireXMLNodeAdded(c);
        
        fireStateChanged();
    }
    
    /** Replaces a child of the element.
     * @param   o   Old child; <code>null</code> implies addition.
     * @param   n   New child; <code>null</code> implies deletion.
     */
    protected void replaceChild(XMLNode o, XMLNode n) {
        replaceChild(NATURAL_SEQUENCE_ORDER, o, n);
    }
    
    /** Replaces a child of the element.
     * @param   s   Sibling sequence order.
     * @param   o   Old child; <code>null</code> implies addition.
     * @param   n   New child; <code>null</code> implies deletion.
     */
    protected void replaceChild(int s, XMLNode o, XMLNode n) {
        if ((null == o) && (n != null)) {
            superAddChild(s, n);
        } else if ((null == n) && (o != null)) {
            superRemoveChild(o);
        } else if ((o != null) && (n != null)) {
            //nodeChildren.set(nodeChildren.indexOf(o), n);
            //n.setSiblingSequenceOrder(s);
            //n.setParent(this);
        	//use add and remove so that events will get fired
        	superRemoveChild(o);
        	superAddChild(s, n);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        superRemoveChild(c);
    }
    
    /** Removes a child of the element.
     * @param   c   Child.
     */
    private void superRemoveChild(XMLNode c) {
        if (nodeChildren != null) {
            nodeChildren.remove(c);
            
            //fire event
            fireXMLNodeRemoved(c);
            
            fireStateChanged();
        }
    }    
    
    /** @see XMLNode#getSiblingSequenceOrder
     */
    public int getSiblingSequenceOrder() {
        return siblingSequenceOrder;
    }
    
    /** @see XMLNode#setSiblingSequenceOrder
     */
    public void setSiblingSequenceOrder(int o) {
        siblingSequenceOrder = o;
    }
 
    /** @see com.sun.bpel.xml.common.model.XMLCharacterData#isCDATAForm
     */
    public boolean isCDATAForm() {
        return cdataForm;
    }
    
    /** @see com.sun.bpel.xml.common.model.XMLCharacterData#setCDATAForm
     */
    public void setCDATAForm(boolean useCDATA) {
        cdataForm = useCDATA;
    }
    
    /**
     * @see XMLNode#getXPath
     */
    public String getXPath() {
        String parentXPath = "";

        if ((getParent() != null) && !(getParent() instanceof XMLDocument)) {
            parentXPath = getParent().getXPath();
        }

        return parentXPath + "/" + getLocalXPathName();
    }
    
    /**
     * Gets the local XPath name.
     * @return  Local XPath name.
     */
    private String getLocalXPathName() {
    	StringBuffer localXPathBuf = new StringBuffer(50);
        String qName = getQualifiedName();
        String localName = getLocalName ();
        if (qName.equals(localName)) {
            qName = BPEL_NS_PREFIX + ":" + qName;
        }
    	localXPathBuf.append(qName);
    	XMLNode parent = getParent();
    	if(parent != null) {
    		List children = parent.getChildren();
    		if(children.size() > 1) {
    			int total = children.size();
                int ind = 0;
                int j = 0;
                for (int i=0; i<total; i++) {
                    XMLNode child = (XMLNode) children.get(i);
                    if (child == this) {
                        ind = j;
                        j ++;
                    } else if (child.getLocalName().equals(getLocalName())) {
                        j ++;
                    }
                }
                if (j == 1) {
                    return localXPathBuf.toString();
                } else {
                    localXPathBuf.append("[");
                    localXPathBuf.append(ind+1);
                    localXPathBuf.append("]");                    
                }
    		}
    	}
        return localXPathBuf.toString();
    }
    
    /**
     * Pluralize a class name used by BPEL and WSDL.
     * @param   name    Name to pluralize.
     * @return  Pluralized name.
     */
    private String pluralize(String name) {
        String pname;
        if (name.endsWith("ch") || name.endsWith("as") || name.endsWith("ss")) {
            pname = name + "es";
        } else if (name.endsWith("ty") || name.endsWith("py") || name.endsWith("ly")) {
            pname = name.substring(0, name.length() - 1) + "ies";
        } else {
            pname = name + "s";
        }
        return pname;
    }
    
    /**
	* Add a PropertyChangeListener
 	  @param l PropertyChangeListener
	*/
    public void addPropertyChangeListener(PropertyChangeListener l) {
    	this.pSupport.addPropertyChangeListener(l);
    }
    
    /**
	* Remove a PropertyChangeListener
 	  @param l PropertyChangeListener
	*/
    public void removePropertyChangeListener(PropertyChangeListener l) {
    	this.pSupport.removePropertyChangeListener(l);
    }
    
    
    /**
     * Adds the specified <code>ChangeListener</code> to the xmlelement.
     *
     * @param l the <code>ChangeListener</code> to add
     */
    public void addChangeListener(ChangeListener l) {
        listenerList.add(ChangeListener.class, l);
    }
    
    /**
     * Removes a <code>ChangeListener</code> from the xmlelement.
     *
     * @param l the <code>ChangeListener</code> to remove
     */
    public void removeChangeListener(ChangeListener l) {
        listenerList.remove(ChangeListener.class, l);
    }
    
    /**
     * Added a <code> XMLNodeListener </code> to the XMLNode. 
     * @param l the <code> XMLNodeListener </code> to add.
     */
    public void addXMLNodeListener(XMLNodeListener  l) {
    	this.listenerSet.add(l);
    }
    
    /**
     * Removed a a <code> XMLNodeListener </code> from the XMLNode.
     * @param l the <code> XMLNodeListener </code> to remove.
     */
    public void removeXMLNodeListener(XMLNodeListener l) {
    	this.listenerSet.remove(l);
    }
    
    /**
     * Notifies all listeners that have registered interest in
     * <code>ChangeEvent</code>s.
     * The event instance 
     * is created if necessary.
     *
     * @see EventListenerList
     */
    protected void fireStateChanged() {
        // Guaranteed to return a non-null array
        Object[] listeners = listenerList.getListenerList();
        // Process the listeners last to first, notifying
        // those that are interested in this event
        for (int i = listeners.length - 2; i >= 0; i -= 2) {
            if (listeners[i] == ChangeListener.class) {
                // Lazily create the event:
                if (changeEvent == null) {
                    changeEvent = new ChangeEvent(this);
                }
                ((ChangeListener) listeners[i + 1]).stateChanged(changeEvent);
            }          
        }
    }
    
    protected void fireXMLNodeAdded(XMLNode child) {
    	XMLDocument owner = getOwnerDocument();
    	if(owner != null && !owner.isEnableEvents()) {
    		return;
    	}
    
    	XMLNodeEvent xmlElementEvent = new XMLNodeEvent(this, child);
         
        Iterator it = listenerSet.iterator();
        while(it.hasNext()) {
	      	XMLNodeListener listener = (XMLNodeListener) it.next();
	      	listener.nodeAdded(xmlElementEvent);
        }
      
      
      //propogate event to parent
      if(xmlElementEvent != null 
      	 && this.getParent() != null) {
      		this.getParent().fireXMLNodeAdded(xmlElementEvent);
      }
   }
   
    
    protected void fireXMLNodeRemoved(XMLNode child) {
    	XMLDocument owner = getOwnerDocument();
    	if(owner != null && !owner.isEnableEvents()) {
    		return;
    	}
    
    	
    	XMLNodeEvent xmlElementEvent = new XMLNodeEvent(this, child);
        Iterator it = listenerSet.iterator();
        while(it.hasNext()) {
	      	XMLNodeListener listener = (XMLNodeListener) it.next();
	      	listener.nodeRemoved(xmlElementEvent);
        }

          
        //propogate event to parent
        if(xmlElementEvent != null
        	&& this.getParent() != null) {
        		this.getParent().fireXMLNodeRemoved(xmlElementEvent);
        }
     }
    
    protected void fireXMLNodeValueChanged(String oldValue, String newValue) {
    	XMLDocument owner = getOwnerDocument();
    	if(owner != null && !owner.isEnableEvents()) {
    		return;
    	}
    
    	PropertyChangeEvent pEvent = new PropertyChangeEvent(this, "value", oldValue, newValue);
        Iterator it = listenerSet.iterator();
        while(it.hasNext()) {
	      	XMLNodeListener listener = (XMLNodeListener) it.next();
	      	listener.nodeValueChanged(pEvent);
        }

          
        //propogate event to parent
        if(pEvent != null
        	&& this.getParent() != null) {
        		this.getParent().fireXMLNodeValueChanged(pEvent);
        }
     }
    
    public void fireXMLNodeAdded(XMLNodeEvent evt) {
	    	Iterator it = listenerSet.iterator();
	        while(it.hasNext()) {
		      	XMLNodeListener listener = (XMLNodeListener) it.next();
		      	listener.nodeAdded(evt);
	        }
    	        
    	    
    	    //propogate event to parent
            if(evt != null
            	&& this.getParent() != null) {
            		this.getParent().fireXMLNodeAdded(evt);
            }
     }
        
       public void fireXMLNodeRemoved(XMLNodeEvent evt) {
	       	Iterator it = listenerSet.iterator();
	        while(it.hasNext()) {
		      	XMLNodeListener listener = (XMLNodeListener) it.next();
		      	listener.nodeRemoved(evt);
	        }
    	        
    	    
    	    //propogate event to parent
            if(evt != null
            	&& this.getParent() != null) {
            		this.getParent().fireXMLNodeRemoved(evt);
            }
       }
       
       public void fireXMLNodeValueChanged(PropertyChangeEvent evt) {
		       	Iterator it = listenerSet.iterator();
		        while(it.hasNext()) {
			      	XMLNodeListener listener = (XMLNodeListener) it.next();
			      	listener.nodeValueChanged(evt);
		        }
           
		    
		    //propogate event to parent
	        if(evt != null
	        	&& this.getParent() != null) {
	        		this.getParent().fireXMLNodeValueChanged(evt);
	        }
       }
       
       public void firePropertyChangeEvent(PropertyChangeEvent evt) {
	    	XMLDocument owner = getOwnerDocument();
	    	if(owner != null && !owner.isEnableEvents()) {
	    		return;
	    	}
    
       		pSupport.firePropertyChange(evt);
	   
	    
		    //propogate event to parent
		    if(evt != null
		    	&& this.getParent() != null) {
		    		this.getParent().firePropertyChangeEvent(evt);
		    }
       }
       
       
       
       public void merge(XMLNode target) {
           List targetChildren = target.getChildren();
           int targetChildrenSize = -1;
           if(targetChildren != null) {
        	   targetChildrenSize = targetChildren.size();
           }
           List children = this.getChildren();
           int childCounter = 0;
           if(children != null) {
        	   
	           for(int i = 0; i < children.size(); i++) {
	               XMLNode child = (XMLNode) children.get(i);
	               XMLNode targetChild = null;
	               if(i < targetChildrenSize) {
	                  targetChild = (XMLNode) targetChildren.get(i);
	               }
	               
	               //if target does not have a child then our child at this index needs to be removed
	               if(targetChild == null) {
	                   this.removeChild(child);
	               } else if(child.getQualifiedName().equals(targetChild.getQualifiedName()) ) {
	                   //if same element merge them
	                   child.merge(targetChild);
	               } else {
	                  //if different elememt in target we need to remove our child
	            	   this.removeChild(child);
	               }
	               childCounter++;
	           }
           }
           
           if(targetChildren != null) {
	           //now we need to add any new child target may have
	           if(childCounter < targetChildren.size()) {
	                for(int j = childCounter; j < targetChildren.size(); j++) {
	                    XMLNode targetChild = (XMLNode) targetChildren.get(j);
	                    this.addChild(targetChild);
	                }
	           }
           }
       }
}
