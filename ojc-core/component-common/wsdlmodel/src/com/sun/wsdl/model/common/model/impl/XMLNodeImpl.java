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

package com.sun.wsdl.model.common.model.impl;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.xml.sax.Locator;
import org.xml.sax.helpers.LocatorImpl;

import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.XMLNode;
import com.sun.wsdl.model.common.model.XMLNodeEvent;
import com.sun.wsdl.model.common.model.XMLNodeListener;

/**
 * Implements a XML node.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class XMLNodeImpl implements XMLNode, Comparable {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 8572740185518294034L;
    
    /** Natural (as in creation) sequence order
     * NATURAL_SEQUENCE_ORDER means add a child at the end of child list. 
     * */
    private static final int NATURAL_SEQUENCE_ORDER = Integer.MAX_VALUE;
    
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
    
    /** Holds the logical sibling sequence order: lower number than this means
     * add this child before any child who have higher sibling sequence.
     *  
     */
    private int siblingSequenceOrder = NATURAL_SEQUENCE_ORDER;
    
    
    /** Holds flag whether CDATA form is to be used. */
    protected boolean cdataForm = false;
    
    /** Holds owner document. */
    private XMLDocument ownerDocument;
    
    
    /** A list of event listeners for this component. */
    protected Set listenerSet = new HashSet();

    protected PropertyChangeSupport pSupport = new PropertyChangeSupport(this);
    
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
		addChildAtTheEnd(c, NATURAL_SEQUENCE_ORDER);
	}
	
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
    	//NATURAL_SEQUENCE_ORDER dictates that this child
    	//should be added at the end
        superAddChild(NATURAL_SEQUENCE_ORDER, c);
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public final void addChildAtTheEnd(XMLNode c, int logicalSiblingOrder) {
    	superAddChildAtTheEnd(c, logicalSiblingOrder);
    }
    
 
    
    /** @see XMLNode#addChild(int, XMLNode)
     */
    public void addChild(int s, XMLNode c) {
        superAddChild(s, c);
    }
    
    //TODO: this is called from bpel
    //see if it makes sense to make it call superAddChild
    //which does the sorting
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
        
        //this is still need so that if two
        //node have same sibling sequence
        //ex: multiple messages
        //then we swap them.
        //this is because
        //later after setting sibling sequence
        //and sorting may not have effect of changing postion
        //of the node.
        
        nodeChildren.set(indexA, b);
        nodeChildren.set(indexB, a);
        //end still needs
        
        //set the sibling sequence
        int aSeq = a.getSiblingSequenceOrder();
        int bSeq = b.getSiblingSequenceOrder();
        
        a.setSiblingSequenceOrder(bSeq);
        b.setSiblingSequenceOrder(aSeq);
        
        Collections.sort(this.nodeChildren);
        return true;
    }

    /** Adds a child of the element.
     * @param   s   logical Sibling sequence order.
     * @param   c   Child.
     */
    private void superAddChild(int s, XMLNode c) {
    	boolean nodeAdded = false;
        
        if (null == nodeChildren) {
            nodeChildren = new ArrayList();
            nodeChildren.add(c);
            c.setParent(this);
            nodeAdded = true;
        } else if (!nodeChildren.contains(c)) {
        	//add child at the end. later based on sibling sequence order it will be sorted
        	nodeChildren.add(c);
            c.setParent(this);
            nodeAdded = true;
            
        }
        
        //fire event
        if(nodeAdded) {
        	//always set logical sibling sequence order 
        	c.setSiblingSequenceOrder(s);
    		//sort since s can be anything
    		Collections.sort(nodeChildren);
        	
        	
        	fireXMLNodeAdded(c);
    	}
    }
    
    /** Always adds a child node at the end of collection.
     * @param   s   logical Sibling sequence order which determines where
     * this node should be added in a parent node. logical sibling sequence order
     * can come as manadated by schema.
     * @param   c   Child.
     */
    private void superAddChildAtTheEnd(XMLNode c, int s) {
    	boolean nodeAdded = false;
    	//set the logical sibiling order
    	c.setSiblingSequenceOrder(s);
    	
        if (null == nodeChildren) {
            nodeChildren = new ArrayList();
            nodeChildren.add(c);
            c.setParent(this);
            nodeAdded = true;
        } else if(!nodeChildren.contains(c)) {
        	c.setParent(this);
        	//always add at the end
        	nodeChildren.add(c);
        	nodeAdded = true;
        }
        
        if(nodeAdded) {
        	fireXMLNodeAdded(c);
        }
    }
    
    	
    /** Replaces a child of the element.
     * @param   o   Old child; <code>null</code> implies addition.
     * @param   n   New child; <code>null</code> implies deletion.
     */
    protected void replaceChild(XMLNode o, XMLNode n) {
    	if(o != null) {
    		replaceChild(o.getSiblingSequenceOrder(), o, n);
    	} else {
    		replaceChild(NATURAL_SEQUENCE_ORDER, o, n);
    	}
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
    
    /** @see com.sun.wsdl.model.common.model.XMLCharacterData#isCDATAForm
     */
    public boolean isCDATAForm() {
        return cdataForm;
    }
    
    /** @see com.sun.wsdl.model.common.model.XMLCharacterData#setCDATAForm
     */
    public void setCDATAForm(boolean useCDATA) {
        cdataForm = useCDATA;
    }
    
    /**
     * @see XMLNode#getXPath
     */
    public String getXPath() {
        String parentXPath = "";

/*        if ((getParent() != null) && !(getParent() instanceof XMLDocument)) {
            parentXPath = getParent().getXPath();
        }
*/
        return parentXPath + "/" + getLocalXPathName();
    }
    
    /**
     * Gets the local XPath name.
     * @return  Local XPath name.
     */
    private String getLocalXPathName() {
/*        Class[] interfaceSearchOrder = new Class[] {
            null,
            Activity.class,
            XMLElement.class,
            XMLNode.class
        };
        String xpName = getLocalName();
        try {
            String interfaceName = getClass().getName();
            interfaceName = interfaceName.substring(interfaceName.lastIndexOf('.') + 1);
            if (interfaceName.endsWith("Impl")) {
                interfaceName = interfaceName.substring(0, interfaceName.length() - 4);
            }
            Class[] interfaces = getClass().getInterfaces();
            if ((interfaces != null) && (interfaces.length > 0)) {
                
                // Find the corresponding interface to the implementation class
                Class interfaceClass = null;
                for (int i = 0; i < interfaces.length; i++) {
                    String iname = interfaces[i].getName();
                    if (iname.substring(iname.lastIndexOf('.') + 1).equals(interfaceName)) {
                        interfaceClass = interfaces[i];
                        break;
                    }
                }
                
                // Now find a getter in the parent class that returns that type
                if ((interfaceClass != null) && (getParent() != null)) {
                    interfaceSearchOrder[0] = interfaceClass;
                    Method[] parentMethods = getParent().getClass().getDeclaredMethods();
                    if ((parentMethods != null) && (parentMethods.length > 0)) {
                        Method getter = null;
                        
                        // Find a getter that returns the interface in above search order
                        for (int intf = 0; ((null == getter) && (intf < interfaceSearchOrder.length)); intf++) {
                            for (int i = 0; i < parentMethods.length; i++) {
                                if (parentMethods[i].getName().startsWith("get")
                                        && parentMethods[i].getReturnType().equals(interfaceSearchOrder[intf])) {
                                    getter = parentMethods[i];
                                    break;
                                }
                            }
                        }
                        
                        // Now derive the proper bean name
                        if (getter != null) {
                            xpName = getter.getName().substring(3);
                            Class[] params = getter.getParameterTypes();
                            if ((params != null) && (1 == params.length) && params[0].equals(int.class)) {
                                Method indexOfMethod = getParent().getClass().getMethod("indexOf" + xpName, 
                                                                                        new Class[] {XMLNode.class});
                                if (indexOfMethod != null) {
                                    Integer index = (Integer) indexOfMethod.invoke(getParent(), new Object[] {this});
                                    xpName = pluralize(xpName) + "[" + (index.intValue() + 1) + "]";
                                }
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            // Do nothing
        }

        // Lowercase the first letter
        return xpName.substring(0, 1).toLowerCase() + xpName.substring(1);
 */ 
        return "";
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
       
       public int compareTo(Object arg0) {
       		if(arg0 instanceof XMLNode) {
       			XMLNode source = (XMLNode) arg0;
       			if(this.getSiblingSequenceOrder() < source.getSiblingSequenceOrder()) {
       				return -1;
       			} else if(this.getSiblingSequenceOrder() == source.getSiblingSequenceOrder()) {
       				return 0;
       			} else {
       				return 1;
       			}
       		}
       		
       		return -1;
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
