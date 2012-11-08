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
 * @(#)XMLNode.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model;

import com.sun.wsdl.model.common.visitor.Visitor;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;
import java.util.List;
import org.xml.sax.Locator;

/**
 * Describes a XML node (such as Attribute, Element, Comment, Text).
 *
 * @author Sun Microsystems
 * @version 
 */
public interface XMLNode extends Serializable {
     
	public static final String PROP_QUALIFIED_NAME = "PROP_QUALIFIED_NAME";
	
	public static final String PROP_LOCAL_NAME = "PROP_QUALIFIED_NAME";
    
    public static final String BPEL_NS_PREFIX = "bpws";
    
    public static final String BPEL_NS_URI = "http://docs.oasis-open.org/wsbpel/2.0/process/executable";
	
    /** Getter for property localName.
     * @return Value of property localName.
     */
    String getLocalName();
    
    /** Setter for property localName.
     * @param   localName   Value of property localName.
     */
    void setLocalName(String localName);
    
    /** Getter for property qualifiedName.
     * @return Value of property qualifiedName.
     *
     */
    String getQualifiedName();
    
    /** Setter for property qualifiedName.
     * @param qualifiedName New value of property qualifiedName.
     *
     */
    void setQualifiedName(String qualifiedName);
    
    /** Getter for property value.
     * @return Value of property value.
     *
     */
    String getValue();
    
    /** Setter for property value.
     * @param value New value of property value.
     *
     */
    void setValue(String value);
    
    /** Getter for property leadingWhitespace.
     * @return Value of property leadingWhitespace.
     *
     */
    char[] getLeadingWhitespace();
    
    /** Setter for property leadingWhitespace.
     * @param leadingWhitespace New value of property leadingWhitespace.
     *
     */
    void setLeadingWhitespace(char[] leadingWhitespace);
    
    /** Getter for property trailingWhitespace.
     * @return Value of property trailingWhitespace.
     *
     */
    char[] getTrailingWhitespace();
    
    /** Setter for property trailingWhitespace.
     * @param trailingWhitespace New value of property trailingWhitespace.
     *
     */
    void setTrailingWhitespace(char[] trailingWhitespace);
    
    /** Getter for property locator.
     * @return Locator for XML event.
     */
    Locator getLocator();
    
    /** Setter for property locator.
     * @param locator Locator for XML event.
     */
    void setLocator(Locator locator);
    
    /** Getter for the parent of this XML node.
     * @return  Parent of node.
     */
    XMLNode getParent();
    
    /** Setter for the parent of this XML node.
     * @param   p   Parent of node.
     */
    void setParent(XMLNode p);
    
    /** Test if node has any children.
     * @return  <code>true</code> if there are children.
     */
    boolean hasChildren();
    
    /** Gets node children as a list.
     * @return  Non-modifiable list of node children.
     */
    List getChildren();
   
    /** Adds a child of the element to the end of children collection.
     * @param   c   Child.
     */
    void addChildAtTail(XMLNode c);
   
    
    /** Adds a child of the element.
     * @param   c   Child.
     */
    void addChild(XMLNode c);
    
    /** Adds a child of the element.
     * @param   s   Sibling sequence order.
     * @param   c   Child.
     */
    void addChild(int s, XMLNode c);
    
    /** Inserts a child of the element before given child.
     * @param   s   Sibling sequence order.
     * @param   b   Child to insert before.
     * @param   c   Child.
     */
    void addChild(int s, XMLNode b, XMLNode c);
    
    /** Removes a child of the element
     * @param   c   Child.
     */
    void removeChild(XMLNode c);
    
    /** Gets the logical sibling sequence order for this node.
     *  This is logical sequence order in that lower number node are ahead
     *  of higher number node in a collection.
     * @return  Sibling sequence order: lower number up front; Integer.MAX_VALUE
     * means this node is added at the end of a collection.
     */
    int getSiblingSequenceOrder();
    
    /** Sets the logical sibling sequence order for this node.
     *  This is logical sequence order in that lower number node are ahead
     *  of higher number node in a collection.
     * @param   o   Sibling sequence order: lower number up front; Integer.MAX_VALUE
     * means this node is added at the end of a collection.
     */
    void setSiblingSequenceOrder(int o);
   
    
    /** Accepts a visitor to perform some work on the element.
     * @param   v   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    boolean accept(Visitor v);
    
    /**
     * Describe <code>indexOfChild</code> method here.
     *
     * @param node a <code>XMLNode</code> value
     * @return an <code>int</code> value
     */
    int indexOfChild(XMLNode node);

    /**
     * both a and b should be children of this node
     * if successfully finds the children swaps and returns true.
     * @param a a <code>XMLNode</code> value
     * @param b a <code>XMLNode</code> value
     * @return a <code>boolean</code> value
     */
    boolean swapChildren(XMLNode a, XMLNode b);
    
    /** Gets the XPath location for this node.
     * @return  XPath location for this node.
     */
    String getXPath();
    
    /**
	* Add a PropertyChangeListener
 	  @param l PropertyChangeListener
	*/
    void addPropertyChangeListener(PropertyChangeListener l);
    
    /**
	* Remove a PropertyChangeListener
 	  @param l PropertyChangeListener
	*/
    void removePropertyChangeListener(PropertyChangeListener l);
    
    /**
     * Added a <code> XMLNodeListener </code> to the XMLNode. 
     * @param l the <code> XMLNodeListener </code> to add.
     */
    void addXMLNodeListener(XMLNodeListener l);
    
    /**
     * Removed a a <code> XMLNodeListener </code> from the XMLNode.
     * @param l the <code> XMLNodeListener </code> to remove.
     */
    void removeXMLNodeListener(XMLNodeListener l);
    
    void fireXMLNodeAdded(XMLNodeEvent evt);
    
    void fireXMLNodeRemoved(XMLNodeEvent evt);
    
    void fireXMLNodeValueChanged(PropertyChangeEvent evt);
   
    void firePropertyChangeEvent(PropertyChangeEvent evt); 
    
    void merge(XMLNode source);
}
