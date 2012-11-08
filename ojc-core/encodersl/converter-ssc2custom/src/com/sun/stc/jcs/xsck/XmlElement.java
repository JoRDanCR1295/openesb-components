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
 * @(#)XmlElement.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.xsck;

import org.xml.sax.*;
import org.xml.sax.helpers.*;
import java.util.*;

/*
 * This class represents an XML element in a Schema -
 * Unlike DOM this class includes location information
 * in the form of an org.xml.sax.Locator, and namespace prefix mapping
 * information not available in DOM.  The former is needed to report error
 * locations when validating a Schema.  The latter information is needed to
 * interpret QName values of various Schema attributes, e.g. xsd:type.
 */

public class XmlElement
{ 
    String localName;
    String qName;
    String namespaceURI;
    HashMap prefixMapping;
    AttributesImpl attributes;
    Locator location;
    String text;

    XmlElement parent;
    XmlElement firstChild;
    XmlElement lastChild;
    XmlElement nextSibling;
    XmlElement prevSibling;

    public XmlElement (Locator location, String localName,
	String qName, String namespaceURI, Attributes attrs,
	HashMap prefixMapping)
    {
	// Note: I make copies of the data provided by sax - as it is
	// only guaranteed to be constant during the sax callback
	this.location = new LocatorImpl(location);
	this.localName = localName;
	this.qName = qName;
	this.namespaceURI = namespaceURI;
	this.prefixMapping = new HashMap();
	Iterator iter = prefixMapping.keySet().iterator();
	while (iter.hasNext())
	{
	    Object key = iter.next();
	    this.prefixMapping.put(key, prefixMapping.get(key));
	}
	this.attributes = new AttributesImpl();
	this.attributes.setAttributes(attrs);
    }

    // Append a child to the list of this element's children.  
    public void appendChild (XmlElement elem)
    {
	elem.parent = this;
	if (firstChild == null)
	{
	    firstChild = lastChild = elem;
	}
	else
	{
	    elem.nextSibling = null;
	    elem.prevSibling = lastChild;
	    lastChild.nextSibling = elem;
	    lastChild = elem;
	}
    }

    // Assign character data child of this element.
    public void putText (String text)
    {
	this.text = text;
    }

    // Get this element's parent element, or null if root.
    public XmlElement getParent ()
	{ return parent; }

    // Get this element's first child element, or null if empty.
    public XmlElement getFirstChild ()
	{ return firstChild; }

    // Get this element's last child element, or null if empty.
    public XmlElement getLastChild ()
	{ return lastChild; }

    // Get this element's last sibling element, or null if last.
    public XmlElement getNextSibling ()
	{ return nextSibling; }

    // Get this element's previous sibling element, or null if first.
    public XmlElement getPreviousSibling ()
	{ return prevSibling; }

    // Returns the source location of this element.
    public Locator getLocation ()
	{ return location; }

    // Returns the attributes of this element.
    public Attributes getAttributes ()
	{ return attributes; }

    /* Returns the namespace prefix mappings that exist in the context
     * of this element.
     */ 
    public HashMap getPrefixMapping ()
	{ return prefixMapping; }

    // Returns the non-qualified name of this element.
    public String getLocalName ()
	{ return localName; }

    // Returns the fully-qualified name of this element.
    public String getQName ()
	{ return qName; }

    // Returns the namespace of this element.
    public String getNamespaceURI ()
	{ return namespaceURI; }

    // Print this element and its contents as canonical XML.  
    static void print (StringBuffer buf, XmlElement elem, String indent)
    {
	if (indent.length() > 0)
	{
	    buf.append("\n");
	    buf.append(indent);
	}
	buf.append("<");
	buf.append(elem.qName);
	Iterator iter = elem.prefixMapping.keySet().iterator();
	while (iter.hasNext())
	{
	    String key = (String)iter.next();
	    String val = (String)elem.prefixMapping.get(key);
	    if (elem.parent != null)
	    {
		String pval = (String)elem.parent.prefixMapping.get(key);
		if (pval != null && pval.equals(val))
		    continue;
	    }
	    buf.append(" xmlns");
	    if (key.length() > 0)
	    {
		buf.append(":");
		buf.append(key);
	    }
	    buf.append("=\"");
	    normalize(buf, val, true);
	    buf.append("\"");
	}
	if (elem.attributes != null)
	{
	    int len = elem.attributes.getLength();
	    for (int i = 0; i < len; i++)
	    {
		String name = elem.attributes.getQName(i);
		String value = elem.attributes.getValue(i);
		buf.append(" ");
		buf.append(name);
		buf.append("=\"");
		normalize(buf, value, true);
		buf.append("\"");
	    }
	}
	if (elem.firstChild == null)
	{
	    if (elem.text != null)
	    {
		buf.append(">");
		normalize(buf, elem.text, false);
		buf.append("</");
		buf.append(elem.qName);
		buf.append(">");
	    }
	    else
	    {
		buf.append("/>");
	    }
	}
	else
	{
	    buf.append(">");
	    String newIndent = indent + " ";
	    for (XmlElement first1 = elem.firstChild;
		first1 != null; first1 = first1.nextSibling)
		{
		print(buf, first1, newIndent);
		if (first1 == elem.lastChild) break;
	    }
	    buf.append("\n");
	    buf.append(indent);
	    buf.append("</");
	    buf.append(elem.qName);
	    buf.append(">");
	}
    }

    // Normalizes the given string.
    static void normalize (StringBuffer str, String s, boolean canonical)
    {
	int len = (s != null) ? s.length() : 0;
	for ( int i = 0; i < len; i++ )
	{
	    char ch = s.charAt(i);
	    switch (ch)
	    {
	    case '<':  str.append("&lt;");   break;
	    case '>':  str.append("&gt;");   break;
	    case '&':  str.append("&amp;");  break;
	    case '"':  str.append("&quot;"); break;
	    case '\'': str.append("&apos;"); break;

	    case '\r':
	    case '\n':
		if ( canonical )
		{
		    str.append("&#");
		    str.append(Integer.toString(ch));
		    str.append(';');
		    break;
		}
		// else, default append char
	    default:
		str.append(ch);
	    }
	}
    }

    /* Resolve prefix to the corresponding namespace in the
     * context of this element.
     */ 
    public String resolvePrefix (String prefix)
    {
	return (String)prefixMapping.get(prefix);
    }

    // Resolve a QNAME namespace in the context of this element.  
    public String getNamespaceURI (String qName)
    {
	int colon = qName.indexOf(':');
	String prefix = "";
	if(colon > 0)
	    prefix = qName.substring(0, colon);
	return (String)prefixMapping.get(prefix);
    }

    // Print this element and its contents as canonical XML.  
    @Override
    public String toString ()
    {
	StringBuffer buf = new StringBuffer();
	print(buf, this, "");
	return buf.toString();
    }

    /* Returns the character data child of this element.  Returns null
     * if this element contains element content.
     */
    public String getText ()
    {
	return text;
    }
}
