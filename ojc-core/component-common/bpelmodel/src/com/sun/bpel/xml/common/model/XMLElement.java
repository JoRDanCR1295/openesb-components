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
 * @(#)XMLElement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common.model;

import com.sun.bpel.xml.NamespaceUtility;
import com.sun.bpel.xml.common.visitor.Visitor;

import java.util.List;
import java.util.Map;
import javax.swing.event.ChangeListener;
import javax.xml.namespace.QName;


/**
 * Describes a XML element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface XMLElement
    extends XMLNode {
    /**---------------Properties Start--------------------**/
	public static final String PROP_PREFIX_NAME_CHANGED = "PROP_PREFIX_NAME_CHANGED";

	/**---------------Properties End--------------------**/

    /** Namespace for SeeBeyond BPEL extensions */
    public static final String SBYNBPEL_EXTN_NAMESPACE
        = "http://sbynbpelextn.seebeyond.com/hawaii/5.0/SBYNBPELExtension/";

    /** Namespace prefix for SeeBeyond BPEL extensions */
    public static final String SBYNBPEL_EXTN_PREFIX = "sbynbpelex";

    /** Namespace for SeeBeyond BPEL runtime extensions */
    public static final String SBYNBPEL_RUNTIME_EXTN_NAMESPACE
        = "http://bpel.seebeyond.com/hawaii/5.0/privateExtension/runtime/";

    /** Namespace prefix for SeeBeyond BPEL runtime extensions */
    public static final String SBYNBPEL_RUNTIME_EXTN_PREFIX = "sbynruntime";

    /** Describes the well-known attributes for this element.
     */
    public interface WellKnownAttr {

        /** Attribute name indicating a default XML namespace. */
        public static final String XMLNS = "xmlns";

        /** Attribute name prefix indicating a specific namespace. */
        public static final String XMLNS_COLON = XMLNS + ":";

        /** Attribute name indicating a map of prefix to namespace. */
        public static final String PREFIX_TO_NAMESPACE_MAP = "PREFIX_TO_NAMESPACE_MAP";

    }

    /** Describes SeeBeyond private extension attributes.
     */
    public interface PrivateAttr {
        /** Attribute name for the line label immediately preceding an element: lineLabel */
        public static final String LINE_LABEL = "lineLabel";
    }

    /** QName object for SeeBeyond Private extension line label */
    public static final QName SBYNPX_LINE_LABEL_QNAME =
        NamespaceUtility.getQName(XMLAttribute.SBYNPX_NAMESPACE, 
        						  PrivateAttr.LINE_LABEL, 
        						  XMLAttribute.SBYNPX);

    /** Getter for array of XML attribute objects for this element.
     * @return  Array of <tt>XMLAttribute</tt> objects; can be <tt>null</tt>
     *          if none.
     */
    XMLAttribute[] getXmlAttributes();

    /** Getter for other attributes.
     * @return  <code>Map</code> representing other attributes
     */
    Map getOtherAttributes();

    /** Setter for other attributes.
     * @param   name    Attribute name.
     * @param   value   Attribute value.
     */
    void setOtherAttributes(String name, String value);

    /** Setter for other attributes.  This convenience method also defines the namespace in the
     * case of a custom attribute.
     * @param   qName   Qualified name for attribute.
     * @param   value   Attribute value.
     */
    void setOtherAttributes(QName qName, String value);

    /** Gets the allowed children element tags for this element.
     * @return  Array of children element tags; <code>null</code> if none.
     */
    String[] getChildrenTags();

    /** Sets the default namespace URI declaration.
     * @param   uri     Default namespace URI.
     */
    void setDefaultNamespace(String uri);

    /** Sets a XML namespace URI declaration.
     * @param   prefix  Prefix for namespace.
     * @param   uri     URI for namespace; <code>null</code> to delete.
     */
    void setNamespace(String prefix, String uri);

    /** Gets the default namespace URI declartion.
     * @return  Default namespace URI; <code>null</code> if none.
     */
    String getDefaultNamespace();

    /** Gets a namespace URI declartion.
     * @param   prefix  Prefix for namespace.
     * @return  A namespace URI; <code>null</code> if none.
     */
    String getNamespace(String prefix);

    /**
     * Gets the prefix associated with the given namespace URI.
     * @param namespaceURI the namespace URI
     * @return the namespace prefix or null if the namespace URI is not
     * currently associated with a prefix
     */
    String getNamespacePrefix(String namespaceURI);

    /** Gets the XML namespace size.
     * @return  Number of namespaces defined.
     */
    int getNamespaceSize();

    /** Gets the XML namespace prefixes.
     * @return  A array of namespace prefixes; <tt>xmlns</tt> is the default.
     *          Can also be <code>null</code> if none.
     */
    String[] getNamespacePrefixes();

    /** Gets all the XML namespace prefixes as a map.
     * @return  <code>Map</code> object representing all the namespaces.
     */
    Map getNamespaces();

    /** Sets the map containing all XML namespace prefixes at this level.
     * @param   m   Map containing XML namespace prefixes for this level.
     */
    void setNamespaces(Map m);

    /** Gets all the namespaces from the parent(s) along with the current element.
     * @return  A map of all the namespaces from the parent(s) and the current element.
     */
    Map getTotalNamespaces();

    /** Setter for property qualifiedName.
     * @param   qualifiedName   New value of property qualifiedName
     */
    void setQualifiedName(QName qualifiedName);

    /** Gets the XML document that owns this element.
     * @return  Owner BPEL Document.
     */
    XMLDocument getOwnerDocument();

    /** Sets the XML document that owns this element.
     * @param   d   Owner document.
     */
    void setOwnerDocument(XMLDocument d);

    /** Gets the list of attributes (in order of creation).
     * @return  List of attributes.
     */
    List getOrderedAttributes();

    /** Gets the XML attribute by name.
     * @param   n   Name of attribute.
     * @return  XML attribute.
     */
    XMLAttribute getAttribute(String n);


    /** Gets the attribute value by name.
     * @param   n   Name of attribute.
     * @return  value of the attribute.
     */
    Object getAttributeValue(String n);

    /** Sets the attribute at the ordinal position.
     * @param   i   Ordinal index (according to each element definition).
     * @param   v   Value of attribute.
     */
    void setAttribute(int i, String v);

    /** Sets the attribute at the ordinal position.
     * @param   i   Ordinal index (according to each element definition).
     * @param   q   Qualified name of attribute.
     * @param   v   Value of attribute.
     */
    void setAttribute(int i, String q, String v);

    /** Sets the attribute by name.
     * @param   n   Name of attribute; can be either local name or qualified
     *              name.
     * @param   v   Value of attribute.
     */
    void setAttribute(String n, String v);

    /** Sets the attribute by name.
     * @param   n   Name of attribute; can be either local name or qualified
     *              name.
     * @param   v   Value of attribute.
     */
    void setAttribute(String n, Object v);

    /** Gets a documentation sub-element for this element.
     * @return  Documentation element.
     */
    Documentation getDocumentation();

    /** Sets a documenation sub-element for this element.
     * @param   doc     Documentation element
     */
    void setDocumentation(Documentation doc);

    /** adds a documenation sub-element for this element at the end of collection.
     * @param   doc     Documentation element
     */
    void setDocumentationAtTail(Documentation doc);

    /** Gets the presentation (for GUI) map for this element.  The map keys and values must be
     * strings. Also, the keys should not have the SeeBeyond private extension presentation
     * prefix ("sbynpxp:") nor should it contain any character that is illegal for an XML
     * attribute name (such as greater than or less than signs).
     *
     * @return  A map to be used in the presentation; <code>null</code> return if none.
     */
    Map getPresentationMap();

    /** Sets the presentation (for GUI) map for this element.  The map keys and values must be
     * strings. Also, the keys should not have the SeeBeyond private extension presentation
     * prefix ("sbynpxp:") nor should it contain any character that is illegal for an XML
     * attribute name (such as greater than or less than signs).
     *
     * @param   map     Presentation map.
     */
    void setPresentationMap(Map map);

    /** Gets the raw presentation map for this element.  Keys here MUST have the SeeBeyond
     * private extension presentation prefix ("sbynpxp:").
     *
     * @param   create  <code>true</code> if map is to be created if it doesn't exist.
     * @return  Raw presentation map.
     */
    Map getRawPresentationMap(boolean create);

    /** Gets the associated label name for the line link coming
     * immediately into this element.
     * @return  Line label name for this element.
     */
    String getLineLabel();

    /** Sets the associated label name for the line link coming
     * immediately into this element.
     * @param   label   Line label name for this element.
     */
    void setLineLabel(String label);

    /** Accepts a visitor to perform some work on the element.
     * @param   v   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    boolean superAccept(Visitor v);

    /**
     * Adds the specified <code>ChangeListener</code> to the xmlelement.
     *
     * @param l the <code>ChangeListener</code> to add
     */
    void addChangeListener(ChangeListener l);

    /**
     * Removes a <code>ChangeListener</code> from the xmlelement.
     *
     * @param l the <code>ChangeListener</code> to remove
     */
    void removeChangeListener(ChangeListener l);

    /**
     * Added a <code> XMLElementListener </code> to the XMLElement.
     * @param l the <code> XMLElementListener </code> to add.
     */
    void addXMLElementListener(XMLElementListener l);

    /**
     * Removed a a <code> XMLElementListener </code> from the XMLElement.
     * @param l the <code> XMLElementListener </code> to remove.
     */
    void removeXMLElementListener(XMLElementListener l);

    void fireXMLAttributeAdded(XMLAttributeEvent evt);

    void fireXMLAttributeRemoved(XMLAttributeEvent evt);

    void fireXMLAttributedModified(XMLAttributeEvent evt);

    /**
     * first looks for existing prefix for given namespace.
     * If exist then return this first prefix otherwise create
     * a new unique prefix set it to the map of prefix to namespace and return it.
     * A preferredPrefix is used for the new created prefix if it is not used by other namespaces
     * @return new prefix.
     */
    String createAndSetNamespacePrefix(String namespaceURI, String preferredPrefix);


}
