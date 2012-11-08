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
 * @(#)PropertyImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.bpel.impl;



import com.sun.wsdl.model.bpel.Property;
import com.sun.wsdl.model.common.model.XMLAttribute;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.model.impl.XMLAttributeImpl;
import com.sun.wsdl.model.common.visitor.Visitor;
import com.sun.wsdl.model.extensions.impl.ExtensibilityElementImpl;
import com.sun.wsdl.model.impl.WSDLElementImpl;
import com.sun.wsdl.model.visitor.WSDLVisitor;



/**
 * Implements the &lt;property&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class PropertyImpl extends ExtensibilityElementImpl implements Property {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 6304456260795704643L;
    
    /** Creates a new instance of PropertyImpl */
    public PropertyImpl() {
        super();
        initProperty();
    }
    
    /** Creates a new instance of PropertyImpl.
     * @param   d   Owner document.
     */
    public PropertyImpl(XMLDocument d) {
        super(d);
        initProperty();
    }
    
    /** Initializes this class.
     */
    private void initProperty() {
        setLocalName(Property.TAG);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.NAME, String.class, false, null),
            new XMLAttributeImpl(ATTR.TYPE, String.class, false, null)
        };
    }
    
    /** Getter for name attribute.
     * @return  Value of name attribute.
     */
    public String getName() {
        return xmlAttrs[NAME].getValue();
    }
    
    /** Setter for name attribute.
     * @param   n   Value of name attribute.
     */
    public void setName(String n) {
        setAttribute(NAME, n);
    }
    
    /** Getter for type attribute.
     * @return  Value of type attribute.
     */
    public String getType() {
        return xmlAttrs[TYPE].getValue();
    }
    
    /** Setter for type attribute.
     * @param   t   Value of type attribute.
     */
    public void setType(String t) {
        setAttribute(TYPE, t);
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if auto-traversal is to continue.
     */
    public boolean accept(Visitor w) {
        WSDLVisitor v = morphVisitor(w);
        return v.visit(this);
    }
}
