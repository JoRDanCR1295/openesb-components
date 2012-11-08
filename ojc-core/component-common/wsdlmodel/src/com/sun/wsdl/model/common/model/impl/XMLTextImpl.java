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
 * @(#)XMLTextImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model.impl;

import com.sun.wsdl.model.common.model.XMLText;
import com.sun.wsdl.model.common.visitor.TextVisitor;
import com.sun.wsdl.model.common.visitor.Visitor;

/**
 *
 * @author Sun Microsystems
 * @version 
 */
public class XMLTextImpl extends XMLNodeImpl implements XMLText {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 8806433573909216357L;
    
    /** Creates a new instance of XMLTextImpl */
    public XMLTextImpl() {
        super();
        initText();
        setValue("");
    }
    
    /** Creates a new instance of XMLTextImpl.
     * @param   v   Value of text.
     */
    public XMLTextImpl(String v) {
        super();
        initText();
        setValue(v);
    }
    
    /** Initializes this class
     */
    private void initText() {
        setLocalName(XMLText.TAG);
        setQualifiedName(XMLText.TAG);
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        TextVisitor v = (TextVisitor) w;
        return v.visit(this);
    }
}
