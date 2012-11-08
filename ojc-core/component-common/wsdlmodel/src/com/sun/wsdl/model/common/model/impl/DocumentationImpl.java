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
 * @(#)DocumentationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model.impl;

import com.sun.wsdl.model.common.model.Documentation;
import com.sun.wsdl.model.common.model.XMLDocument;
import com.sun.wsdl.model.common.visitor.DocumentationVisitor;
import com.sun.wsdl.model.common.visitor.Visitor;

/**
 * Implements the &lt;document&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class DocumentationImpl extends XMLElementImpl implements Documentation {
    
    /** serialVersionUID for this class */
    //static final long serialVersionUID = -6590729964509647635L;
    
    /** Creates a new instance of DocumenationImpl */
    public DocumentationImpl() {
        super();
        initDocumentation();
    }
    
    /** Constructor for new documentation instance.
     * @param   d   Owner document.
     */
    public DocumentationImpl(XMLDocument d) {
        super(d);
        initDocumentation();
    }
    
    /** Initializes this class.
     */
    private void initDocumentation() {
        setLocalName(Documentation.TAG);
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor w) {
        DocumentationVisitor v = (DocumentationVisitor) w;
        return v.visit(this);
    }
}
