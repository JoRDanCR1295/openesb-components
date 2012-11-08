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
 * @(#)XMLCommentImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model.impl;

import com.sun.wsdl.model.common.model.XMLComment;
import com.sun.wsdl.model.common.visitor.CommentVisitor;
import com.sun.wsdl.model.common.visitor.Visitor;

/**
 * Implements a XML comment.
 *
 * @author Sun Microsystems
 * @version 
 */
public class XMLCommentImpl extends XMLNodeImpl implements XMLComment {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = 8039068966648475089L;
    
    /** Creates a new instance of XMLCommentImpl */
    public XMLCommentImpl() {
        super();
        initComment();
        setValue("");
    }
    
    /** Creates a new instance of XMLCharacterDataImpl.
     * @param   v   Value of text.
     */
    public XMLCommentImpl(String v) {
        super();
        initComment();
        setValue(v);
    }
    
    /** Initializes this class.
     */
    private void initComment() {
        setLocalName(XMLComment.TAG);
        setQualifiedName(XMLComment.TAG);
    }
    
    /** Accepts a visitor to perform some work on the element.
     * @param   v   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
     */
    public boolean accept(Visitor v) {
        return ((CommentVisitor) v).visit(this);
    }
}
