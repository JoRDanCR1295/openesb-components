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
 * @(#)BPELDocumentationImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import com.sun.bpel.model.BPELDocumentation;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.visitor.DocumentationVisitor;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class BPELDocumentationImpl extends BPELElementImpl implements BPELDocumentation {
	public BPELDocumentationImpl(XMLDocument d) {
		 super(d);
		    initDocumentation();
   }
   
   /** Initializes this class.
    */
   private void initDocumentation() {
       setLocalName(BPELDocumentation.TAG);
   }
	
	 /** @see com.sun.bpel.xml.common.model.XMLNode#accept(
    *  com.sun.bpel.xml.common.model.visitor.Visitor)
    */
   public boolean accept(Visitor w) {
   	DocumentationVisitor v = (DocumentationVisitor) w;
       return v.visit(this);
   }
}
