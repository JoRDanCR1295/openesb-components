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
 * @(#)LazyImportVisitorService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.visitor;

import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.common.visitor.AbstractVisitorService;


/**
 * @author Sun Microsystems
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class LazyImportVisitorService extends AbstractVisitorService {
    
	private BPELParseContext mContext;
	
    /** Creates a new instance of SAXParseVisitorService */
    public LazyImportVisitorService() {
        super(new Class[] {
        	com.sun.bpel.model.visitor.LazyImportVisitor.class,
        });
    }
    
    public void setBPELParseContext(BPELParseContext context) {
    	this.mContext = context;
    }
    
    public BPELParseContext getBPELParseContext() {
    	return this.mContext;
    }
}
