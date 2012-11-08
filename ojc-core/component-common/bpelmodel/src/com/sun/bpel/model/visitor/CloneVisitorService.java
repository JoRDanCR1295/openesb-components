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
 * @(#)CloneVisitorService.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.visitor;

import com.sun.bpel.model.common.visitor.AbstractVisitorService;
 

/**
 * Service to provide all clone visitors needed to clone elements within
 * a BPEL4WS document.
 *
 * @author Sun Microsystems
 * @version 
 */
public class CloneVisitorService extends AbstractVisitorService {
    
    /** Creates a new instance of CloneVisitorService */
    public CloneVisitorService() {
        super(new Class[] {
            com.sun.bpel.model.visitor.CloneVisitor.class
        });
    }
}
