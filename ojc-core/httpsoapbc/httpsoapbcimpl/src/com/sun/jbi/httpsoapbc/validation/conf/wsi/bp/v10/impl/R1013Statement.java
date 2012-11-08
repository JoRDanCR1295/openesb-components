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
 * @(#)R1013Statement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.v10.impl;

import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.Statement;
import com.sun.jbi.internationalization.Messages;

/**
 * A representation of the WSI Basic Profile normative statement R1013.
 * The soap:mustUnderstand attribute has a restricted type of "xsd:boolean" 
 * that takes only "0" or "1". Therefore, only those two values are allowed.
 * <p>
 * R1013 A MESSAGE containing a soap:mustUnderstand attribute MUST only use 
 * the lexical forms "0" and "1".
 *
 */
public class R1013Statement implements Statement {
    private static final Messages messages =
        Messages.getMessages(R1013Statement.class);

    public String getDescription() {
        return messages.getString("R1013_Description");
    }

    public String getID() {
        return "R1013";
    }

    public String toString() {
        return messages.getString("R1013_Statement");        
    }
}
