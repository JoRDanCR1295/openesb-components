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
 * @(#)R1011Statement.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.validation.conf.wsi.bp.v10.impl;

import com.sun.jbi.httpsoapbc.validation.conf.wsi.api.Statement;
import com.sun.jbi.internationalization.Messages;

/**
 * A representation of the WSI Basic Profile normative statement R1011.
 * <p>
 * The interpretation of sibling elements following the soap:Body element is 
 * unclear. Therefore, such elements are disallowed.
 * <p>
 * R1011 A MESSAGE MUST NOT have any element children of soap:Envelope following the soap:Body element.
 * <p>
 * This requirement clarifies a mismatch between the SOAP 1.1 specification and the SOAP 1.1 XML Schema.
 *
 */
public final class R1011Statement implements Statement {
    private static final Messages messages =
        Messages.getMessages(R1011Statement.class);

    public String getDescription() {
        return messages.getString("R1011_Description");
    }

    public String getID() {
        return "R1011";
    }
    
    public String toString() {
        return messages.getString("R1011_Statement");
    }
}
