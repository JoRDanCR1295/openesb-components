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
 * @(#)MEP.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ldapbc;


/**
 *
 * @author Narayanaa
 */
public enum MEP {IN_ONLY,
    IN_OUT,
    IN_OPTIONAL_OUT,
    ROBUST_IN_ONLY,
    OUT_ONLY,
    OUT_IN,
    OUT_OPTIONAL_IN,
    ROBUST_OUT_ONLY;
    @Override
    public String toString() {
        switch (this) {
        case IN_ONLY:
            return "http://www.w3.org/2004/08/wsdl/in-only";

        case IN_OUT:
            return "http://www.w3.org/2004/08/wsdl/in-out";

        case IN_OPTIONAL_OUT:
            return "http://www.w3.org/2004/08/wsdl/in-opt-out";

        case ROBUST_IN_ONLY:
            return "http://www.w3.org/2004/08/wsdl/robust-in-only";

        case OUT_ONLY:
            return "http://www.w3.org/2004/08/wsdl/out-only";

        case OUT_IN:
            return "http://www.w3.org/2004/08/wsdl/out-in";

        case OUT_OPTIONAL_IN:
            return "http://www.w3.org/2004/08/wsdl/out-opt-in";

        case ROBUST_OUT_ONLY:
            return "http://www.w3.org/2004/08/wsdl/robust-out-only";

        default:
            return null;
        }
    }
}
