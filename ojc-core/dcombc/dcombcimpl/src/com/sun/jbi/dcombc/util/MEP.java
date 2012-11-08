/* *************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/

package com.sun.jbi.dcombc.util;

public enum MEP {
	IN_ONLY,
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
