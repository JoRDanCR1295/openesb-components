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
 * @(#)XPath2Functions.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import java.util.Set;

import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.Functions;

public class XPath2Functions  implements Functions {

    static final Function getCurrentDateTime = new CurrentDateTime();
    static final Function getCurrentDate = new CurrentDate();
    static final Function getCurrentTime = new CurrentTime();
    static final Function dateTimeLessThan = new DateTimeLessThan();
    static final Function dateLessThan = new DateLessThan();
    static final Function timeLessThan = new TimeLessThan();

    /* (non-Javadoc)
     * @see org.apache.commons.jxpath.Functions#getUsedNamespaces()
     */
    public Set getUsedNamespaces() {
        // TODO Auto-generated method stub
        return null;
    }
    
    /* (non-Javadoc)
     * @see org.apache.commons.jxpath.Functions#getFunction(java.lang.String, java.lang.String, java.lang.Object[])
     */
    public Function getFunction(String namespace, String name, Object[] parameters) {
        if (name.equals("current-dateTime")) {
            return getCurrentDateTime;
        } else if (name.equals("current-date")) {
            return getCurrentDate;
        } else if (name.equals("current-time")) {
            return getCurrentTime;
        } else if (name.equals("dateTime-less-than")) {
        	return dateTimeLessThan;
        } else if (name.equals("date-less-than")) {
        	return dateLessThan;
        } else if (name.equals("time-less-than")) {
        	return timeLessThan;
        }
        return null;
    }
}
