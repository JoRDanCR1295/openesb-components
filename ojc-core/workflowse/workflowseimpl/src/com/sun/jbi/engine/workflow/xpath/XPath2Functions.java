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

package com.sun.jbi.engine.workflow.xpath;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.jxpath.Function;
import org.apache.commons.jxpath.Functions;

public class XPath2Functions  implements Functions {

    static final Function getCurrentDateTime = new CurrentDateTime();
    static final Function getCurrentDate = new CurrentDate();
    static final Function getCurrentTime = new CurrentTime();
    static final Function getEmailAddress = new  GetEmailAddress ();
    static final Function getManagerEmailAddress = new GetManagerEmailAddress ();
    static final Function getManagerUID = new GetManagerUID ();
    static final Function getTaskOwner = new GetTaskOwner ();
    static final Function getTaskId = new GetTaskId ();
    
    static final String WLMNS = "http://jbi.com.sun/wfse/xpath-functions";
    
    static final Set<String> WLMNSSET = new HashSet<String> ();
    
    static {
        WLMNSSET.add(WLMNS);
    }

    /* (non-Javadoc)
     * @see org.apache.commons.jxpath.Functions#getUsedNamespaces()
     */
    public Set getUsedNamespaces() {
        // TODO Auto-generated method stub
        return WLMNSSET;
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
        } else if (name.equals ("get-email-ldap") || name.equals ("get-email")) {
            return getEmailAddress;
        } else if (name.equals("get-manager-email-ldap") || name.equals("get-manager-email")) {
            return getManagerEmailAddress;
        } else if (name.equals ("get-manager-uid-ldap") || name.equals ("get-manager-uid")) {
            return getManagerUID;
        }  else if (name.equals ("get-task-owner")) {
            return getTaskOwner;
        } else if (name.equals ("get-task-id")) {
            return getTaskId;
        }
        return null;
    }
}
