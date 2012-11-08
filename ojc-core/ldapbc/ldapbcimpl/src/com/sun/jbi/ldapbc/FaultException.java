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
 * @(#)FaultException.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.ldapbc;

import javax.jbi.messaging.MessagingException;


/**
 * Contains the information to reply with a fault
 */
public class FaultException extends Exception {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private String mFaultCode;
    private String mFaultString;
    private String mFaultActor;
    private String mDetail;
    
    /** Creates a new instance of HttpException */
    public FaultException() {
        super();
    }
    
    public FaultException(final MessagingException ex) {
        // default
        mFaultCode = "Server";
        //mFaultString;
        //mFaultActor;
        mDetail = ex.getMessage();
    }
    
    public String getFaultCode() {
        return mFaultCode;
    }
    
    public String getFaultString() {
        return mFaultString;
    }
    
    public String getFaultActor() {
        return mFaultActor;
    }
    
    public String getDetail() {
        return mDetail;
    }
}
