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
 * @(#)EInsightModelException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.common.model;

import com.sun.wsdl.model.common.BaseRuntimeException;

/**
 * General exception thrown to indicate problems with the eInsight model.
 *
 * @author Sun Microsystems
 * @version 
 */
public class EInsightModelException extends BaseRuntimeException {
    
    /**
     * Construct a new EInsightModelException instance.
     *
     * @param message a message to be stored with the exception.
     */
    public EInsightModelException(String message) {
        super(message, null);
    }
    
    /**
     * Creates a new EInsightModelException instance.
     *
     * @param ex an Exception object.
     */
    public EInsightModelException(Exception ex) {
        super(ex.getMessage(), ex);
    }
    
    /**
     * Construct a new EInsightModelException that references
     * a parent Exception.
     *
     * @param message a message to be stored with the exception.
     * @param t a Throwable object.
     */
    public EInsightModelException(String message, Throwable t) {
        super(message, t);
    }
}
