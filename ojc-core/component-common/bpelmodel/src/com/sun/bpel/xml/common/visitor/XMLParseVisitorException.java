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
 * @(#)XMLParseVisitorException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.xml.common.visitor;

import com.sun.bpel.xml.common.BaseRuntimeException;

/**
 * Exception thrown by a XML parse visitor.
 *
 * @author Sun Microsystems
 * @version 
 */
public class XMLParseVisitorException extends BaseRuntimeException {
    
    /**
     * Construct a new XMLParseVisitorException instance.
     *
     * @param message a message to be stored with the exception.
     */
    public XMLParseVisitorException(String message) {
        super(message, null);
    }
    
    /**
     * Creates a new XMLParseVisitorException instance.
     *
     * @param ex an Exception object.
     */
    public XMLParseVisitorException(Exception ex) {
        super(ex.getMessage(), ex);
    }
    
    /**
     * Construct a new XMLParseVisitorException that references
     * a parent Exception.
     *
     * @param message a message to be stored with the exception.
     * @param t a Throwable object.
     */
    public XMLParseVisitorException(String message, Throwable t) {
        super(message, t);
    }
}
