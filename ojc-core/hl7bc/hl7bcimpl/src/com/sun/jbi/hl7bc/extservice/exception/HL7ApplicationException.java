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
 * @(#)HL7ApplicationException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2006, Sun Microsystems inc,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems inc.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems inc.
 *
 ***************************************************************************/
package com.sun.jbi.hl7bc.extservice.exception;


/**
 * Exception class for HL7 application.
 *
 * @author Raghunadh
 * 
 */

public class HL7ApplicationException extends Exception  {
   
    private Exception nestedException;

    /**
     * Creates a new instance of <code>HL7ApplicationException</code>
     * without detail message.
     */
    public HL7ApplicationException() {
        super();
    }

    /**
     * Constructs an instance of <code>HL7ApplicationException</code>
     * with the specified detail message.
     *
     * @param msg the detail message.
     */
    public HL7ApplicationException(String msg) {
        super(msg);
    }
    
    /**
     * @param nestedException
     */
    public HL7ApplicationException(Exception nestedException) {
        super(nestedException);
    }

    /**
     * @param msg
     * @param nestedException
     */
    public HL7ApplicationException(String msg, Exception nestedException) {
        super(msg, nestedException);
        
    }

   

}
