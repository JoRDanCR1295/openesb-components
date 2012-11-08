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
 * @(#)SAPException.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
*
*          Copyright (c) 2002, SeeBeyond Technology Corporation,
*          All Rights Reserved
*
*          This program, and all the routines referenced herein,
*          are the proprietary properties and trade secrets of
*          SEEBEYOND TECHNOLOGY CORPORATION.
*
*          Except as provided for by license agreement, this
*          program shall not be duplicated, used, or disclosed
*          without  written consent signed by an officer of
*          SEEBEYOND TECHNOLOGY CORPORATION.
*
***************************************************************************/

package com.sun.jbi.sapbc.extservice;

import com.sap.mw.jco.JCO;

/**
 * The class wraps up the Exception happening whie executing the BAPI
 * and throws it back to the Collaboration. The User can get the Exception String
 * as well as the Actual Exception occurred for their processing.
 * 
 * @author  $Author: Chalapathy
 */

public class SAPException extends java.lang.Exception
{
    private static final String newline = System.getProperty("line.separator");
    
 
    /**
     * Constructs a new <code>SAPException</code> with null as its detail message
     *
     */
    public SAPException()
    {
    }
    
    /**
     * Constructs a new <code>SAPException</code> with the specified detail message.
     *
     * @param msg the detail message
     */
    public SAPException(String msg)
    {
        super(msg);
    }
    
    /**
     * Constructs a new <code>SAPException</code> with the specified detail message and cause.
     *
     * @param msg the detail message
     * @param aCause Throwable that caused the this exception
     */
    public SAPException(String msg, Throwable aCause)
    {
        super(msg, aCause);
    }
    
    /**
     * Constructs a <code>SAPException</code> with the specified cause and a detail message of 
     * (cause==null ? null : cause.toString()) (which typically contains the class and detail 
     * message of cause).
     *
     * @param aCause Throwable that caused the this exception
     */
    public SAPException(Throwable aCause)
    {
        super(aCause);
    }
        
    /**
     * The String representation of the Exception occurred.
     *
     * @return String containing the Exception details.
     *   
     */
    public String getMessage()
    {
        StringBuffer strBuffer = new StringBuffer();
        if(super.getCause() instanceof JCO.Exception)
        {   
            JCO.Exception jcoException = (JCO.Exception) super.getCause();
            strBuffer.append("******* JCO Exception occurred while executing the BAPI ****************").append(newline);
            strBuffer.append("\tException Group    = ").append( jcoException.getGroup()).append(newline);
            strBuffer.append("\tException Key      = ").append( jcoException.getKey()).append(newline);
            strBuffer.append("\tException String   = ").append( jcoException.toString()).append(newline);
            strBuffer.append("***********************************************************************").append(newline);
        }
        else
        {
            strBuffer.append("*********** Exception occurred while executing the BAPI ****************").append(newline);
            strBuffer.append("\tException String   = ").append( (super.getMessage())).append(newline);
            strBuffer.append("***********************************************************************").append(newline);
        }        
        return strBuffer.toString();
    }
}
