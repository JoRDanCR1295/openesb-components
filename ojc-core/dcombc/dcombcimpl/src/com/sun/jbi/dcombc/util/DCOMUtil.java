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

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * utility class of DCOM BC
 * 
 * @author Chandrakanth Belde
 */
public class DCOMUtil {

    /**
     * Returns the stack trace of a Throwable as a String
     * 
     * @param t Instance of Throwable
     * @return The stack trace of t as a String
     */
    public static String getStackTraceAsString(Throwable t) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        t.printStackTrace(pw);
        return sw.toString();
    }
}