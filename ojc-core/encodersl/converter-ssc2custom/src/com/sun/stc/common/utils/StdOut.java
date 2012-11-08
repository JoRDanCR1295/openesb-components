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
 * @(#)StdOut.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.common.utils;

/******************************************************************************
 * Copyright (c) 1996, Software Technologies Corporation, All Rights Reserved *
 *                                                                            *
 * This program, and all the routines referenced herein, are the proprietary  *
 * properties and trade secrets of SOFTWARE TECHNOLOGIES CORPORATION.         *
 *                                                                            *
 * Except as provided for by license agreement, this program shall not be     *
 * duplicated, used or disclosed without the written consent, signed by an    *
 * officer of SOFTWARE TECHNOLOGIES CORPORATION.                              *
 *                                                                            *
 ******************************************************************************/
												
/**
 *  <p>
 *  DESCRIPTION OF CLASS PARAGRAPH 1
 *  <p>
[* @see     related class ]
 * @author  Jonelle Barrera 
 */ 

public class StdOut
{
	private static boolean debugEnabled = false;
/**
 * This method was created in VisualAge.
 * @return boolean
 */
public static boolean getDebugEnabled() {
	return debugEnabled;
}
/**
 * This method was created in VisualAge.
 * @param val java.lang.String
 */
public static void print(String val)
{
	if ((getDebugEnabled() == true) && (val != null))
	{
		System.out.print(val);
	}
}
/**
 * This method was created in VisualAge.
 * @param val java.lang.String
 */
public static void println(String val)
{
	if ((getDebugEnabled() == true) && (val != null))
	{
		System.out.println(val);
	}
}
/**
 * This method was created in VisualAge.
 * @param val int
 */
public static void print(int val)
{
        if (getDebugEnabled() == true)
        {
                System.out.print(val);
        }
}
/**
 * This method was created in VisualAge.
 * @param val int
 */
public static void println(int val)
{
        if (getDebugEnabled() == true)
        {
                System.out.println(val);
        }
}
/**
 * This method was created in VisualAge.
 * @param val long
 */
public static void print(long val)
{
        if (getDebugEnabled() == true)
        {
                System.out.print(val);
        }
}
/**
 * This method was created in VisualAge.
 * @param val long
 */
public static void println(long val)
{
        if (getDebugEnabled() == true)
        {
                System.out.println(val);
        }
}
/**
 * This method was created in VisualAge.
 * @param val long
 */
public static void print(Exception val)
{
        if (getDebugEnabled() == true)
        {
                System.out.print(val);
        }
}
/**
 * This method was created in VisualAge.
 * @param val long
 */
public static void println(Exception val)
{
        if (getDebugEnabled() == true)
        {
                System.out.println(val);
        }
}
/**
 * This method was created in VisualAge.
 * @param newValue boolean
 */
public static void setDebugEnabled(boolean newValue) {
	StdOut.debugEnabled = newValue;
}
}
