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
 * @(#)MQPROPVARIANT.java 
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


package com.sun.jbi.msmqbc.jni;

public class MQPROPVARIANT {
	public int vt = STCComVartype.VT_EMPTY;
	public boolean bIsByRef  = false;

	// Some of the types from the C MQPROPVARIANT struct. Tried to
	// keep the union member names the same where possible.
	//
	// TODO: More types...
	public byte    bVal;           // VT_UI1
	public short   iVal;           // VT_I2
	public boolean boolVal;        // VT_BOOL
	public int     lVal;           // VT_I4
	public int     scode;          // VT_ERROR
	public double  date;           // VT_DATE
	public String  bstrVal = null; // VT_BSTR
	public String  pwszVal = null; // VT_LPWSTR

	public MQPROPVARIANT() {
		vt = STCComVartype.VT_EMPTY;
	}

	public MQPROPVARIANT(byte by) {
		bVal = by;
		vt = STCComVartype.VT_UI1;
	}

	public MQPROPVARIANT(short s) {
		iVal = s;
		vt = STCComVartype.VT_I2;
	}

	public MQPROPVARIANT(boolean b) {
		boolVal = b;
		vt = STCComVartype.VT_BOOL;
	}

	public MQPROPVARIANT(int i, int type) {
		if (STCComVartype.VT_I4 == type) {
			lVal = i;
			vt = STCComVartype.VT_I4;
		}
		else if (STCComVartype.VT_ERROR == type) {
			scode = i;
			vt = STCComVartype.VT_ERROR;
		}
		else {
			// error
			vt = STCComVartype.VT_EMPTY;
		}
	}

	public MQPROPVARIANT(String s, int type) {
		if (STCComVartype.VT_BSTR == type) {
			bstrVal = s;
			vt = STCComVartype.VT_BSTR;
		}
		else {
			pwszVal = s;
			vt = STCComVartype.VT_LPWSTR;
		}
	}

	public MQPROPVARIANT(double d) {
		date = d;
		vt = STCComVartype.VT_DATE;
	}

// Prep for reuse
	public void clear() {
		bVal     = 0;;
		iVal     = 0;
		boolVal  = false;
		lVal     = 0;
		scode    = 0;
		date     = 0.0;
		bstrVal  = null;
		pwszVal  = null;
		bIsByRef = false;
		vt = STCComVartype.VT_EMPTY;
	}

	boolean isByRef() {
		return bIsByRef;
	}

	void isByRef(boolean b) {
		bIsByRef = b;
	}
}
