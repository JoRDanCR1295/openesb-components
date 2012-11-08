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
 * @(#)STCComVartype.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.jni;

/**
 * This class just represents the COM VARTYPE values. The values
 * MUST match the values in the vartype enum in wtypes.h
 *
 * @author Sun Microsystems
 */

public class STCComVartype {
    public static final int VT_EMPTY = 0;

    public static final int VT_NULL = 1;

    public static final int VT_I2 = 2;

    public static final int VT_I4 = 3;

    public static final int VT_R4 = 4;

    public static final int VT_R8 = 5;

    public static final int VT_CY = 6;

    public static final int VT_DATE = 7;

    public static final int VT_BSTR = 8;

    public static final int VT_DISPATCH = 9;

    public static final int VT_ERROR = 10;

    public static final int VT_BOOL = 11;

    public static final int VT_VARIANT = 12;

    public static final int VT_UNKNOWN = 13;

    public static final int VT_DECIMAL = 14;

    public static final int VT_I1 = 16;

    public static final int VT_UI1 = 17;

    public static final int VT_UI2 = 18;

    public static final int VT_UI4 = 19;

    public static final int VT_I8 = 20;

    public static final int VT_UI8 = 21;

    public static final int VT_INT = 22;

    public static final int VT_UINT = 23;

    public static final int VT_VOID = 24;

    public static final int VT_HRESULT = 25;

    public static final int VT_PTR = 26;

    public static final int VT_SAFEARRAY = 27;

    public static final int VT_CARRAY = 28;

    public static final int VT_USERDEFINED = 29;

    public static final int VT_LPSTR = 30;

    public static final int VT_LPWSTR = 31;

    public static final int VT_RECORD = 36;

    public static final int VT_FILETIME = 64;

    public static final int VT_BLOB = 65;

    public static final int VT_STREAM = 66;

    public static final int VT_STORAGE = 67;

    public static final int VT_STREAMED_OBJECT = 68;

    public static final int VT_STORED_OBJECT = 69;

    public static final int VT_BLOB_OBJECT = 70;

    public static final int VT_CF = 71;

    public static final int VT_CLSID = 72;

    public static final int VT_VERSIONED_STREAM = 73;

    public static final int VT_BSTR_BLOB = 0xfff;

    public static final int VT_VECTOR = 0x1000;

    public static final int VT_ARRAY = 0x2000;

    public static final int VT_BYREF = 0x4000;

    public static final int VT_RESERVED = 0x8000;

    public static final int VT_ILLEGAL = 0xffff;

    public static final int VT_ILLEGALMASKED = 0xfff;

    public static final int VT_TYPEMASK = 0xfff;
}
