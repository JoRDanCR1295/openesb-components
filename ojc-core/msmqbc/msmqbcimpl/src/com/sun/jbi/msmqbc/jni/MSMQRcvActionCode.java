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
 * @(#)MSMQRcvActionCode.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.jni;

/**
 * This class wraps the action modes param for the ReceiveMessage The values must match those in
 * mq.h
 * 
 * @author Sun Microsystems
 */

public class MSMQRcvActionCode {
    public static int ACTION_RECEIVE = 0x00000000;

    public static int ACTION_PEEK_CURRENT = 0x80000000;

    public static int ACTION_PEEK_NEXT = 0x80000001;

    public static int LOOKUP_PEEK_CURRENT = 0x40000010;

    public static int LOOKUP_PEEK_NEXT = 0x40000011;

    public static int LOOKUP_PEEK_PREV = 0x40000012;

    public static int LOOKUP_PEEK_FIRST = 0x40000014;

    public static int LOOKUP_PEEK_LAST = 0x40000018;

    public static int LOOKUP_RECEIVE_CURRENT = 0x40000020;

    public static int LOOKUP_RECEIVE_NEXT = 0x40000021;

    public static int LOOKUP_RECEIVE_PREV = 0x40000022;

    public static int LOOKUP_RECEIVE_FIRST = 0x40000024;

    public static int LOOKUP_RECEIVE_LAST = 0x40000028;
}
