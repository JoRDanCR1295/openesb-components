/*
 * @(#)file      SnmpInformHandlerImpl.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.3
 * @(#)lastedit  04/04/07
 *
 * Copyright 2004 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.soabi.snmpagent;

import com.sun.management.comm.SnmpInformHandler;
import com.sun.management.comm.SnmpInformRequest;
import com.sun.management.snmp.SnmpVarBindList;

public class SnmpInformHandlerImpl implements SnmpInformHandler {
    public void processSnmpPollData(SnmpInformRequest request, 
                                    int errStatus, 
                                    int errIndex, 
                                    SnmpVarBindList vblist) {
        java.lang.System.out.println(
                             "processSnmpPollData Inform Response received : ");
        java.lang.System.out.println("       errStatus : " + errStatus);
        java.lang.System.out.println("       errIndex : " + errIndex);
        java.lang.System.out.println("       vblist : " + vblist);
    }
    
    public void processSnmpPollTimeout(SnmpInformRequest request) {
        java.lang.System.out.println(
                             "processSnmpPollTimeout Inform Response Timeout.");
    }

    public void processSnmpInternalError(SnmpInformRequest request,
                                         String errmsg) {
        java.lang.System.out.println(
                             "processSnmpInternalError Inform Response.");
    }
}
