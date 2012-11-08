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
 * @(#)ExecUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.execbc.util;

import java.io.File;

import com.sun.jbi.execbc.extensions.ExecAddress;

/**
 *
 * @author Jun Xu
 */
public class ExecUtil {
    
    public static boolean isRemote(String hostName) {
        if (hostName == null || "localhost".equals(hostName) ||
                "127.0.0.1".equals(hostName)) {
            return false;
        }
        return true;
    }
    
    public static String[] getHosts(ExecAddress addr) {
        String hosts = addr.getHostName();
        if (hosts == null || hosts.length() == 0) {
            return new String[0];
        }
        return hosts.split(",");
    }
}
