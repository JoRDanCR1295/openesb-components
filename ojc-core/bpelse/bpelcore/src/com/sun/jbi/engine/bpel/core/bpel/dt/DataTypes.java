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
 * @(#)DataTypes.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.dt;

import java.util.HashMap;


/**
 * DateTypes class
 *
 * @author Sun Microsystems
 *         Preferences - Java - Code Generation - Code and Comments
 */
public class DataTypes {
    private static HashMap supportedTypes = new HashMap();

    static {
        supportedTypes.put("string", String.class);
        supportedTypes.put("boolean", boolean.class);
        supportedTypes.put("decimal", java.math.BigDecimal.class);
        supportedTypes.put("integer", int.class);
        supportedTypes.put("long", long.class);
        supportedTypes.put("short", short.class);
        supportedTypes.put("byte", byte.class);
        supportedTypes.put("float", float.class);
        supportedTypes.put("double", double.class);
        supportedTypes.put("duration",
            com.sun.jbi.engine.bpel.core.bpel.dt.Duration.class);
        supportedTypes.put("datetime",
            com.sun.jbi.engine.bpel.core.bpel.dt.DateTime.class);
        supportedTypes.put("time",
            com.sun.jbi.engine.bpel.core.bpel.dt.Time.class);
        supportedTypes.put("date",
            com.sun.jbi.engine.bpel.core.bpel.dt.Date.class);
        supportedTypes.put("gyearmonth",
            com.sun.jbi.engine.bpel.core.bpel.dt.GYearMonth.class);
        supportedTypes.put("gyear",
            com.sun.jbi.engine.bpel.core.bpel.dt.GYear.class);
        supportedTypes.put("gmonthday",
            com.sun.jbi.engine.bpel.core.bpel.dt.GMonthDay.class);
        supportedTypes.put("gday",
            com.sun.jbi.engine.bpel.core.bpel.dt.GDay.class);
        supportedTypes.put("gmonth",
            com.sun.jbi.engine.bpel.core.bpel.dt.GMonth.class);
        supportedTypes.put("hexbinary",
            com.sun.jbi.engine.bpel.core.bpel.dt.HexBinary.class);
        supportedTypes.put("base64binary",
            com.sun.jbi.engine.bpel.core.bpel.dt.Base64Binary.class);
        supportedTypes.put("anyuri", String.class);
        supportedTypes.put("qname", String.class);
        supportedTypes.put("notation", String.class);
        supportedTypes.put("anytype", String.class);
        supportedTypes.put("node", org.w3c.dom.Node.class);
    }

    /**
     * gets output type class
     *
     * @param oType output type string
     *
     * @return Class output type class
     */
    public static Class getOutputTypeClass(String oType) {
        if (supportedTypes != null) {
            return (Class) supportedTypes.get(oType.toLowerCase());
        }

        return null;
    }
}
