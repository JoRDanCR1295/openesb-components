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
 * @(#)$Id: Displayer.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7.cli.display;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 *
 * @author  ylee
 */
public class Displayer {
    
    public void display(Object obj) {
        String clsName = obj.getClass().getCanonicalName();
        Class cls = obj.getClass();
        //System.out.append("clsName: " + clsName);
        if ("java.lang.String[]".equals(clsName)) {
            String[] list = (String[]) obj;
            for (int i = 0; i < list.length; i++) {
                System.out.println(list[i]);
            }
        } else if ( java.util.List.class.isAssignableFrom(cls) ) {
            List<String> list = (List)obj;
            for (int i = 0; i < list.size(); i++) {
                System.out.println(list.get(i));
            }
        } else if ( java.util.Map.class.isAssignableFrom(cls) ) {
            Map<String,Object> map = (Map) obj;
            Iterator iterator = map.keySet().iterator();
            while (iterator.hasNext()) {
                String key = (String) iterator.next();
                String value = (String) map.get(key);
                System.out.println(key + " = " + value);
            }

        } else {
            System.out.println(obj.toString());
        }
    }
}
