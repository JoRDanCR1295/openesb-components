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
 * @(#)MapDynamicPropertyHandler.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * Implements the DynamicPropertyHandler interface for java.util.Map.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class MapDynamicPropertyHandler implements DynamicPropertyHandler {

    private static final String[] STRING_ARRAY = new String[0];

    /**
     * Returns string representations of all keys in the map.
     */
    public String[] getPropertyNames(Object object) {
        Map map = (Map) object;
        Set set = map.keySet();
        String names[] = new String[set.size()];
        Iterator it = set.iterator();
        for (int i = 0; i < names.length; i++) {
            names[i] = String.valueOf(it.next());
        }
        return names;
    }

    /**
     * Returns the value for the specified key.
     */
    public Object getProperty(Object object, String propertyName) {
        return ((Map) object).get(propertyName);
    }

    /**
     * Sets the specified key value.
     */
    public void setProperty(Object object, String propertyName, Object value) {
        ((Map) object).put(propertyName, value);
    }
}
