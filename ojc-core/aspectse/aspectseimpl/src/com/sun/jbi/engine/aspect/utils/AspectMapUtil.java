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
 * @(#)AspectMapUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.utils;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.sun.jbi.engine.aspect.endpoint.factory.AspectMap;

/**
 * This class contains the utility methods.
 *
 * @author karthikeyan s
 */
public class AspectMapUtil {
    
    /**
     * Creates a new instance of AspectMapUtil
     */
    public AspectMapUtil() {
    }
    
    public static Element getOutputForID(Element root, String id) {
        NodeList outputs = root.getElementsByTagName(AspectMap.OUTPUT_ELEMENT);
        for(int i = 0; i < outputs.getLength(); i++) {
            Element output = (Element)outputs.item(i);
            if(output.getAttribute(AspectMap.ID_ATTR).equalsIgnoreCase(id)) {
                return output;                
            }
        }
        return null;
    }    
}
