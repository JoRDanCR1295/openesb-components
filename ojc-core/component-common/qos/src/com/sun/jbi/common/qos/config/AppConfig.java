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
 * @(#)AppConfig.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.qos.config;

import java.util.Map;
import java.util.TreeMap;

/**
 * Represents an instance of application configuration.
 * 
 * @author Kevan Simpson
 */
public class AppConfig extends AbstractConfig {
    private String mName;
    private Map<String, Property> mMetadata;
    
    AppConfig(String name, Property[] props /*Map<String, Property> metadata*/) {
        setName(name);
        mMetadata = new TreeMap<String, Property>(/*metadata*/);
        if (props != null) {
            for (Property p : props) {
                mMetadata.put(p.getName(), p);
                addProperty(new Property(p)); // instance values
            }
        }
    }

    /**
     * @return the name
     */
    public String getName() {
        return mName;
    }

    /**
     * @param name the name to set
     */
    public void setName(String name) {
        mName = name;
    }
    
//    public AppConfig toAppConfig(String name, CompositeData data) {
//        
//    }
}
