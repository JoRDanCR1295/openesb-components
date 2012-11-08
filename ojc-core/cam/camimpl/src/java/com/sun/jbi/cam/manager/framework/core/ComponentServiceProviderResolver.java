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
 * @(#)ComponentServiceProviderResolver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.core;

import com.sun.jbi.cam.common.GenericConstants;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Make this class a singleton
 *
 * @author ylee
 * 
 */
public class ComponentServiceProviderResolver {
    
    private Map<String,String> providers = new HashMap<String,String>();
    
    private static String DEFAULT_COMPONENT_SERVICE_PROVIDER_URL = "http://www.sun.com";      //$NON-NLS-1$
    
    private static Logger logger = Logger.getLogger(ComponentServiceProviderResolver.class.getName());
    
    private static ComponentServiceProviderResolver instance;
    
    
    public static ComponentServiceProviderResolver getInstance() {
        if ( instance==null ) {
           instance = new  ComponentServiceProviderResolver();
        }
        return instance;
    }
    
    /** Creates a new instance of ServiceResolver */
    private ComponentServiceProviderResolver() {
        init();
    }
    
    private void init() {
        // add default service providers
        providers.put(GenericConstants.SA_TYPE,GenericConstants.SERVICE_ASSEMBLY_VIEWER_URL);
        providers.put(GenericConstants.SU_TYPE,"/faces/manager/framework/generic/tabsFrame.jsp");  //$NON-NLS-1$
        providers.put(GenericConstants.SE_TYPE,"/faces/manager/framework/generic/tabsFrame.jsp");  //$NON-NLS-1$
        providers.put(GenericConstants.BC_TYPE,"/faces/manager/framework/generic/tabsFrame.jsp");  //$NON-NLS-1$
        
        // plug-ins registrations here ...
        providers.put("com.sun.httpsoapbc-1.0-2","/faces/manager/framework/generic/tabsFrame.jsp");  //$NON-NLS-1$
        
    }
    
    public void addProvider(String type, String url) {
        if ( type==null || url==null ) {
            // bad input parameters
            logger.severe("Invalid name="+type+" or url="+url);
        } else {
            providers.put(type,url);
        }
    }
    
    public void removeProvider(String type) {
        if ( type==null ) {
            // bad input parameters
            logger.severe("Invalid name="+type);
        } else {
            providers.remove(type);
        }
    }    
    
    public String getProviderUrl(String type) {
        String url = null;
        if ( type!=null ) {
             url = providers.get(type);
        }
        if ( url == null ) {
            // use default
            url = DEFAULT_COMPONENT_SERVICE_PROVIDER_URL;
        }
        return url;
    }

    public String getProviderUrl(String name, String type, String ctype, String cname) {
        String url = null;
        String searchString = null;
        
        // 1. search with name, type, ctype, cname 
        searchString = name+GenericConstants.COLON_SEPARATOR+type+GenericConstants.COLON_SEPARATOR+ctype+GenericConstants.COLON_SEPARATOR+cname;
        url = providers.get(searchString);
        if ( url==null ) {
            // 2. search with type, ctype, cname (SU)
            searchString = type+GenericConstants.COLON_SEPARATOR+ctype+GenericConstants.COLON_SEPARATOR+cname;
            url = providers.get(searchString);
            // 3. search with name, type (SE/BC)
            if ( url==null ) {
                searchString = name+GenericConstants.COLON_SEPARATOR+type;
                url = providers.get(searchString);
                // 4. get generic type
                if ( url == null ) {
                    return getProviderUrl(type);
                }
            }
        }
        
        return url;
    }
    
    
    public Map<String,String> getMappings() {
        return Collections.unmodifiableMap(providers);
    }
}
