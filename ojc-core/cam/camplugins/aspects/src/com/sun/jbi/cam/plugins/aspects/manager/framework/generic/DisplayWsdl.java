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
 * @(#)DisplayWsdl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.aspects.manager.framework.generic;

import com.sun.jbi.cam.plugins.aspects.common.AspectsGenericConstants;
import com.sun.jbi.cam.plugins.aspects.manager.framework.common.*;

/**
 *
 * @author ylee
 */
public class DisplayWsdl extends DisplayBase {
  
    /**
     * Creates a new instance of DisplayWsdl
     */
    public DisplayWsdl(String name) {
        super(name,null,null);
        init();
    }
    
    private void init() {
        // set up type based on file extension...
        if ( name!=null ) {
            if ( name!=null && name.endsWith(AspectsGenericConstants.WSDL_SUFFIX) ) {
                setType(AspectsGenericConstants.WSDL_TYPE);
            } else if ( name!=null && name.endsWith(AspectsGenericConstants.XSD_SUFFIX) ) {
                setType(AspectsGenericConstants.XSD_TYPE);
            } else if ( name!=null && name.endsWith(AspectsGenericConstants.ZIP_SUFFIX) ) {
                setType(AspectsGenericConstants.ZIP_TYPE);
            } 
        }
    }
    
    public String getName() {
        return name;
    }   
    
    public String getDesc() {
        return desc;
    }    
    
    public String getStatus() {
        return status;
    }    
    
    public String getType() {
        return type;
    }    
    
}