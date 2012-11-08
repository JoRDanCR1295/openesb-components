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
 * @(#)RealTimeStatisticHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import com.sun.jbi.cam.manager.framework.generic.StatisticsBean;

/**
 *
 * @author rdamir
 */
 final class RealTimeStatisticHelper extends StatisticsBean {

    /** Creates a new instance of RealTimeStatisticHelper */
    public RealTimeStatisticHelper() {
    }
    
    protected void setup(String componentName,String componentType,
            String cname,String ctype,String pname,String targetName) {
       super.cName = cname;
       super.componentName = componentName;
       super.componentType = componentType;
       super.cType = ctype;
       super.pName = pname;
       super.tName = targetName;
       super.getStatisticsService();
    }
    void setCName(String name) {
        super.cName = name;
    }
    
    void setComponentName(String componentName) {
        super.componentName = componentName;
    }
    
    void setComponentType(String componentType) {
        super.componentType = componentType;
    }
    
    void setCType(String type) {
        super.cType = type;
    }
    
    void setPName(String name) {
        super.pName = name;
    }

    void setTName(String targetName) {
       super.tName = targetName;
    }
   
    
}
