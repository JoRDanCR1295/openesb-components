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
 * @(#)RealTimeSUStateHelper.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.renderers.svg;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.generic.ServiceUnitsBean;
import com.sun.jbi.cam.model.management.JBIServiceUnitStatus;
import java.util.List;

/**
 *
 * @author rdamir
 */
final class RealTimeSUStateHelper extends ServiceUnitsBean{
    
    // These value need to be kept in sync with the javascript
    // in SAControl.jsp
    private static final String STARTED ="=1";
    private static final String STOPPED ="=2";
    private static final String SHUTDOWN ="=3";
    private static final String PAIR_SEP =";";
    
    private String componentName;
   
    /** Creates a new instance of RealTimeSUStateHelper */
    public RealTimeSUStateHelper(String componentName) {
        this.componentName = componentName;
    }
    

    /*
     * the method will return a semiColon seperated name value pairs
     *  name represent the service unit in question
     *  value can have the following values:
     *     STARTED = "1"
     *     STOPPED ="2";
     *     SHUTDOWN ="3";
     */
    public String getServiceUnitsState() {
        StringBuffer stateInfo = new StringBuffer();
        List<JBIServiceUnitStatus> suStatusList = 
               getSAServiceUnitStatusList(pName,tName);
        int serviceUnitCount = suStatusList.size();
        for (int index = 0; index < serviceUnitCount; index++) {
            JBIServiceUnitStatus su = suStatusList.get(index);
            String suStatus = su.getStatus();
            String suName = su.getServiceUnitName();
            if(suStatus.equals(Messages.getString("state.Started"))) {
                suStatus = STARTED;
            } else if (suStatus.equals(Messages.getString("state.Stopped"))) {
                suStatus = STOPPED;
            } else if (suStatus.equals(Messages.getString("state.Shutdown"))) {
                suStatus = SHUTDOWN;
            }
            stateInfo.append(suName+suStatus);
            if(index < serviceUnitCount-1){
                stateInfo.append(PAIR_SEP);
            }
        }
        return stateInfo.toString();
        
    }
}
