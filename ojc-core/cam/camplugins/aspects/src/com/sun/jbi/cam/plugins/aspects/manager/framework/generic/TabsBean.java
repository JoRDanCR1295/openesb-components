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
 * @(#)TabsBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.aspects.manager.framework.generic;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.common.BaseTabsBean;
import com.sun.webui.jsf.component.Tab;
import com.sun.webui.jsf.component.TabSet;
import java.io.Serializable;

/**
 *
 * @author ylee
 */
public class TabsBean extends BaseTabsBean implements Serializable {
    

    /** Creates a new instance of TabsBean */
    public TabsBean() {
    }
    
    
    protected void createTabSet() {
        tabSet = new TabSet();
        // build tabs per component context
        getRequestParameters();
        
        // remove this tab when the TAB control is fixed (WoodStock)
        addTab(tabSet,"","tabBlank","http://www.sun.com",TARGET_FRAME).setVisible(false);

        String linkString = null;
        if ( GenericConstants.DOMAIN_SERVER.equals(tName) ) {
                linkString = this.createUrl("/../cam/faces/manager/framework/generic/general.jsp", componentName, componentType, cName, cType, pName, tName);    
	        addTab(tabSet,Messages.getString("generic_General"),"tabGeneral",linkString,TARGET_FRAME);
        } else {
                linkString = this.createUrl("/../cam/faces/manager/framework/generic/statistics.jsp", componentName, componentType, cName, cType, pName, tName);    
	        addTab(tabSet,Messages.getString("generic_Statistics"),"tabStatistics",linkString,TARGET_FRAME);
	        // add the Chart Statistics tab - move to Statistics tab as a toggle button
	        //addTab(tabSet,Messages.getString("generic_ChartStatistics"),"tabChartStatistics","/faces/manager/framework/generic/ChartStatistics.jsp",TARGET_FRAME);
	        if ( GenericConstants.SU_TYPE.equals(componentType)==false ) {
                    linkString = this.createUrl("/../cam/faces/manager/framework/generic/control.jsp", componentName, componentType, cName, cType, pName, tName);    
	            addTab(tabSet,Messages.getString("generic_Control"),"tabControl",linkString,TARGET_FRAME);
	        }
                linkString = this.createUrl("/faces/manager/framework/generic/configuration.jsp", componentName, componentType, cName, cType, pName, tName);    
	        addTab(tabSet,Messages.getString("generic_Configuration"),"tabConfiguration",linkString,TARGET_FRAME);
	        // check containers
	        if ( GenericConstants.BC_TYPE.equals(componentType) || GenericConstants.SE_TYPE.equals(componentType) ) {
	            // add Service Unit tab
                    linkString = this.createUrl("/../cam/faces/manager/framework/generic/serviceUnits.jsp", componentName, componentType, cName, cType, pName, tName);    
	            addTab(tabSet,Messages.getString("generic_ServiceUnits"),"tabSU", linkString,TARGET_FRAME);
	        }
        }
    }
    
    /**
     *
     */
    private String createUrl(String linkString, String name, String componentType, String ctype, String cname, String parentName, String targetName) {
        if ( parentName==null ) {
            parentName = name;
        }
        
        String urlString = linkString
        +"?"+GenericConstants.COMPONENT_NAME+"="+name+"&"
                +GenericConstants.COMPONENT_TYPE+"="+componentType+"&"
                +GenericConstants.COMPONENT_CNAME+"="+cname+"&"
                +GenericConstants.COMPONENT_CTYPE+"="+ctype+"&"
                +GenericConstants.COMPONENT_PNAME+"="+parentName+"&"
                +GenericConstants.COMPONENT_TNAME+"="+targetName;
        
        return urlString;
    }    
    

    
}
