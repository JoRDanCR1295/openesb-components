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
 * @(#)BPTabsBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.plugins.bpelse.common.BpelsePluginMessages;
import com.sun.jbi.cam.manager.framework.common.BaseTabsBean;
import com.sun.webui.jsf.component.TabSet;
import java.io.Serializable;

/**
 *
 * @author ylee
 */
public class BPTabsBean extends BaseTabsBean implements Serializable {
    
    /** Creates a new instance of BpelTabsBean */
    public BPTabsBean() {
    }
    
    
    public void createTabSet() {
        super.createTabSet();
        
        String strUrl = null;
        
        // @todo remove blank tab
        addTab(tabSet,"","tabBlank","http://www.sun.com",TARGET_FRAME).setVisible(false);
        
        if ( GenericConstants.DOMAIN_SERVER.equals(tName) ) {
	        addTab(tabSet,Messages.getString("generic_General"),"tabGeneral","/../cam/faces/manager/framework/generic/general.jsp",TARGET_FRAME);
        } else {
	        
        	//
        	// Generic Tabs
        	//
        	
	        //////////////////
            // Statistics
            //////////////////
        	strUrl = this.createComponentUrl("/../cam/faces/manager/framework/generic/statistics.jsp", componentName, componentType, cName, cType, pName, tName);
            addTab(tabSet,Messages.getString("generic_Statistics"),"tabStatistics",strUrl,TARGET_FRAME);

	        //////////////////
            // Control
            //////////////////
	        if ( GenericConstants.SU_TYPE.equals(componentType)==false ) {
	        	strUrl = this.createComponentUrl("/../cam/faces/manager/framework/generic/control.jsp", componentName, componentType, cName, cType, pName, tName);
	        	addTab(tabSet,Messages.getString("generic_Control"),"tabControl",strUrl,TARGET_FRAME);
	        }
	        
	        //////////////////
            // Configuration
            //////////////////
            if ( GenericConstants.SU_TYPE.equals(componentType)==false ) {
                strUrl = this.createComponentUrl("/../cam/faces/manager/framework/generic/configuration.jsp", componentName, componentType, cName, cType, pName, tName);
            } else {
            	// currently there is no generic SU config page (unitconfiguration.jsp) available
            	// in the framework (CAM).
                //strUrl = this.createUnitUrl("/../cam/faces/manager/framework/generic/unitconfiguration.jsp", componentName, componentType, cName, cType, pName, tName, suName);
            	strUrl = this.createUnitUrl("/../cam/faces/manager/framework/generic/configuration.jsp", componentName, componentType, cName, cType, pName, tName, suName);
            }
            addTab(tabSet,Messages.getString("generic_Configuration"),"tabConfiguration",strUrl,TARGET_FRAME);
            
	        //////////////////
            // Service Units
            //////////////////
	        if ( GenericConstants.BC_TYPE.equals(componentType) || GenericConstants.SE_TYPE.equals(componentType) ) {
	        	strUrl = this.createComponentUrl("/../cam/faces/manager/framework/generic/serviceUnits.jsp", componentName, componentType, cName, cType, pName, tName);
                addTab(tabSet,Messages.getString("generic_ServiceUnits"),"tabSU",strUrl,TARGET_FRAME);
	        }
	        
	        //
	        // Custom BPEL Tabs
	        //
	        strUrl = this.createComponentUrl("/faces/tabs/instancesTable.jsp", componentName, componentType, cName, cType, pName, tName);
	        addTab(tabSet,BpelsePluginMessages.getString("bpelse_instances_tab_title"),"tabInstances",strUrl,TARGET_FRAME);
	        
	        strUrl = this.createComponentUrl("/faces/tabs/bpVisualizer.jsp", componentName, componentType, cName, cType, pName, tName);	        	
            addTab(tabSet,BpelsePluginMessages.getString("bpelse_charts_tab_title"),"tabbpVisualizer",strUrl,TARGET_FRAME);
        }
    }
    
}
