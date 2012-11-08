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
 * @(#)AspectsTabsBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.aspects.manager.framework.generic;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.plugins.aspects.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.common.BaseTabsBean;
import com.sun.jbi.cam.plugins.aspects.common.AspectsGenericConstants;
import com.sun.webui.jsf.component.Tab;
import com.sun.webui.jsf.component.TabSet;
import java.io.Serializable;
import java.util.*;
import java.util.logging.*;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;


/**
 *
 * @author graj
 * @author ylee
 */
public class AspectsTabsBean extends BaseTabsBean implements Serializable {
	private static final long serialVersionUID = 1L;
    final static String CACHE_ASPECT_ENGINE_NAME = "com.sun.aspect.cachese-1.0.2";
    
    protected String url = null;                                                // component URL
    protected Map<String,String> mapUrl = new HashMap<String,String>();         // map of component URLs
    protected String message = "No tab clicked";
    
    // tabs
    private static String STATISTICS_TAB = "Statistics";
    private static String CONTROL_TAB = "Control";
    private static String SERVICE_UNITS_TAB = "ServiceUnits";
    private static String CONFIGURATION_TAB = "Configuration";
    private static String ASPECTS_TAB = "Aspects";

    private static Logger logger = Logger.getLogger(AspectsTabsBean.class.getName());
    
    /**
     * Creates a new instance of AspectsTabsBean
     */
    public AspectsTabsBean() {
    }
    
    
    @Override
    protected void createTabSet() {
        tabSet = new TabSet();
        // build tabs per component context
        this.getRequestParameters();
        
        // remove this tab when the TAB control is fixed (WoodStock)
        addTab(tabSet,"","tabBlank","http://www.sun.com",TARGET_FRAME).setVisible(false);
        
        String linkString = null;
        //
        //if ( GenericConstants.DOMAIN_SERVER.equals(tName) ) {
        //    linkString = this.createComponentUrl("/../cam/faces/manager/framework/generic/general.jsp", componentName, componentType, cName, cType, pName, tName);
        //    addTab(tabSet,Messages.getString("generic_General"),"tabGeneral",linkString,TARGET_FRAME);
        //} else {
            //////////////////
            // Statistics
            //////////////////
            linkString = this.createComponentUrl("/../cam/faces/manager/framework/generic/statistics.jsp", componentName, componentType, cName, cType, pName, tName);
            //addTab(tabSet,Messages.getString("generic_Statistics"),"tabStatistics",null,null,"#{TabsBean.setStatisticsUrl}");
            //addTab(tabSet,Messages.getString("generic_Statistics"),"tabStatistics",null,null,null);
            addTab(tabSet,Messages.getString("generic_Statistics"),"tabStatistics",linkString,TARGET_FRAME);
            mapUrl.put(STATISTICS_TAB,linkString);
            
            // add the Chart Statistics tab - move to Statistics tab as a toggle button
            //addTab(tabSet,Messages.getString("generic_ChartStatistics"),"tabChartStatistics","/faces/manager/framework/generic/ChartStatistics.jsp",TARGET_FRAME);
            
            //////////////////
            // Control
            //////////////////
            
            if ( GenericConstants.SU_TYPE.equals(componentType)==false ) {
                linkString = this.createComponentUrl("/../cam/faces/manager/framework/generic/control.jsp", componentName, componentType, cName, cType, pName, tName);
                //addTab(tabSet,Messages.getString("generic_Control"),"tabControl",null,null,"#{TabsBean.setControlUrl}");
                //addTab(tabSet,Messages.getString("generic_Control"),"tabControl",null,null,null);
                addTab(tabSet,Messages.getString("generic_Control"),"tabControl",linkString,TARGET_FRAME);
                mapUrl.put(CONTROL_TAB,linkString);
            }
            
            //////////////////
            // Configuration
            //////////////////
            if ( GenericConstants.SU_TYPE.equals(componentType)==false ) {
                linkString = this.createComponentUrl("/faces/manager/framework/generic/configuration.jsp", componentName, componentType, cName, cType, pName, tName);
            } else {
                if(true == cName.equals(CACHE_ASPECT_ENGINE_NAME)) {
                    linkString = this.createUnitUrl("/faces/manager/framework/generic/cacheaspectunitconfiguration.jsp", componentName, componentType, cName, cType, pName, tName, suName);
                } else {
                    linkString = this.createUnitUrl("/faces/manager/framework/generic/unitconfiguration.jsp", componentName, componentType, cName, cType, pName, tName, suName);
                }
            }
            //addTab(tabSet,Messages.getString("generic_Configuration"),"tabConfiguration",null,null,"#{TabsBean.setConfigurationUrl}");
            //addTab(tabSet,Messages.getString("generic_Configuration"),"tabConfiguration",null,null,null);
            addTab(tabSet,Messages.getString("generic_Configuration"),"tabConfiguration",linkString,TARGET_FRAME);
            mapUrl.put(CONFIGURATION_TAB,linkString);

            
            //////////////////
            // ServiceUnits
            //////////////////
            // check containers
            if ( GenericConstants.BC_TYPE.equals(componentType) || GenericConstants.SE_TYPE.equals(componentType) ) {
                // add Service Unit tab
                linkString = this.createComponentUrl("/faces/manager/framework/generic/serviceUnits.jsp", componentName, componentType, cName, cType, pName, tName);
                //addTab(tabSet,Messages.getString("generic_ServiceUnits"),"tabSU", null,null,"#{TabsBean.setServiceUnitsUrl}");
                //addTab(tabSet,Messages.getString("generic_ServiceUnits"),"tabSU", null,null,null);
                addTab(tabSet,Messages.getString("generic_ServiceUnits"),"tabSU",linkString,TARGET_FRAME);
                mapUrl.put(SERVICE_UNITS_TAB,linkString);
            }

            
            //////
            // add aspects tab
            linkString = this.createComponentUrl("/faces/manager/framework/policygroups/policygroups.jsp", componentName, componentType, cName, cType, pName, tName);
            //addTab(tabSet,Messages.getString("generic_aspects"),"tabAspects", null,null,"#{TabsBean.setAspectsUrl}");
            //addTab(tabSet,Messages.getString("generic_aspects"),"tabAspects", null,null,null);
            addTab(tabSet,Messages.getString("generic_aspects"),"tabAspects",linkString,TARGET_FRAME);
            mapUrl.put(ASPECTS_TAB,linkString);

        //}
    }
    
    public String setStatisticsUrl() {
        logger.info("set URL"+STATISTICS_TAB);
        setUrl(STATISTICS_TAB);
        message = "Statistics tab clicked";
        return "";
    }
    
    public String setControlUrl() {
        logger.info("set URL"+CONTROL_TAB);
        setUrl(CONTROL_TAB);
        message = "Control tab clicked";
        return "";
    }
    
    public String setServiceUnitsUrl() {
        logger.info("set URL"+SERVICE_UNITS_TAB);
        setUrl(SERVICE_UNITS_TAB);
        message = "Service Units tab clicked";
        return "";
    }
    
    public String setConfigurationUrl() {
        logger.info("set URL"+CONFIGURATION_TAB);
        setUrl(CONFIGURATION_TAB);
        message = "Configuration tab clicked";
        return "";
    }
    
    public String setAspectsUrl() {
        logger.info("set URL"+ASPECTS_TAB);
        setUrl(ASPECTS_TAB);
        message = "Aspects tab clicked";
        return "";
    }
    
    public void setUrl(String tab) {
        url = mapUrl.get(tab);
    }
    
    public String getUrl() {
        logger.info("get url: "+url);
        return url;
    }

    
    public String getMessage() {
        return message;
    }
    
    public String tab1Clicked() {
        System.out.println("Tab1 clicked.");
        message = "tab1 clicked";
        url = "http://www.sun.com";
        return "";
    }
    
    public String tab2Clicked() {
        System.out.println("Tab2 clicked.");
        message = "tab2 clicked";
        url = "http://www.yahoo.com";
        return "";
    }
    
    public String tab3Clicked() {
        System.out.println("Tab3 clicked.");
        message = "tab3 clicked";
        url = "http://www.google.com";
        return "";
    }      
}
