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
 * @(#)BaseTabsBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.common;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.JsfUtility;
import com.sun.webui.jsf.component.Tab;
import com.sun.webui.jsf.component.TabSet;
import java.io.Serializable;
import java.util.logging.Logger;

/**
 * Sun Microsystems
 * @author ylee
 * @author graj
 */
public class BaseTabsBean extends BaseBean implements Serializable {
    
    protected TabSet tabSet = null;
    protected static String TARGET_FRAME= "tabBottomFrame";      //$NON-NLS-1$
    
    private static Logger logger = Logger.getLogger(BaseTabsBean.class.getName());
    
    /** Creates a new instance of BaseTabsBean */
    public BaseTabsBean() {
    }
    
    /**
     * create a new TabSet object
     */
    protected void createTabSet() {
        tabSet = new TabSet();
        // build tabs per component context
        getRequestParameters();
    }
    
    /**
     * get the TabSet object
     */
    public TabSet getTabSet() {
        createTabSet();
        return tabSet;
    }

    
   /**
    * create a new tab based on the following input parameters
    * @param    parent      parent tabset
    * @param    label       tab label
    * @param    id      
    * @param    url
    * @param    target 
    * @return   new Tab object
    */       
   @SuppressWarnings("unchecked")
    public Tab addTab(TabSet parent, String label, String id,  String url, String target) {
        Tab tab = new Tab(label);
        tab.setId(id);
        tab.setUrl(url);
        tab.setTarget(target);
        parent.getChildren().add(tab);
        return tab;
    }

   /**
    * create a new tab based on the following input parameters
    * @param    parent      parent tabset
    * @param    label       tab label
    * @param    id      
    * @param    url
    * @param    target 
    * @param    action      name of Backingbean method to invoke
    * @return   new Tab object
    */       
   @SuppressWarnings("unchecked")
   public Tab addTab(TabSet parent, String label, String id,  String url, String target, String actionMethod) {
       Tab tab = new Tab(label);
       tab.setId(id);
       if ( url!=null ) {
           tab.setUrl(url);
       }
       if ( target!=null ) {
           tab.setTarget(target);
       }
       if ( actionMethod!=null ) {
           logger.info("setting method: "+actionMethod);
           JsfUtility.setMethodExpression(tab,"actionExpression", actionMethod);             //$NON-NLS-1$
       }
       parent.getChildren().add(tab);
       return tab;
   }

   protected String createComponentUrl(String linkString, String name, String componentType, String cname, String ctype, String parentName, String targetName) {
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
       
       System.out.println("**** Creating Component URL:\n"+urlString);
       
       return urlString;
   }
   
   protected String createUnitUrl(String linkString, String name, String componentType, String cname, String ctype, String parentName, String targetName, String serviceUnitName) {
       if ( parentName==null ) {
           parentName = name;
       }
       
       String urlString = linkString
               +"?"+GenericConstants.COMPONENT_NAME+"="+name
               +"&"+GenericConstants.COMPONENT_TYPE+"="+componentType
               +"&"+GenericConstants.COMPONENT_CNAME+"="+cname
               +"&"+GenericConstants.COMPONENT_CTYPE+"="+ctype
               +"&"+GenericConstants.COMPONENT_PNAME+"="+parentName
               +"&"+GenericConstants.COMPONENT_TNAME+"="+targetName
               +"&"+GenericConstants.COMPONENT_SUNAME+"="+serviceUnitName;
       
       System.out.println("**** Creating Unit URL:\n"+urlString);
       
       return urlString;
   }
   
}
