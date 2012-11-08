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

package com.sun.jbi.cam.manager.framework.aspects;

import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.manager.framework.common.BaseTabsBean;
import com.sun.webui.jsf.component.TabSet;
import java.io.Serializable;

/**
 *
 * @author ylee
 */
public class AspectsTabsBean extends BaseTabsBean implements Serializable {
    
    /** Creates a new instance of AspectsTabBean */
    public AspectsTabsBean() {
    }
    
    
    protected void createTabSet() {
        
        tabSet = new TabSet();
        // build tabs per component context
        getRequestParameters();
        
        // remove this tab when the TAB control is fixed (WoodStock)
        addTab(tabSet,"","tabBlank","http://www.sun.com",TARGET_FRAME).setVisible(false);
        
        addTab(tabSet,Messages.getString("aspects_PolicyGroupManagement"),"tabPolicyGroups","/../aspects/faces/aspects/policygroups.jsp",TARGET_FRAME);
        addTab(tabSet,Messages.getString("aspects_CatalogManagement"),"tabCatalog","/../aspects/faces/aspects/catalog.jsp",TARGET_FRAME);
    }
        
    
}
