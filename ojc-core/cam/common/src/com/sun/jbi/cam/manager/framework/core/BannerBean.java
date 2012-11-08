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
 * @(#)BannerBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.core;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.RequestDispatcher;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/**
 *
 * @author ylee
 */
public class BannerBean extends BaseBean {
    
    private static final String REDIRECT_LOGIN = "redirect-login";
    
    /** Creates a new instance of BannerBean */
    public BannerBean() {
    }
    
    public String logout() {
        System.out.println("Logout called...");
        
        // invalidate session
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();
        HttpSession session = request.getSession();           
        session.invalidate();
        
        return REDIRECT_LOGIN;
    }
    
    
    public String help() {
         System.out.println("help called...");
         
         return "";
    }
    
    public String version() {
         System.out.println("version called...");
         
         return "";
    }
    
    
}
