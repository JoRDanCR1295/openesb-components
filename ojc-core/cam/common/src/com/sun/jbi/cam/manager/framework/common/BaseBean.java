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
 * @(#)BaseBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.common;

import com.sun.data.provider.TableDataProvider;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.resources.Messages;
import com.sun.jbi.cam.services.ServiceManager;
import com.sun.jbi.cam.services.ServiceManagerFactory;
import java.io.Serializable;
import java.util.logging.Logger;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 *
 * @author ylee
 */
public class BaseBean implements Serializable {
    
    transient private Logger logger = Logger.getLogger(BaseBean.class.getName());
    
    protected TableDataProvider provider = null;                  // Data provider.    
    
    protected String componentName;
    protected String componentType;
    protected String cType;
    protected String cName;
    protected String pName;
    protected String tName;
    protected String suName;
    
    protected ServiceManager serviceManager;
    
    
    /** Creates a new instance of BaseBean */
    public BaseBean() {
    }
    
    protected void getRequestParameters() {
        HttpServletRequest request = getRequest();
        componentName = (String)getParameter(request,GenericConstants.COMPONENT_NAME);
        componentType= (String)getParameter(request,GenericConstants.COMPONENT_TYPE);
        cType = (String)getParameter(request,GenericConstants.COMPONENT_CTYPE);
        cName = (String)getParameter(request,GenericConstants.COMPONENT_CNAME);
        pName = (String)getParameter(request,GenericConstants.COMPONENT_PNAME);
        tName = (String)getParameter(request,GenericConstants.COMPONENT_TNAME);
        
        suName = (String)getParameter(GenericConstants.COMPONENT_SUNAME);
        validateRequestParameters();
    }
    
    
    protected void validateRequestParameters() {
        if ( cType==null ) {
            cType = GenericConstants.HASH_SEPARATOR;        // self
        }
        if ( cName==null ) {
            cName = GenericConstants.HASH_SEPARATOR;        // self
        }
    }
    
    protected Object getParameter(HttpServletRequest request,String paramName) {
    	Object paramValue = request.getParameter(paramName);
    	if ( paramValue==null ) {
            paramValue = request.getSession().getAttribute(paramName);
    	}
    	return paramValue;
    }
    
    /**
     * getParameter
     * @param paramName     name of parameter
     */
    protected Object getParameter(String paramName) {
        HttpServletRequest request = getRequest();
        Object paramValue = getParameter(request,paramName);
        return paramValue;
    }

    /**
     * setParameter - store parameter in session
     * @param   key
     * @param   value
     */
    protected void  setParameter(String key, Object value) {
        HttpServletRequest request = getRequest();
        request.getSession().setAttribute(key,value);
    }
    
    protected HttpServletRequest getRequest() {
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();       
        return request;
    }
    
    protected ServletContext getServletContext() {
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext externalContext = context.getExternalContext();
        ServletContext servletContext = (ServletContext) externalContext.getContext();
        return servletContext;
    }
    
    protected void getServiceManager() {
        serviceManager = ServiceManagerFactory.getServiceManager(componentName,componentType);
    }

    protected  void setup() {
        getRequestParameters();
        getServiceManager();
    }
    
    
    public String getName() {
        String name = componentName;
        if ( GenericConstants.SU_TYPE.equals(componentType) ) {
            if ( pName!=null ) {
                name = pName+"-"+componentName;
            }
        }
        return name;
    }
    

    public String getTitle(String msgid) {
        return getName()+" - " + Messages.getString(msgid);
    }    
    

    public String getTableTitle(String bcTableMsgId, String seTableMsgId, String suTableMsgId) {
        String label = "";
        if ( GenericConstants.BC_TYPE.equals(componentType) ) {
            label = Messages.getString(bcTableMsgId);
        } else if ( GenericConstants.SE_TYPE.equals(componentType) ) {
            label = Messages.getString(seTableMsgId);
        } else if ( GenericConstants.SU_TYPE.equals(componentType) ) {
            label = Messages.getString(suTableMsgId);
        }
        return label;
    }    
    
}
