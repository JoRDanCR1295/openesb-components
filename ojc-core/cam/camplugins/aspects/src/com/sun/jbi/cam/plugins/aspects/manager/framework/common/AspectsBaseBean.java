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
 * @(#)AspectsBaseBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.aspects.manager.framework.common;

import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.plugins.aspects.common.AspectsGenericConstants;
import java.io.Serializable;
import java.util.logging.Logger;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

/**
 *
 * @author graj
 */
public class AspectsBaseBean extends BaseBean implements Serializable {
	private static final long serialVersionUID = 1L; 
    transient private Logger logger = Logger.getLogger(AspectsBaseBean.class.getName());
    protected String serviceUnitName;
    
    /** Creates a new instance of AspectsBaseBean */
    public AspectsBaseBean() {
    }
    
    
}
