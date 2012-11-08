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
 * @(#)DisplayControl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.generic;

import java.io.Serializable;

/**
 *
 * @author ylee
 */
public class DisplayControl implements Serializable {
    
    private String name;
    private String status;
    private String actions;
          
    
    /** Creates a new instance of DisplayControl */
    public DisplayControl() {
    }

    public DisplayControl(String name,String status,String actions) {
        this.name = name;
        this.status = status;
        this.actions = actions;
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getStatus() {
        return status;
    }    
    
    public void setStatus(String status) {
        this.status = status;
    }        
    
    public String getActions() {
        return actions;
    }    
    
    public void setActions(String actions) {
        this.actions = actions;
    }    
}
