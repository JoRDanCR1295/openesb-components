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
 * @(#)DisplayBase.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.aspects.manager.framework.common;

import com.sun.jbi.cam.common.EqualsUtil;
import com.sun.jbi.cam.common.HashCodeUtil;

/**
 *
 * @author ylee
 */
public class DisplayBase {
    
    protected String name;
    protected String desc;
    protected String status;
    protected String type;
    protected String action;
    protected boolean selected;
    
    /**
     * Creates a new instance of DisplayBase class
     */
    public DisplayBase(String name, String desc, String status) {
        this(name,desc,status,null);
    }
    
    public DisplayBase(String name, String desc, String status,String type) {
        this.name = name;
        this.desc = desc;
        this.status = status;
        this.type = type;
    }
    
    public String getName() {
        return name;
    }
    
    public void setName(String name) {
        this.name = name;
    }
    
    public String getDesc() {
        return desc;
    }
    
    public void setDesc(String desc) {
        this.desc = desc;
    }
    
    public String getStatus() {
        return status;
    }
    
    public void setStatus(String status) {
        this.status = status;
    }
    
    public String getType() {
        return type;
    }
    
    public void setType(String type) {
        this.type = type;
    }
    
    public String getAction() {
        return action;
    }
    
    public void setAction(String action) {
        this.action = action;
    }
    
    public void setSelected(boolean b) {
        selected = b;
    }
    
    public boolean getSelected() {
        return selected;
    } 
    
    public boolean equals(Object aThat) {
        // check for self-comparison
        if (this == aThat) {
            return true;
        }
        
        // use instanceof instead of getClass here for two reasons
        // 1. if need be, it can match any supertype, and not just one class;
        // 2. it renders an explict check for "that == null" redundant, since
        // it does the check for null already - "null instanceof [type]" always
        // returns false. (See Effective Java by Joshua Bloch.)
        if (!(aThat instanceof DisplayBase)) {
            return false;
        }
        // Alternative to the above line :
        // if ( aThat == null || aThat.getClass() != this.getClass() ) return
        // false;
        
        // cast to native object is now safe
        DisplayBase that = (DisplayBase) aThat;
        
        // now a proper field-by-field evaluation can be made
        return EqualsUtil.areEqual(this.name, that.name);
        
    }
    
    public int hashCode() {
        int result = HashCodeUtil.SEED;
        result = HashCodeUtil.hash(result, this.name);
        return result;
    }
    
}