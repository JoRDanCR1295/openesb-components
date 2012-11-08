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
 * @(#)GroupImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.clientapi;

import java.security.Principal;
import java.security.acl.Group;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Vector;

public class GroupImpl implements Group {
    
    private Vector<Principal> mMembers = new Vector<Principal> ();
    
    private String mName;
    
    

    public GroupImpl(String name) {
         mName = name;
    }

    public boolean addMember(Principal user) {
        // TODO Auto-generated method stub
        mMembers.add(user);
        return true;
    }

    public boolean isMember(Principal member) {
        // TODO Auto-generated method stub
        
        return mMembers.contains(member);
    }

    public Enumeration<? extends Principal> members() {
        // TODO Auto-generated method stub
        return mMembers.elements();
    }

    public boolean removeMember(Principal user) {
        // TODO Auto-generated method stub
        return mMembers.removeElement(user);
    }

    public String getName() {
        // TODO Auto-generated method stub
        return mName;
    }
    
    

}
