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
 * @(#)BPELWorkflowRequest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow;

import javax.security.auth.Subject;

import org.w3c.dom.Element;

import com.sun.jbi.workflow.model.Task;

public class BPELWorkflowRequest implements WorkflowRequest {
    private String mId;
    private String mRelId;
    private Element mInput;
    private Task mTaskModel;
    
    public BPELWorkflowRequest (String id, String meId, Element input, Task tasks) {
        mId = id;
        mInput = input;
        mTaskModel = tasks;
        mRelId = meId;
        
    }
    
    public String getId() {
        // TODO Auto-generated method stub
        return mId;
    }

    public RequestType getType() {
        // TODO Auto-generated method stub
        return WorkflowRequest.RequestType.BPEL;
    }

    public Element getInput() {
        return mInput;
    }

    public Task getTaskModel() {
        return mTaskModel;
    }

    public Subject getSubject() {
        // TODO Auto-generated method stub
        return null;
    }
    public String getRelId () {
        return mRelId;
    }

}
