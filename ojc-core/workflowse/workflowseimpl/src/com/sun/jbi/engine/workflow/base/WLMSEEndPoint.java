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
 * @(#)$Id: WLMSEEndPoint.java,v 1.1 2010/02/15 19:25:08 fkieviet Exp $
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow.base;

import java.util.ArrayList;
import java.util.List;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpoint;
import com.sun.jbi.engine.workflow.WorkflowMapEntry;

public class WLMSEEndPoint extends AbstractEndpoint<Object> {

    private List<String> mServiceUnitNames;

    private WorkflowMapEntry mEntry;

    public WLMSEEndPoint(EndpointInfo info) {
        super(info);
        // TODO Auto-generated constructor stub
        mServiceUnitNames = new ArrayList<String>();
    }

    public WLMSEEndPoint(EndpointInfo info, ServiceUnit srvcUnit) {
        this(info);
        mServiceUnitNames = new ArrayList<String>();
        mServiceUnitNames.add(srvcUnit.getName());
    }

    public List<String> getServiceUnitNames() {
        return mServiceUnitNames;
    }

    public void setEntry(WorkflowMapEntry entry) {
        mEntry = entry;
    }

    public WorkflowMapEntry getEntry() {
        return mEntry;
    }

}
