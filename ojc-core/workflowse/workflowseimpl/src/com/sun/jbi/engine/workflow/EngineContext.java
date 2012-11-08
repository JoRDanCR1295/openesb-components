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
 * @(#)EngineContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.workflow;

import java.util.Properties;
import java.util.Set;

import javax.naming.InitialContext;

import com.sun.jbi.common.qos.config.ComponentConfig;
import com.sun.jbi.common.qos.config.Property;
import com.sun.jbi.engine.workflow.process.LDAPConfig;

/**
 * Encapsulates context object used by workflow engine
 * @author Sun Microsystems
 *
 */
public class EngineContext {
    
    private InitialContext mInitialContext;
    
    private WorkflowMapEntryTable mWorkflowMapEntryTable;
    
    private Properties mProps = null;
    
    private LDAPConfig mLDAPConfig = null;

    public InitialContext getInitialContext() {
        return mInitialContext;
    }

    public void setInitialContext(InitialContext initialContext) {
        mInitialContext = initialContext;
    }

    public WorkflowMapEntryTable getWorkflowMapEntryTable() {
        return mWorkflowMapEntryTable;
    }

    public void setWorkflowMapEntryTable(WorkflowMapEntryTable workflowMapEntryTable) {
        mWorkflowMapEntryTable = workflowMapEntryTable;
    }

    public void setConfig (Properties configProp) {
        mProps = configProp;
    }
    public void setConfig(ComponentConfig configProp) {
        // TODO Auto-generated method stub
        Set<Property> propertySet = configProp.propertySet();
        mProps = new Properties ();
        for (Property prop : propertySet) {
            if (prop.getValue() != null) {
                mProps.put(prop.getName(), prop.getValue());
            }
        }       
        if (Boolean.valueOf(mProps.getProperty(EnginePropertyConstants.USE_LDAP))) {
            mLDAPConfig = new LDAPConfig (configProp);
        }
    }
    
    public Properties getConfig () {
        return mProps;
    }
    
    public LDAPConfig getLDAPConfig () {
        return mLDAPConfig;
    }
    

}
