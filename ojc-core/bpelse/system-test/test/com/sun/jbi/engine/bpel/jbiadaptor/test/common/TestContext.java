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
 * @(#)TestContext.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.jbiadaptor.test.common;

import java.util.Properties;

import javax.naming.InitialContext;
import javax.naming.NamingException;

import com.sun.jbi.engine.bpel.core.bpel.connection.ConnectionProperties;


/**
 *
 * @author Sun Microsystems
 */
public class TestContext extends InitialContext {
    
    private DummyTxManagerAndDataSource mDummyTMAndDS;
    private DummyNonXATxManagerAndDataSource mDummyNonXATMAndDS;
    private Properties mProps;
    
    public static String END_OF_INSTANCE_LISTENER = "END_OF_INSTANCE_LISTENER"; 
    private Object mEND_OF_INSTANCE_LISTENER_Obj;
    
    public TestContext(DummyNonXATxManagerAndDataSource dummyNonXATMAndDS, 
            DummyTxManagerAndDataSource dummyTMAndDS, Properties props)
        throws NamingException {
        if (dummyNonXATMAndDS == null || dummyTMAndDS == null || props == null) {
            throw new RuntimeException("Testing Environment is not set right");
        }
        mDummyTMAndDS = dummyTMAndDS;
        mDummyNonXATMAndDS = dummyNonXATMAndDS;
        mProps = props;
    }

    public Object lookup(String jndiName) throws NamingException {
        if (jndiName.equals(END_OF_INSTANCE_LISTENER)) {
            return mEND_OF_INSTANCE_LISTENER_Obj;
        } else if (jndiName.equals(mProps.get(ConnectionProperties.DatabaseNonXAJNDIName))) {
            return this.mDummyNonXATMAndDS;
        } else if (jndiName.equals(mProps.get(ConnectionProperties.DatabaseXAJNDIName))) {
            return this.mDummyTMAndDS;
        }
        throw new RuntimeException("Testing Environment is not set right");
    }

    /** @see javax.naming.InitialContext#bind(java.lang.String, java.lang.Object)
     */
    public void bind(String arg0, Object arg1) throws NamingException {
        if (arg0.equals(END_OF_INSTANCE_LISTENER)) {
            mEND_OF_INSTANCE_LISTENER_Obj = arg1;
        }
    }
}
