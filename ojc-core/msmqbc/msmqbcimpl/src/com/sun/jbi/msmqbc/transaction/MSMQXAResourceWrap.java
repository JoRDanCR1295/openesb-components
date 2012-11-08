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
 * @(#)MSMQXAResourceWrap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.msmqbc.transaction;

import javax.transaction.xa.Xid;

/**
 * Resource Wrap to interact with microsoft message server
 * 
 * @author Sun Microsystems
 */

// using JVM logger to have the msmqjni.jar have miminum dependency
// not even mport com.stc.connector.logging.Logger.
// because the class is going to be loaded by is class loader v.s.
// connector class loader.
// tomato is using jdk logger will the log message will appear
// webspher and weblogic is log4j logger the jvm logger may be null
// so the mseeage in jni layer may not appear
import java.util.logging.Logger;

import com.sun.jbi.internationalization.Messages;

public class MSMQXAResourceWrap {

    private Logger mLogger = Logger.getLogger(getClass().getName());

    int resourceHandle = 0;

    public native int open(String xa_resource_name);

    public native int close(String xa_resource_name);

    public native int commit(byte[] gtrid, byte[] bqual, int formatid, boolean flag);

    public native int end(byte[] gtrid, byte[] bqual, int formatid, int flags);

    public native int forget(byte[] gtrid, byte[] bqual, int formatid);

    public native int prepare(byte[] gtrid, byte[] bqual, int formatid);

    public native int recover(byte[] gtrids, byte[] bquals, long count, int flags);

    public native int rollback(byte[] gtrid, byte[] bqual, int formatid);

    public native int start(byte[] gtrid, byte[] bqual, int formatid, int flags);

    public int getInstance() {
        mLogger.info("MSMQXAResourceWrap() - called");
        return resourceHandle;
    }

    private void callback() throws NullPointerException {
        throw new NullPointerException("thrown in CatchThrow.callback");
    }

    public void MSMQXAResourceWrap() {
        mLogger.info("MSMQXAResourceWrap_getInstance()");
        resourceHandle = 0;
    }

    static {
        //System.loadLibrary("MSMQXAResourceJni");
    }
}
