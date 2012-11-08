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
 */

/*
 * @(#)$Id: XAResourceWrapper.java,v 1.1 2008/12/18 02:54:53 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;

/**
 * A XAResource wrapper that overrides {@link javax.transaction.xa.XAResource#isSameRM}
 * to return true instead of false.  This is a workaround for a bug in the
 * WebSphere MQ XAResource implementation where enlisting multiple WSMQ
 * transactions in the same transaction causes an indefinite blocking. See
 * https://jira.jboss.org/jira/browse/JBAS-3183:
 * <pre>
 * Transaction tx = transactionManager.begin();
 * Connection c1 = wsmqConnectionFactory.createXAConnection();
 * XAResource xar1 = getXAResource(c1); tx.start(xar1, TM_NOFLAGS);
 * Connection c2 = wsmqConnectionFactory.createXAConnection();
 * XAResource xar2 = getXAResource(c2); tx.start(xar2, TM_JOIN); // HANGS HERE!
 * </pre>
 * By making it appear that each XAResource represents a different resource
 * manager, the bug is bypassed by starting a new transaction branch for each
 * XAResource instance. This is likely to incur a performance penalty, so its
 * usage will be controlled by configuration flag(s).
 *
 * @author Noel.Ang@sun.com
 */
public final class XAResourceWrapper
        implements XAResource {
    public XAResourceWrapper(XAResource xares) {
        this.xares = xares;
    }

    public void commit(Xid xid, boolean b)
            throws
            XAException {
        xares.commit(xid, b);
    }

    public void end(Xid xid, int i)
            throws
            XAException {
        xares.end(xid, i);
    }

    public void forget(Xid xid)
            throws
            XAException {
        xares.forget(xid);
    }

    public int getTransactionTimeout()
            throws
            XAException {
        return xares.getTransactionTimeout();
    }

    public boolean isSameRM(XAResource xaResource)
            throws
            XAException {
        // Allow exceptions that happen, to throw.
        xares.isSameRM(xaResource);
        // Intercept decisions and override.
        return false;
    }

    public int prepare(Xid xid)
            throws
            XAException {
        return xares.prepare(xid);
    }

    public Xid[] recover(int i)
            throws
            XAException {
        return xares.recover(i);
    }

    public void rollback(Xid xid)
            throws
            XAException {
        xares.rollback(xid);
    }

    public boolean setTransactionTimeout(int i)
            throws
            XAException {
        return xares.setTransactionTimeout(i);
    }

    public void start(Xid xid, int i)
            throws
            XAException {
        xares.start(xid, i);
    }

    @Override
    public int hashCode() {
        return xares.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }
        if (obj instanceof XAResourceWrapper) {
            return ((XAResourceWrapper) obj).xares.equals(xares);
        }
        return false;
    }

    private final XAResource xares;
}
