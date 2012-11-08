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
 * @(#)XidFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.transaction;


import java.net.InetAddress;
import javax.transaction.xa.Xid;
import java.net.UnknownHostException;
/**
 * Factory class for generation of Xids
 * @author Venkat P
 */
public class XidFactory {
    
    private String globalId;
    private long globalIdAsNumber = 0;
    private boolean isPadApplied = false;
    private byte[] branchId = new byte[1];
    private byte[] globalIdToBytes;
    
    private static XidFactory instance = null;
    
    public static XidFactory getXidFactoryInstance(){
            if (instance == null) {
                instance = new XidFactory();
            }
            return instance;
    }
    public XidFactory() {
        try {
            globalId = InetAddress.getLocalHost().getHostName();
            if (globalId.length() > Xid.MAXGTRIDSIZE - 15)
                globalId = globalId.substring(0, Xid.MAXGTRIDSIZE - 15);
            globalId = globalId + "/";
        } catch (UnknownHostException e) {
            globalId = "localhost/";
        }
        globalIdToBytes = globalId.getBytes();
    }
   
    public String getBaseGlobalId() {
        return globalId;
    }
    
    public void setBaseGlobalId(final String baseGlobalId) {
        this.globalId = baseGlobalId;
        globalIdToBytes = baseGlobalId.getBytes();
    }
 
    public synchronized long getGlobalIdNumber() {
        return globalIdAsNumber;
    }
    
    public synchronized void setGlobalIdNumber(final long globalIdNumber) {
        this.globalIdAsNumber = globalIdNumber;
    }
 
    public boolean isPadApplied() {
        return isPadApplied;
    }
 
    public void setPadApplied(boolean pad) {
        this.isPadApplied = pad;
        if (pad)
            branchId = new byte[Xid.MAXBQUALSIZE];
        else
            branchId = new byte[1]; 
    }
    
    
    public XidImpl newXid() {
        long localId = getNextId();
        String id = Long.toString(localId);
        int len = isPadApplied?Xid.MAXGTRIDSIZE:id.length()+globalIdToBytes.length;
        byte[] globalId = new byte[len];
        System.arraycopy(globalIdToBytes, 0, globalId, 0, globalIdToBytes.length);
        id.getBytes(0, id.length(), globalId, globalIdToBytes.length);
        return new XidImpl(globalId, branchId, (int)localId, localId);
    }
      
    public XidImpl newBranch(XidImpl xid, long branchIdNum) {
        String id = Long.toString(branchIdNum);
        int len = isPadApplied?Xid.MAXBQUALSIZE:id.length();
        byte[] branchId = new byte[len];
        id.getBytes(0, id.length(), branchId, 0);
        return new XidImpl(xid, branchId);
    }
 
    public long extractLocalIdFrom(byte[] globalId) {
        int i, start;
        int len = globalId.length;
        
        for (i = 0; globalId[i++] != (byte)'/'; )
            ;
        start = i;
        while (i < len && globalId[i] != 0)
            i++;
        String globalIdNumber = new String(globalId, 0, start, i - start);
        return Long.parseLong(globalIdNumber);
    }
    
    public String toString(Xid xid) {
        if (xid instanceof XidImpl)
            return XidImpl.toString(xid);
        else
            return xid.toString();
    }
    
    public synchronized long getNextId() {
        return ++globalIdAsNumber;
    }
    
}
