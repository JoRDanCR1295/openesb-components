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
 * @(#)XAResourceImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.glassfish.openesb.databasebc.transaction;


import java.sql.SQLException;
import javax.sql.XAConnection;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import javax.transaction.xa.Xid;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.sql.XADataSource;
/**
 *
 * @author Venkat P
 */
public class XAResourceImpl implements XAResource{
    
    protected final Object stateLock = new Object();
    protected AtomicBoolean inLocalTransaction = new AtomicBoolean(false);
    protected boolean jdbcAutoCommit = true;
    protected boolean underlyingAutoCommit = true;
    protected boolean jdbcReadOnly;
    protected boolean underlyingReadOnly;
    protected int jdbcTransactionIsolation;
    protected boolean destroyed = false;
    protected final XAConnection xaConnection;
    protected final XAResource xaResource;
    protected Xid currentXid;
    /** Creates a new instance of XAResourceImpl */
    public XAResourceImpl(){
        xaConnection = null;
        xaResource = null;
    }
    
   private XAResourceImpl(XADataSource xad) throws SQLException{
       xaConnection = xad.getXAConnection();
       xaResource = xaConnection.getXAResource();
   }

   private XAResourceImpl(XAConnection xaConnection) throws SQLException{
      this.xaConnection = xaConnection;
      xaConnection.addConnectionEventListener(new javax.sql.ConnectionEventListener(){
            //@Override
         public void connectionClosed(javax.sql.ConnectionEvent ce){
            //only we can do this, ignore
         }
            //@Override
         public void connectionErrorOccurred(javax.sql.ConnectionEvent ce){
             
         }
      });
      this.xaResource = xaConnection.getXAResource();
   }

   protected XAResource getXAResource() throws Exception{
      return this;
   }

   protected void destroy() throws Exception{
      try{
         //xaResource.end();
      }
      finally{
         try{
            xaConnection.close();
         }catch (SQLException e){
            throw e;
         }
      }
   }

    //@Override
   public void start(Xid xid, int flags) throws XAException{
      xaResource.start(xid, flags);
      synchronized (stateLock){
         currentXid = xid;
      }
   }

    //@Override
   public void end(Xid xid, int flags) throws XAException{
      xaResource.end(xid, flags);
      //we want to allow ending transactions that are not the current
      //one. When one does this, inManagedTransaction is still true.
      synchronized (stateLock){
         if (currentXid != null && currentXid.equals(xid)){
            currentXid = null;
         }
      }
   }

    //@Override
   public int prepare(Xid xid) throws XAException
   {
      return xaResource.prepare(xid);
   }

    //@Override
   public void commit(Xid xid, boolean onePhase) throws XAException{
      xaResource.commit(xid, onePhase);
   }

    //@Override
   public void rollback(Xid xid) throws XAException{
      xaResource.rollback(xid);
   }

    //@Override
   public void forget(Xid xid) throws XAException{
      xaResource.forget(xid);
   }

    //@Override
   public Xid[] recover(int flag) throws XAException{
      return xaResource.recover(flag);
   }

    //@Override
   public boolean isSameRM(XAResource other) throws XAException{
       return true;
   }

    //@Override
   public int getTransactionTimeout() throws XAException{
      return xaResource.getTransactionTimeout();
   }

    //@Override
   public boolean setTransactionTimeout(int seconds) throws XAException{
      return xaResource.setTransactionTimeout(seconds);
   }
   
   protected void checkState() throws SQLException{
      synchronized (stateLock){
         // Check readonly
         if (jdbcReadOnly != underlyingReadOnly){
            //xaConnection.setReadOnly(jdbcReadOnly);
            underlyingReadOnly = jdbcReadOnly;
         }
      }
   }
}
