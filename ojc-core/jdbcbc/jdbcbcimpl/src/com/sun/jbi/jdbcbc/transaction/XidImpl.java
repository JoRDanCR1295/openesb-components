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
 * @(#)XidImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.jdbcbc.transaction;

/**
 *
 * @author Venkat P
 */
import javax.transaction.xa.Xid;

/**
 *  This object encapsulates the ID of a transaction.
 *  This implementation is immutable and always serializable at runtime.
 *
 *  @author <a href="Narayana.Rallabandi@Sun.Com">Narayana Rallabandi</a>
 *  @version 
 */
public class XidImpl implements Xid, java.io.Serializable {
  
    static final long serialVersionUID = -123456789L;
   public static final int FORMAT_ID = 0x0101;
   private static boolean trulyGlobalIdsEnabled = false;
   
   // represents the format id for this xid
   private final int formatId;
   // global id immutable once set
   private final byte[] globalId; 
   // branch qualifier
   private final byte[] branchId;
   // hash for the xid
   private final int hash;
   // local id for this xid
   private final long localId;
   //global id 
   private final GlobalId trulyGlobalId;

   public static void setTrulyGlobalIdsEnabled(boolean newValue) {
      trulyGlobalIdsEnabled = newValue;
   }

   public static boolean getTrulyGlobalIdsEnabled() {
      return trulyGlobalIdsEnabled;
   }

   public static String toString(Xid id){
      if (id == null)
         return "[NULL Xid]";

      String s = id.getClass().getName();
      s = s.substring(s.lastIndexOf('.') + 1);
      s = s + "[FormatId=" + id.getFormatId()
            + ", GlobalId=" + new String(id.getGlobalTransactionId()).trim()
            + ", BranchQual=" + new String(id.getBranchQualifier()).trim()
            + ((id instanceof XidImpl) ? ", localId=" + ((XidImpl)id).localId 
                                       : "") 
            + "]";

      return s;
   }

   public XidImpl(int formatId, 
                  byte[] globalId, byte[] branchId, int hash, long localId){
      this.formatId = formatId;
      this.globalId = globalId;
      this.branchId = branchId;
      this.hash = hash;
      this.localId = localId;
      this.trulyGlobalId = (trulyGlobalIdsEnabled) 
                           ? new GlobalId(formatId, globalId) 
                           : null;
   }

   /**
    *  Create a new instance with JBOSS_FORMAT_ID.
    */
   public XidImpl(byte[] globalId, byte[] branchId, int hash, long localId){
      this.formatId = FORMAT_ID;
      this.globalId = globalId;
      this.branchId = branchId;
      this.hash = hash;
      this.localId = localId;
      this.trulyGlobalId = (trulyGlobalIdsEnabled) 
                           ? new GlobalId(FORMAT_ID, globalId, hash)
                           : null;
   }
  
   public XidImpl(final XidImpl xidImpl, final byte[] branchId){
      this.formatId = xidImpl.formatId;
      this.globalId = xidImpl.globalId; 
      this.branchId = branchId;
      this.hash = xidImpl.hash;
      this.localId = xidImpl.localId;
      this.trulyGlobalId = (trulyGlobalIdsEnabled) 
                           ? xidImpl.trulyGlobalId 
                           : null;
   }

  // retrun the Global TxId
    //@Override
   public byte[] getGlobalTransactionId(){
      return (byte[])globalId.clone();
   }
   
   // return branch qualifier
    //@Override
   public byte[] getBranchQualifier(){
      if (branchId.length == 0)
         return branchId; 
      else
         return (byte[])branchId.clone();
   }

    //@Override
   public int getFormatId() {
      return formatId;
   }

   /**
    *  checks for same format id, the same global transaction id 
    *  and the same transaction branch qualifier.
    */
    //@Override
   public boolean equals(Object obj)
   {
      if(obj==this) 
         return true;
      if (obj instanceof XidImpl) {
         XidImpl other = (XidImpl)obj;

         if (formatId != other.formatId ||
             globalId.length != other.globalId.length ||
             branchId.length != other.branchId.length)
            return false;

         for (int i = 0; i < globalId.length; ++i)
            if (globalId[i] != other.globalId[i])
               return false;

         for (int i = 0; i < branchId.length; ++i)
            if (branchId[i] != other.branchId[i])
               return false;

         return true;
      }
      return false;
   }

    @Override
   public int hashCode(){
      return hash;
   }

    @Override
   public String toString(){
      return toString(this);
   }

    public long getLocalIdValue() {
      return localId;
   }
  
   public LocalId getLocalId() {
      return new LocalId(localId);
   }
 
   public GlobalId getTrulyGlobalId() {
      return trulyGlobalId;
   }

 
   public boolean sameTransaction(XidImpl other){
      if(other == this) 
         return true;
      if (formatId != other.formatId ||
          globalId.length != other.globalId.length)
         return false;

      for (int i = 0; i < globalId.length; ++i)
         if (globalId[i] != other.globalId[i])
            return false;
      return true;
   }

   //returns a reference to the global id byte array
   public byte[] getInternalGlobalTransactionId(){
      return globalId;
   }
}
