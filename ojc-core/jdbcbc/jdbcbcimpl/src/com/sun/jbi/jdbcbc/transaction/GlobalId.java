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
 * @(#)GlobalId.java 
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
import java.io.IOException;
import javax.transaction.xa.Xid;

/**
 *  This object encapsulates the global transaction ID of a transaction.
 *  It is similar to an Xid, but holds only the GlobalId part.
 *  This implementation is immutable and always serializable at runtime.
 *
 *  @see XidImpl
 *  @author <a href="mailto:Narayana.Rallabandi@Sun.Com">Narayana Rallabandi</a>
 *  @version 
 */
public class GlobalId implements java.io.Externalizable{
    
   static final long serialVersionUID = 234567891L;
   private int formatId;
   //Global Transaction Id
   private byte[] globalId;
   // hash code for the global id
   private int hash;

   public GlobalId(){
   }
   
   protected GlobalId(int formatId, byte[] globalId, int hash){
      this.formatId = formatId;
      this.globalId = globalId;
      this.hash = hash;
   }

   protected GlobalId(int formatId, byte[] globalId){
      this.formatId = formatId;
      this.globalId = globalId;
      hash = computeHash();
   }

   private GlobalId(Xid xid){
      formatId = xid.getFormatId();
      globalId = xid.getGlobalTransactionId();
      if (xid instanceof XidImpl) {
         hash = xid.hashCode();
      }else{
         hash = computeHash();
      }
   }

   private GlobalId(int formatId, int bqual_length, byte[] tid){
      this.formatId = formatId;
      if (bqual_length == 0)
         globalId = tid;
      else {
         int len = tid.length - bqual_length;
         globalId = new byte[len];
         System.arraycopy(tid, 0, globalId, 0, len);
      }
      hash = computeHash();
   }
   
   // @Override
   public boolean equals(Object obj){
      if (obj instanceof GlobalId) {
         GlobalId other = (GlobalId)obj;
         if (formatId != other.formatId)
            return false;
         if (globalId == other.globalId)
            return true;
         if (globalId.length != other.globalId.length)
            return false;

         int len = globalId.length;
         for (int i = 0; i < len; ++i)
            if (globalId[i] != other.globalId[i])
               return false;
         return true;
      }
      return false;
   }

    //@Override
   public int hashCode(){
      return hash;
   }
   
    //@Override
   public String toString() {
      return getClass().getName() + "[formatId=" + formatId
            + ", globalId=" + new String(globalId).trim()
            + ", hash=" + hash + "]";
   }

    //@Override
   public void writeExternal(java.io.ObjectOutput out) throws IOException {
      out.writeInt(formatId);
      out.writeObject(globalId);
   }
   
   // @Override
   public void readExternal(java.io.ObjectInput in) throws IOException, ClassNotFoundException{
      formatId = in.readInt();
      globalId = (byte[])in.readObject();
      hash = computeHash();
   }

   private int computeHash(){
         int len = globalId.length;
         int hashval = 0;
         // TODO: use a better hash function
         for (int i = 0; i < len; ++i)
            hashval = 3 * globalId[i] + hashval;
         hashval += formatId;
         return hashval;
   }

}
