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
 * @(#)LocalId.java 
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
/**
 *  LocalId symbloizes the transaction id from the Application Server
 *  This class implements the Externizable interface for making the object 
 *  serialiable during runtime.
 *
 *  @author <a href="Narayana.Rallabandi@Sun.Com">Narayana Rallabandi</a>
 *  @version 
 */
public class LocalId implements java.io.Externalizable{
    
   static final long serialVersionUID = 123456789L;
   private long localIdvalue; // 

   public LocalId(){
   }
   
   protected LocalId(long localIdvalue){
      this.localIdvalue = localIdvalue;
   }

   protected LocalId(XidImpl xid){
      this(xid.getLocalIdValue());
   }
   
   private long getValue(){
      return localIdvalue;
   }

    //@Override
   public boolean equals(Object obj){
      return (obj instanceof LocalId) ? 
          (localIdvalue == ((LocalId)obj).localIdvalue): false;
   }

    //@Override
   public int hashCode(){
      return (int)localIdvalue;
   }
   
    //@Override
   public void writeExternal(java.io.ObjectOutput out)
      throws java.io.IOException
   {
      out.writeLong(localIdvalue);
   }
   
    //@Override
   public void readExternal(java.io.ObjectInput in)
                throws java.io.IOException, ClassNotFoundException {
      localIdvalue = in.readLong();
   }
   
   // this morphs the long under consideration 
   // which fecilitates to Obtain the global transaction identifier 
   // part of XID as an array of bytes
   private static void toByteArray(long localId, byte[] destination, int dstBegin){
      destination[dstBegin + 0] = (byte)(0xff & (localId >>> 56));
      destination[dstBegin + 1] = (byte)(0xff & (localId >>> 48));
      destination[dstBegin + 2] = (byte)(0xff & (localId >>> 40));
      destination[dstBegin + 3] = (byte)(0xff & (localId >>> 32));
      destination[dstBegin + 4] = (byte)(0xff & (localId >>> 24));
      destination[dstBegin + 5] = (byte)(0xff & (localId >>> 16));
      destination[dstBegin + 6] = (byte)(0xff & (localId >>>  8));
      destination[dstBegin + 7] = (byte)(0xff & (localId >>>  0));
   }

   // this morphs the byrearray source under consideration 
   // which fecilitates to Obtain the global transaction identifier 
   // as long which is part of XID.
   private static long fromByteArray(byte[] source, int srcBegin) {
      return ((long)(source[srcBegin + 0] & 0xff) << 56)
	 | ((long)(source[srcBegin + 1] & 0xff) << 48)
	 | ((long)(source[srcBegin + 2] & 0xff) << 40)
	 | ((long)(source[srcBegin + 3] & 0xff) << 32)
	 | ((long)(source[srcBegin + 4] & 0xff) << 24)
	 | ((long)(source[srcBegin + 5] & 0xff) << 16)
	 | ((long)(source[srcBegin + 6] & 0xff) << 8)
	 | ((long)(source[srcBegin + 7] & 0xff));
   }
}
