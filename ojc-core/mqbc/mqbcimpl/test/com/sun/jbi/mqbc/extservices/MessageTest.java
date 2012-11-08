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
 * @(#)MessageTest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;

import junit.framework.*;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InvalidClassException;
import java.io.InvalidObjectException;
import java.io.OptionalDataException;
import java.io.StreamCorruptedException;
import java.text.MessageFormat;
import com.ibm.mq.MQMD;
import com.ibm.mq.MQMessage;
import java.util.logging.Logger;
import java.util.logging.Level;
import com.sun.jbi.internationalization.Messages;
import com.ibm.mq.MQQueueManager;

/**
 *
 * @author rchen
 */
public class MessageTest extends TestCase {
    ExtServiceMQMessage mInstance ;
    
    public MessageTest(String testName) {
        super(testName);
    }
    
    protected void setUp() throws Exception {
        mInstance = new ExtServiceMQMessage();
        
    }
    
    protected void tearDown() throws Exception {
    }
    
    public static Test suite() {
        TestSuite suite = new TestSuite(MessageTest.class);
        
        return suite;
    }
    
    /**
     * Test of getMsgBody method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testGetMsgBody() {
        System.out.println("getMsgBody");
        
        
        ExtServiceMQMessage.MsgBody result = mInstance.getMsgBody();
        assertNotNull(result);
        
        
    }
    
    /**
     * Test of getMsgHeader method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testGetMsgHeader() {
        System.out.println("getMsgHeader");
        
        
        
        ExtServiceMQMessage.MsgHeader expResult = null;
        ExtServiceMQMessage.MsgHeader result = mInstance.getMsgHeader();
        assertNotNull(result);
        
    }
    
    
    /**
     * Test of getSyncpointController method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testGetSyncpointController() {
        System.out.println("getSyncpointController");
        
        
        
        SyncpointControl expResult = null;
        SyncpointControl result = mInstance.getSyncpointController();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of getTotalMessageLength method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testGetTotalMessageLength() {
        System.out.println("getTotalMessageLength");
        
        
        
        int expResult = 0;
        int result = mInstance.getTotalMessageLength();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of getMessageLength method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testGetMessageLength() throws Exception {
        System.out.println("getMessageLength");
        
        
        
        int expResult = 0;
        int result = mInstance.getMessageLength();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of getDataLength method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testGetDataLength() throws Exception {
        System.out.println("getDataLength");
        
        
        
        int expResult = 0;
        int result = mInstance.getDataLength();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of seek method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testSeek() throws Exception {
        System.out.println("seek");
        
        int pos = 0;
        
        
        mInstance.seek(pos);
        
        
    }
    
    /**
     * Test of setDataOffset method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testSetDataOffset() throws Exception {
        System.out.println("setDataOffset");
        
        int offset = 0;
        
        
        mInstance.setDataOffset(offset);
        
        
    }
    
    /**
     * Test of getDataOffset method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testGetDataOffset() throws Exception {
        System.out.println("getDataOffset");
        
        
        
        int expResult = 0;
        int result = mInstance.getDataOffset();
        assertEquals(expResult, result);
        
        
    }
    
    /**
     * Test of clearMessage method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testClearMessage() throws Exception {
        System.out.println("clearMessage");
        
        
        
        mInstance.clearMessage();
        
        
    }
    
    /**
     * Test of resizeBuffer method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testResizeBuffer() throws Exception {
        System.out.println("resizeBuffer");
        
        int size = 0;
        
        
        mInstance.resizeBuffer(size);
        
        
    }
    
    /**
     * Test of readBoolean method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadBoolean() throws Exception {
        System.out.println("readBoolean");
        
        
        
        boolean expResult = true;
        try{
            boolean result = mInstance.readBoolean();
        } catch (java.io.EOFException ex) {
            
        }
        
        
        
        
    }
    
    /**
     * Test of readByte method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadByte() throws Exception {
        System.out.println("readByte");
        
        
        
        byte expResult = 0;
        try{
            byte result = mInstance.readByte();
        } catch (java.io.EOFException ex) {
            
        }
        
        
    }
    
    /**
     * Test of readChar method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadChar() throws Exception {
        System.out.println("readChar");
        
        
        
        char expResult = ' ';
        try{
            char result = mInstance.readChar();
        } catch (java.io.EOFException ex) {
            
        }
        
        
    }
    
    /**
     * Test of readDouble method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadDouble() throws Exception {
        System.out.println("readDouble");
        
        
        
        double expResult = 0.0;
        try{
            double result = mInstance.readDouble();
        } catch (java.io.EOFException ex) {
            
        }
        
        
    }
    
    /**
     * Test of readFloat method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadFloat() throws Exception {
        System.out.println("readFloat");
        
        
        
        float expResult = 0.0F;
        try{
            float result = mInstance.readFloat();
        } catch (java.io.EOFException ex) {
            
        }
        
        
    }
    
    /**
     * Test of readFully method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadFully() throws Exception {
        System.out.println("readFully");
        
        byte[] buffer = new byte[10];
        
        try{
            mInstance.readFully(buffer);
        } catch (java.io.EOFException ex) {
            
        }
        
        
    }
    
    /**
     * Test of readInt method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadInt() throws Exception {
        System.out.println("readInt");
        
        
        
        int expResult = 0;
        try{
            int result = mInstance.readInt();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readInt4 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadInt4() throws Exception {
        System.out.println("readInt4");
        
        
        
        int expResult = 0;
        try {
            int result = mInstance.readInt4();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readLine method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadLine() throws Exception {
        System.out.println("readLine");
        
        
        
        String expResult = "";
        try {
            String result = mInstance.readLine();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readLong method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadLong() throws Exception {
        System.out.println("readLong");
        
        
        
        long expResult = 0L;
        try {
            long result = mInstance.readLong();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readInt8 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadInt8() throws Exception {
        System.out.println("readInt8");
        
        
        
        long expResult = 0L;
        try {
            long result = mInstance.readInt8();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readObject method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadObject() throws Exception {
        System.out.println("readObject");
        
        
        
        Object expResult = null;
        try {
            Object result = mInstance.readObject();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readShort method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadShort() throws Exception {
        System.out.println("readShort");
        
        
        
        short expResult = 0;
        try {
            short result = mInstance.readShort();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readInt2 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadInt2() throws Exception {
        System.out.println("readInt2");
        
        
        
        short expResult = 0;
        try {
            short result = mInstance.readInt2();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readUTF method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadUTF() throws Exception {
        System.out.println("readUTF");
        
        
        
        String expResult = "";
        try {
            String result = mInstance.readUTF();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readUnsignedByte method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadUnsignedByte() throws Exception {
        System.out.println("readUnsignedByte");
        
        
        
        int expResult = 0;
        try {
            int result = mInstance.readUnsignedByte();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readUnsignedShort method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadUnsignedShort() throws Exception {
        System.out.println("readUnsignedShort");
        
        
        
        int expResult = 0;
        try {
            int result = mInstance.readUnsignedShort();
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readUInt2 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadUInt2() throws Exception {
        System.out.println("readUInt2");
        
        
        try{
            int expResult = 0;
            int result = mInstance.readUInt2();
            assertEquals(expResult, result);
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readString method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadString() throws Exception {
        System.out.println("readString");
        
        int length = 0;
        
        try {
            String expResult = "";
            String result = mInstance.readString(length);
            assertEquals(expResult, result);
        } catch (java.io.EOFException ex) {
            
        }
    }
    
    /**
     * Test of readDecimal2 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadDecimal2() throws Exception {
        System.out.println("readDecimal2");
        
        
        try {
        short expResult = 0;
        short result = mInstance.readDecimal2();
        assertEquals(expResult, result);
          } catch (java.io.EOFException ex)
        {
            
        }
    }
    
    /**
     * Test of readDecimal4 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadDecimal4() throws Exception {
        System.out.println("readDecimal4");
        
        try {
        
        int expResult = 0;
        int result = mInstance.readDecimal4();
        assertEquals(expResult, result);
        
          } catch (java.io.EOFException ex)
        {
            
        }
    }
    
    /**
     * Test of readDecimal8 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testReadDecimal8() throws Exception {
        System.out.println("readDecimal8");
        
        
        try {
        long expResult = 0L;
        long result = mInstance.readDecimal8();
        assertEquals(expResult, result);
        
          } catch (java.io.EOFException ex)
        {
            
        }
    }
    
   
    
    /**
     * Test of skipBytes method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testSkipBytes() throws Exception {
        System.out.println("skipBytes");
        
        int numberOfBytes = 0;
        
        
        int expResult = 0;
        int result = mInstance.skipBytes(numberOfBytes);
        assertEquals(expResult, result);
        
       
    }
    
    /**
     * Test of write method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWrite() throws Exception {
        System.out.println("write");
        
        int byteValue = 0;
        
        
        mInstance.write(byteValue);
        
        
    }
    
    /**
     * Test of writeBoolean method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteBoolean() throws Exception {
        System.out.println("writeBoolean");
        
        boolean boolValue = true;
        
        
        mInstance.writeBoolean(boolValue);
        
     
    }
    
    /**
     * Test of writeByte method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteByte() throws Exception {
        System.out.println("writeByte");
        
        int byteValue = 0;
        
        
        mInstance.writeByte(byteValue);
        
        
    }
    
    /**
     * Test of writeBytes method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteBytes() throws Exception {
        System.out.println("writeBytes");
        
        String stringValue = "";
        
        
        mInstance.writeBytes(stringValue);
        
     
    }
    
    /**
     * Test of writeChar method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteChar() throws Exception {
        System.out.println("writeChar");
        
        int unicodeChar = 0;
        
        
        mInstance.writeChar(unicodeChar);
        
       
    }
    
    /**
     * Test of writeChars method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteChars() throws Exception {
        System.out.println("writeChars");
        
        String str = "";
        
        
        mInstance.writeChars(str);
        
      
    }
    
    /**
     * Test of writeDouble method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteDouble() throws Exception {
        System.out.println("writeDouble");
        
        double doubleValue = 0.0;
        
        
        mInstance.writeDouble(doubleValue);
        
      
    }
    
    /**
     * Test of writeFloat method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteFloat() throws Exception {
        System.out.println("writeFloat");
        
        float floatValue = 0.0F;
        
        
        mInstance.writeFloat(floatValue);
        
      
    }
    
    /**
     * Test of writeInt method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteInt() throws Exception {
        System.out.println("writeInt");
        
        int intValue = 0;
        
        
        mInstance.writeInt(intValue);
        
       
    }
    
    /**
     * Test of writeInt4 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteInt4() throws Exception {
        System.out.println("writeInt4");
        
        int intValue = 0;
        
        
        mInstance.writeInt4(intValue);
        
       
    }
    
    /**
     * Test of writeLong method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteLong() throws Exception {
        System.out.println("writeLong");
        
        long longValue = 0L;
        
        
        mInstance.writeLong(longValue);
        
      
    }
    
    /**
     * Test of writeInt8 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteInt8() throws Exception {
        System.out.println("writeInt8");
        
        long longValue = 0L;
        
        
        mInstance.writeInt8(longValue);
        
       
    }
    
    /**
     * Test of writeObject method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteObject() throws Exception {
        System.out.println("writeObject");
        
        Object objValue = new Boolean("true");
        
        
        mInstance.writeObject(objValue);
        
       
    }
    
    /**
     * Test of writeShort method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteShort() throws Exception {
        System.out.println("writeShort");
        
        int shortValue = 0;
        
        
        mInstance.writeShort(shortValue);
        
       
    }
    
    /**
     * Test of writeInt2 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteInt2() throws Exception {
        System.out.println("writeInt2");
        
        int shortValue = 0;
        
        
        mInstance.writeInt2(shortValue);
        
       
    }
    
    /**
     * Test of writeDecimal2 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteDecimal2() throws Exception {
        System.out.println("writeDecimal2");
        
        short decimal2Value = 0;
        
        
        mInstance.writeDecimal2(decimal2Value);
        
       
    }
    
    /**
     * Test of writeDecimal4 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteDecimal4() throws Exception {
        System.out.println("writeDecimal4");
        
        int decimal4Value = 0;
        
        
        mInstance.writeDecimal4(decimal4Value);
        
       
    }
    
    /**
     * Test of writeDecimal8 method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteDecimal8() throws Exception {
        System.out.println("writeDecimal8");
        
        long decimal8Value = 0L;
        
        
        mInstance.writeDecimal8(decimal8Value);
        
     
    }
    
    /**
     * Test of writeUTF method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteUTF() throws Exception {
        System.out.println("writeUTF");
        
        String utfString = "";
        
        
        mInstance.writeUTF(utfString);
        
      
    }
    
    /**
     * Test of writeString method, of class com.sun.jbi.mqbc.extservices.Message.
     */
    public void testWriteString() throws Exception {
        System.out.println("writeString");
        
        String stringValue = "";
        
        
        mInstance.writeString(stringValue);
        
       
    }
    
}
