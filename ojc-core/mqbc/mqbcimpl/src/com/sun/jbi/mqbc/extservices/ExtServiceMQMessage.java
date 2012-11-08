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
 * @(#)ExtServiceMQMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extservices;


import java.io.EOFException;
import java.io.IOException;
import java.io.InvalidClassException;
import java.io.OptionalDataException;
import java.io.StreamCorruptedException;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.ibm.mq.MQMessage;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.mqbc.I18n;
import com.sun.jbi.mqbc.extensions.MessageDescriptors;
import com.sun.jbi.mqbc.util.DateTimeFormatException;
import com.sun.jbi.mqbc.util.XmlUtil;

/**
 * Represents a WebSphere MQ message, both its descriptor and data. This is the
 * class exposed as part of the eWay's OTD(s).  It subclasses {@link
 * com.ibm.mq.MQMessage} to add logging and to expose MQMessage public variables
 * in the Java Collaboration Editor by giving them accessors and mutators (the
 * Editor does not allow direct manipulation of fields, even if public).
 * 
 */
public class ExtServiceMQMessage extends MQMessage {

    public ExtServiceMQMessage() {
        super();

        m_body = new MsgBody(this);
        m_hdr = new MsgHeader(this);
      
        m_syncpointController = null;

        m_logging.log(Level.FINER, mMessages.getString(
                "DEBUG_NEW_OBJECT",
                ExtServiceMQMessage.class.getName()));
    }

    /**
     * Obtain a reference to the MsgBody object representing the Message's
     * content.
     * 
     * 
     * @return MsgBody object for the Message.
     * @see Message.MsgBody
     */
    public ExtServiceMQMessage.MsgBody getMsgBody() {
        return m_body;
    }

    /**
     * Obtain a reference to the MsgHeader object representing the message's
     * descriptor.
     * 
     * 
     * @return MsgHeader object for the Messge.
     * @see Message.MsgHeader
     */
    public ExtServiceMQMessage.MsgHeader getMsgHeader() {
        return m_hdr;
    }

    

    
    /**
     * Associate a syncpoint controller to this message, for the purpose of
     * giving users access to a restricted representation of the controller at a
     * later time, in order to roll back or commit the message.
     */ 
    public void assignSyncpointController(SyncpointControl sc) {
        m_syncpointController = sc;
    }
    
    /**
     * Retrieve the syncpoint controller associated to this Message.  May be
     * <code>null</code>.
     */ 
    public SyncpointControl getSyncpointController() {
        return m_syncpointController;
    }
    
    /**
     * @see com.ibm.mq.MQMessage#getTotalMessageLength()
     */
    public int getTotalMessageLength() {
        final int val = super.getTotalMessageLength();
        m_logging.log(Level.FINER,
                      "ExtServiceMQMessage_DEBUG_MQMTD_TOTALMESSAGELENGTH",
                      String.valueOf(val));
        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#getMessageLength()
     */
    public int getMessageLength() throws IOException {
        int val;

        val = super.getMessageLength();

        m_logging.log(Level.FINER,
                      "ExtServiceMQMessage_DEBUG_MQMTD_MESSAGELENGTH",
                      String.valueOf(val));
        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#getDataLength()
     */
    public int getDataLength() throws IOException {
        int val;

        val = super.getDataLength();

        m_logging.log(Level.FINER,
                      "ExtServiceMQMessage_DEBUG_MQMTD_DATALENGTH",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#seek(int)
     */
    public void seek(int pos) throws EOFException {
        m_logging.log(Level.FINE, "ExtServiceMQMessage_DEBUG_MQMTD_SEEK", String.valueOf(pos));
        super.seek(pos);
    }

    /**
     * @see com.ibm.mq.MQMessage#setDataOffset(int)
     */
    public void setDataOffset(int offset) throws EOFException {
        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_DATAOFFSET",
                      String.valueOf(offset));
        super.setDataOffset(offset);
    }

    /**
     * @see com.ibm.mq.MQMessage#getDataOffset()
     */
    public int getDataOffset() throws IOException {
        int val;

        val = super.getDataOffset();

        m_logging.log(Level.FINER,
                      "ExtServiceMQMessage_DEBUG_MQMTD_DATAOFFSET",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#clearMessage()
     */
    public void clearMessage() throws IOException {
        m_logging.log(Level.FINE, "ExtServiceMQMessage_DEBUG_MQMTD_CLEARMESSAGE");
        super.clearMessage();
    }

    /**
     * @see com.ibm.mq.MQMessage#resizeBuffer(int)
     */
    public void resizeBuffer(int size) throws IOException {
        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_RESIZEBUFFER",
                      String.valueOf(size));
        super.resizeBuffer(size);
    }

    /**
     * @see com.ibm.mq.MQMessage#readBoolean()
     */
    public boolean readBoolean() throws EOFException,
                                        IOException {
        boolean val;

        val = super.readBoolean();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READBOOLEAN",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readByte() 
     */ 
    public byte readByte() throws IOException, EOFException {
        byte val;

        val = super.readByte();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READBYTE",
                      Integer.toHexString((int) val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readChar()
     */
    public char readChar() throws EOFException,
                                  IOException {
        char val;

        val = super.readChar();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READCHAR",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readDouble()
     */
    public double readDouble() throws EOFException,
                                      IOException {
        double val;

        val = super.readDouble();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READDOUBLE",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readFloat()
     */
    public float readFloat() throws EOFException,
                                    IOException {
        float val;

        val = super.readFloat();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READDOUBLE",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readFully(byte[])
     */
    public void readFully(byte[] buffer) throws EOFException,
                                                IOException {
        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READFULLY1",
                      String.valueOf(buffer.length));
        super.readFully(buffer);
    }

    /**
     * @see com.ibm.mq.MQMessage#readFully(byte[], int, int)
     */
    public void readFully(byte[] buffer, int offset, int len)
            throws EOFException,
                   IOException {
        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READFULLY3",new Object[]{String.valueOf(buffer.length),String.valueOf(offset),String.valueOf(len)});
                   
        super.readFully(buffer, offset, len);
    }

    /**
     * @return 
     * 
     * @see com.ibm.mq.MQMessage#readInt()
     */
    public int readInt() throws EOFException,
                                IOException {
        int val;

        val = super.readInt();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READINT",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readInt4()
     */
    public int readInt4() throws EOFException,
                                 IOException {
        int val;

        val = super.readInt4();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READINT4",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readLine()
     */
    public String readLine() throws IOException {
        String val;

        val = super.readLine();

        m_logging.log(Level.FINE, "ExtServiceMQMessage_DEBUG_MQMTD_READLINE", val);

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readLong()
     */
    public long readLong() throws EOFException,
                                  IOException {
        long val;

        val = super.readLong();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READLONG",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readInt8()
     */
    public long readInt8() throws EOFException,
                                  IOException {
        long val;

        val = super.readInt8();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READINT8",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readObject()
     */
    public Object readObject() throws ClassNotFoundException,
                                      InvalidClassException,
                                      StreamCorruptedException,
                                      OptionalDataException,
                                      IOException {
        Object val;

        val = super.readObject();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READOBJ",
                      val.getClass().getName());

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readShort()
     */
    public short readShort() throws EOFException,
                                    IOException {
        short val;

        val = super.readShort();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READSHORT",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readInt2()
     */
    public short readInt2() throws EOFException,
                                   IOException {
        short val;

        val = super.readInt2();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READINT2",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readUTF()
     */
    public String readUTF() throws IOException {
        String val;

        val = super.readUTF();

        m_logging.log(Level.FINE, "ExtServiceMQMessage_DEBUG_MQMTD_READUTF", val);

        return val;
    }

    /**
     * @return 
     * 
     * @see com.ibm.mq.MQMessage#readUnsignedByte()
     */
    public int readUnsignedByte() throws EOFException,
                                         IOException {
        int val;

        val = super.readUnsignedByte();

        m_logging.log(Level.FINE, "ExtServiceMQMessage_DEBUG_MQMTD_UBYTE", String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readUnsignedShort()
     */
    public int readUnsignedShort() throws EOFException,
                                          IOException {
        int val;

        val = super.readUnsignedShort();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_USHORT",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readUInt2()
     */
    public int readUInt2() throws EOFException,
                                  IOException {
        int val;

        val = super.readUInt2();

        m_logging.log(Level.FINE, "ExtServiceMQMessage_DEBUG_MQMTD_UINT2", String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readString(int)
     */
    public String readString(int length) throws EOFException,
                                                IOException {
        String val;

        val = super.readString(length);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READSTRING",
                      String.valueOf(length)+
                      val);

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readDecimal2()
     */
    public short readDecimal2() throws EOFException,
                                       IOException {
        short val;

        val = super.readDecimal2();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READDEC2",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readDecimal4()
     */
    public int readDecimal4() throws EOFException,
                                     IOException {
        int val;

        val = super.readDecimal4();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READDEC4",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#readDecimal8()
     */
    public long readDecimal8() throws EOFException,
                                      IOException {
        long val;

        val = super.readDecimal8();

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_READDEC8",
                      String.valueOf(val));

        return val;
    }

    /**
     * @see com.ibm.mq.MQMessage#setVersion(int)
     */
    public void setVersion(int version) throws MQBCExtServiceException {
        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_SETVERSION",
                      String.valueOf(version));

        try {
            super.setVersion(version);
        } catch (com.ibm.mq.MQException e) {
            throw new MQBCExtServiceException(e);
        }
    }

    /**
     * @see com.ibm.mq.MQMessage#skipBytes(int)
     */
    public int skipBytes(int numberOfBytes) throws EOFException,
                                                   IOException {
        int val;

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_SKIPBYTES",
                      String.valueOf(numberOfBytes));

        val = super.skipBytes(numberOfBytes);

        return val;
    }

    /**
     * @param byteValue 
     * 
     * @see com.ibm.mq.MQMessage#write(int)
     */
    public void write(int byteValue) throws IOException {
        super.write(byteValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEBYTE",
                      String.valueOf(byteValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#write(byte[])
     */
    public void write(byte[] byteArray) throws IOException {
        super.write(byteArray);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEBYTES",
                      String.valueOf(byteArray.length));
    }

    /**
     * @see com.ibm.mq.MQMessage#write(byte[], int, int)
     */
    public void write(byte[] byteArray, int offset, int len)
            throws IOException {
        super.write(byteArray, offset, len);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEBYTESPART",
                      String.valueOf(len)+
                      String.valueOf(offset));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeBoolean(boolean)
     */
    public void writeBoolean(boolean boolValue) throws IOException {
        super.writeBoolean(boolValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEBOOLEAN",
                      String.valueOf(boolValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeByte(int)
     */
    public void writeByte(int byteValue) throws IOException {
        super.writeByte(byteValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEBYTE",
                      String.valueOf(byteValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeBytes(java.lang.String)
     */
    public void writeBytes(String stringValue) throws IOException {
        super.writeBytes(stringValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITESTRINGBYTES",
                      stringValue+
                      String.valueOf(stringValue.length()));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeChar(int)
     */
    public void writeChar(int unicodeChar) throws IOException {
        super.writeChar(unicodeChar);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITECHAR",
                      String.valueOf(unicodeChar));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeChars(java.lang.String)
     */
    public void writeChars(String str) throws IOException {
        super.writeChars(str);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITECHARS",
                      String.valueOf(str));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeDouble(double)
     */
    public void writeDouble(double doubleValue) throws IOException {
        super.writeDouble(doubleValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEDOUBLE",
                      String.valueOf(doubleValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeFloat(float)
     */
    public void writeFloat(float floatValue) throws IOException {
        super.writeFloat(floatValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEFLOAT",
                      String.valueOf(floatValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeInt(int)
     */
    public void writeInt(int intValue) throws IOException {
        super.writeInt(intValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEINT",
                      String.valueOf(intValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeInt4(int)
     */
    public void writeInt4(int intValue) throws IOException {
        super.writeInt4(intValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEINT",
                      String.valueOf(intValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeLong(long)
     */
    public void writeLong(long longValue) throws IOException {
        super.writeLong(longValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITELONG",
                      String.valueOf(longValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeInt8(long)
     */
    public void writeInt8(long longValue) throws IOException {
        super.writeInt8(longValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITELONG",
                      String.valueOf(longValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeObject(java.lang.Object)
     */
    public void writeObject(Object objValue) throws IOException {
        super.writeObject(objValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEOBJ",
                      objValue.getClass().getName());
    }

    /**
     * @see com.ibm.mq.MQMessage#writeShort(int)
     */
    public void writeShort(int shortValue) throws IOException {
        super.writeShort(shortValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITESHORT",
                      String.valueOf(shortValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeInt2(int)
     */
    public void writeInt2(int shortValue) throws IOException {
        super.writeInt2(shortValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITESHORT",
                      String.valueOf(shortValue));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeDecimal2(short)
     */
    public void writeDecimal2(short decimal2Value) throws IOException {
        super.writeDecimal2(decimal2Value);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEDEC2",
                      String.valueOf(decimal2Value));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeDecimal4(int)
     */
    public void writeDecimal4(int decimal4Value) throws IOException {
        super.writeDecimal4(decimal4Value);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEDEC4",
                      String.valueOf(decimal4Value));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeDecimal8(long)
     */
    public void writeDecimal8(long decimal8Value) throws IOException {
        super.writeDecimal8(decimal8Value);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEDEC8",
                      String.valueOf(decimal8Value));
    }

    /**
     * @see com.ibm.mq.MQMessage#writeUTF(java.lang.String)
     */
    public void writeUTF(String utfString) throws IOException {
        super.writeUTF(utfString);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITEUTF",
                      utfString);
    }

    /**
     * @see com.ibm.mq.MQMessage#writeString(java.lang.String)
     */
    public void writeString(String stringValue) throws IOException {
        super.writeString(stringValue);

        m_logging.log(Level.FINE,
                      "ExtServiceMQMessage_DEBUG_MQMTD_WRITESTRING",
                      stringValue);
    }

    private Integer makeInteger(Object data) {
        if (data instanceof Integer) {
            return (Integer) data;
        } else {
            return Integer.valueOf(data.toString());
        }
    }

    private Calendar makeCalendar(Object data) throws DateTimeFormatException {
        if (data instanceof Calendar) {
            return (Calendar) data;
        } else {
            Calendar calendar = Calendar.getInstance();
            if (data instanceof Date) {
                calendar.setTime((Date) data);
            } else if (data instanceof Long) {
                calendar.setTimeInMillis((Long) data);
            } else if (data instanceof Integer) {
                calendar.setTimeInMillis((Integer) data);
            } else {
                calendar = XmlUtil.convertToCalendar(data.toString());
            }
            calendar.getTimeInMillis(); // force compute
            return calendar;
        }
    }

    /**
     * Facade that represents the descriptor portion of a Message. It provides
     * access to Message member variables, but does not validate the values
     * specified for these variables thru this implementation. Please consult
     * IBM's WebSphere MQ Java API documentation detailed information about
     * these variables and their applicable values.
     * 
     * 
     * @see com.ibm.mq.MQMessage
     */
    public class MsgHeader  {
        // Back reference to host Message.
        private ExtServiceMQMessage m_msg;

        /**
         * Create a MsgHeader.
         * 
         * @param m Owner of this object.
         */
        public MsgHeader(ExtServiceMQMessage m) {
            m_msg = m;
        }

        /**
         * Assign a value to the Message's <em>report</em> variable.
         * 
         * 
         * @param val Report request value
         * @see com.ibm.mq.MQMessage#report
         */
        public void setReport(int val) {
            m_msg.report = val;
        }

        /**
         * Retrieve the value of the Message's <em>report</em> variable.
         * 
         * 
         * @return Report request value.
         * @see com.ibm.mq.MQMessage#report
         */
        public int getReport() {
            return m_msg.report;
        }

        /**
         * Assign a value to the Message's <em>messageType</em> variable.
         * 
         * 
         * @param val Message type
         * @see com.ibm.mq.MQMessage#messageType
         */
        public void setMessageType(int val) {
            m_msg.messageType = val;
        }

        /**
         * Retrieve the value of the Message's <em>messageType</em> variable.
         * 
         * 
         * @return Message type value.
         * @see com.ibm.mq.MQMessage#messageType
         */
        public int getMessageType() {
            return m_msg.messageType;
        }

        /**
         * Assign a value to the Message's <em>expiry</em> variable.
         * 
         * 
         * @param val expiry
         * @see com.ibm.mq.MQMessage#expiry
         */
        public void setExpiry(int val) {
            m_msg.expiry = val;
        }

        /**
         * Retrieve the value of the Message's <em>expiry</em> variable.
         * 
         * 
         * @return expiry value.
         * @see com.ibm.mq.MQMessage#expiry
         */
        public int getExpiry() {
            return m_msg.expiry;
        }

        /**
         * Assign a value to the Message's <em>feedback</em> variable.
         * 
         * 
         * @param val feedback
         * @see com.ibm.mq.MQMessage#feedback
         */
        public void setFeedback(int val) {
            m_msg.feedback = val;
        }

        /**
         * Retrieve the value of the Message's <em>feedback</em> variable.
         * 
         * 
         * @return feedback value.
         * @see com.ibm.mq.MQMessage#feedback
         */
        public int getFeedback() {
            return m_msg.feedback;
        }

        /**
         * Assign a value to the Message's <em>encoding</em> variable.
         * 
         * 
         * @param val encoding type
         * @see com.ibm.mq.MQMessage#encoding
         */
        public void setEncoding(int val) {
            m_msg.encoding = val;
        }

        /**
         * Retrieve the value of the Message's <em>encoding</em> variable.
         * 
         * 
         * @return encoding value.
         * @see com.ibm.mq.MQMessage#encoding
         */
        public int getEncoding() {
            return m_msg.encoding;
        }

        /**
         * Assign a value to the Message's <em>characterSet</em> variable.
         * 
         * 
         * @param val characterSet
         * @see com.ibm.mq.MQMessage#characterSet
         */
        public void setCharacterSet(int val) {
            m_msg.characterSet = val;
        }

        /**
         * Retrieve the value of the Message's <em>characterSet</em> variable.
         * 
         * 
         * @return characterSet value.
         * @see com.ibm.mq.MQMessage#characterSet
         */
        public int getCharacterSet() {
            return m_msg.characterSet;
        }

        /**
         * Assign a value to the Message's <em>format</em> variable.
         * 
         * 
         * @param val format type
         * @see com.ibm.mq.MQMessage#format
         */
        public void setFormat(String val) {
            m_msg.format = val;
        }

        /**
         * Retrieve the value of the Message's <em>format</em> variable.
         * 
         * 
         * @return format value.
         * @see com.ibm.mq.MQMessage#format
         */
        public String getFormat() {
            return m_msg.format;
        }

        /**
         * Assign a value to the Message's <em>priority</em> variable.
         * 
         * 
         * @param val priority
         * @see com.ibm.mq.MQMessage#priority
         */
        public void setPriority(int val) {
            m_msg.priority = val;
        }

        /**
         * Retrieve the value of the Message's <em>priority</em> variable.
         * 
         * 
         * @return priority value.
         * @see com.ibm.mq.MQMessage#priority
         */
        public int getPriority() {
            return m_msg.priority;
        }

        /**
         * Assign a value to the Message's <em>persistence</em> variable.
         * 
         * 
         * @param val persistence
         * @see com.ibm.mq.MQMessage#persistence
         */
        public void setPersistence(int val) {
            m_msg.persistence = val;
        }

        /**
         * Retrieve the value of the Message's <em>persistence</em> variable.
         * 
         * 
         * @return persistence value.
         * @see com.ibm.mq.MQMessage#persistence
         */
        public int getPersistence() {
            return m_msg.persistence;
        }

        /**
         * Assign a value to the Message's <em>messageId</em> variable.
         * 
         * 
         * @param val Message ID
         * @see com.ibm.mq.MQMessage#messageId
         */
        public void setMessageId(byte[] val) {
            m_msg.messageId = val;
        }

        /**
         * Retrieve the value of the Message's <em>messageId</em> variable.
         * 
         * 
         * @return messageId value.
         * @see com.ibm.mq.MQMessage#messageId
         */
        public byte[] getMessageId() {
            return m_msg.messageId;
        }

        /**
         * Assign a value to the Message's <em>correlationId</em> variable.
         * 
         * 
         * @param val Correlation ID
         * @see com.ibm.mq.MQMessage#correlationId
         */
        public void setCorrelationId(byte[] val) {
            m_msg.correlationId = val;
        }

        /**
         * Retrieve the value of the Message's <em>correlationId</em> variable.
         * 
         * 
         * @return correlationId value.
         * @see com.ibm.mq.MQMessage#correlationId
         */
        public byte[] getCorrelationId() {
            return m_msg.correlationId;
        }

        /**
         * Assign a value to the Message's <em>backoutCount</em> variable.
         * 
         * 
         * @param val backout count
         * @see com.ibm.mq.MQMessage#backoutCount
         */
        public void setBackoutCount(int val) {
            m_msg.backoutCount = val;
        }

        /**
         * Retrieve the value of the Message's <em>backoutCount</em> variable.
         * 
         * 
         * @return backoutCount value.
         * @see com.ibm.mq.MQMessage#backoutCount
         */
        public int getBackoutCount() {
            return m_msg.backoutCount;
        }

        /**
         * Assign a value to the Message's <em>replyToQueueName</em> variable.
         * 
         * 
         * @param val replyToQueueName value
         * @see com.ibm.mq.MQMessage#replyToQueueName
         */
        public void setReplyToQueueName(String val) {
            m_msg.replyToQueueName = val;
        }

        /**
         * Retrieve the value of the Message's <em>replyToQueueName</em>
         * variable.
         * 
         * 
         * @return replyToQueueName value.
         * @see com.ibm.mq.MQMessage#replyToQueueName
         */
        public String getReplyToQueueName() {
            return m_msg.replyToQueueName;
        }

        /**
         * Assign a value to the Message's <em>replyToQueueManagerName</em>
         * variable.
         * 
         * 
         * @param val replyToQueueManagerName value
         * @see com.ibm.mq.MQMessage#replyToQueueManagerName
         */
        public void setReplyToQueueManagerName(String val) {
            m_msg.replyToQueueManagerName = val;
        }

        /**
         * Retrieve the value of the Message's <em>replyToQueueManagerName</em>
         * variable.
         * 
         * 
         * @return replyToQueueManagerName value.
         * @see com.ibm.mq.MQMessage#replyToQueueManagerName
         */
        public String getReplyToQueueManagerName() {
            return m_msg.replyToQueueManagerName;
        }

        /**
         * Assign a value to the Message's <em>userId</em> variable.
         * 
         * 
         * @param val User ID
         * @see com.ibm.mq.MQMessage#userId
         */
        public void setUserId(String val) {
            m_msg.userId = val;
        }

        /**
         * Retrieve the value of the Message's <em>userId</em> variable.
         * 
         * 
         * @return userId value.
         * @see com.ibm.mq.MQMessage#userId
         */
        public String getUserId() {
            return m_msg.userId;
        }

        /**
         * Assign a value to the Message's <em>accountingToken</em> variable.
         * 
         * 
         * @param val Accounting token type
         * @see com.ibm.mq.MQMessage#accountingToken
         */
        public void setAccountingToken(byte[] val) {
            m_msg.accountingToken = val;
        }

        /**
         * Retrieve the value of the Message's <em>accountingToken</em>
         * variable.
         * 
         * 
         * @return accountingToken value.
         * @see com.ibm.mq.MQMessage#accountingToken
         */
        public byte[] getAccountingToken() {
            return m_msg.accountingToken;
        }

        /**
         * Assign a value to the Message's <em>applicationIdData</em> variable.
         * 
         * 
         * @param val Application ID
         * @see com.ibm.mq.MQMessage#applicationIdData
         */
        public void setApplicationIdData(String val) {
            m_msg.applicationIdData = val;
        }

        /**
         * Retrieve the value of the Message's <em>applicationIdData</em>
         * variable.
         * 
         * 
         * @return applicationIdData value.
         * @see com.ibm.mq.MQMessage#applicationIdData
         */
        public String getApplicationIdData() {
            return m_msg.applicationIdData;
        }

        /**
         * Assign a value to the Message's <em>putApplicationType</em>
         * variable.
         * 
         * 
         * @param val Message type
         * @see com.ibm.mq.MQMessage#putApplicationType
         */
        public void setPutApplicationType(int val) {
            m_msg.putApplicationType = val;
        }

        /**
         * Retrieve the value of the Message's <em>putApplicationType</em>
         * variable.
         * 
         * 
         * @return putApplicationType value.
         * @see com.ibm.mq.MQMessage#putApplicationType
         */
        public int getPutApplicationType() {
            return m_msg.putApplicationType;
        }

        /**
         * Assign a value to the Message's <em>putApplicationName</em>
         * variable.
         * 
         * 
         * @param val Application name
         * @see com.ibm.mq.MQMessage#putApplicationName
         */
        public void setPutApplicationName(String val) {
            m_msg.putApplicationName = val;
        }

        /**
         * Retrieve the value of the Message's <em>putApplicationName</em>
         * variable.
         * 
         * 
         * @return putApplicationName value.
         * @see com.ibm.mq.MQMessage#putApplicationName
         */
        public String getPutApplicationName() {
            return m_msg.putApplicationName;
        }

        /**
         * Assign a value to the Message's <em>putDateTime</em> variable.
         * 
         * 
         * @param val Put date time
         * @see com.ibm.mq.MQMessage#putDateTime
         */
        public void setPutDateTime(java.util.GregorianCalendar val) {
            m_msg.putDateTime = val;
        }

        /**
         * Retrieve the value of the Message's <em>putDateTime</em> variable.
         * 
         * 
         * @return putDateTime value.
         * @see com.ibm.mq.MQMessage#putDateTime
         */
        public java.util.GregorianCalendar getPutDateTime() {
            return m_msg.putDateTime;
        }

        /**
         * Assign a value to the Message's <em>applicationOriginData</em>
         * variable.
         * 
         * 
         * @param val Application origin
         * @see com.ibm.mq.MQMessage#applicationOriginData
         */
        public void setApplicationOriginData(String val) {
            m_msg.applicationOriginData = val;
        }

        /**
         * Retrieve the value of the Message's <em>applicationOriginData</em>
         * variable.
         * 
         * 
         * @return applicationOriginData value.
         * @see com.ibm.mq.MQMessage#applicationOriginData
         */
        public String getApplicationOriginData() {
            return m_msg.applicationOriginData;
        }

        /**
         * Assign a value to the Message's <em>groupId</em> variable.
         * 
         * 
         * @param val Group ID
         * @see com.ibm.mq.MQMessage#groupId
         */
        public void setGroupId(byte[] val) {
            m_msg.groupId = val;
        }

        /**
         * Retrieve the value of the Message's <em>groupId</em> variable.
         * 
         * 
         * @return groupId value.
         * @see com.ibm.mq.MQMessage#groupId
         */
        public byte[] getGroupId() {
            return m_msg.groupId;
        }

        /**
         * Assign a value to the Message's <em>messageSequenceNumber</em>
         * variable.
         * 
         * 
         * @param val Message sequence number
         * @see com.ibm.mq.MQMessage#messageSequenceNumber
         */
        public void setMessageSequenceNumber(int val) {
            m_msg.messageSequenceNumber = val;
        }

        /**
         * Retrieve the value of the Message's <em>messageSequenceNumber</em>
         * variable.
         * 
         * 
         * @return messageSequenceNumber value.
         * @see com.ibm.mq.MQMessage#messageSequenceNumber
         */
        public int getMessageSequenceNumber() {
            return m_msg.messageSequenceNumber;
        }

        /**
         * Assign a value to the Message's <em>offset</em> variable.
         * 
         * 
         * @param val offset
         * @see com.ibm.mq.MQMessage#offset
         */
        public void setOffset(int val) {
            m_msg.offset = val;
        }

        /**
         * Retrieve the value of the Message's <em>offset</em> variable.
         * 
         * 
         * @return offset value.
         * @see com.ibm.mq.MQMessage#offset
         */
        public int getOffset() {
            return m_msg.offset;
        }

        /**
         * Assign a value to the Message's <em>messageFlags</em> variable.
         * 
         * 
         * @param val Message flags
         * @see com.ibm.mq.MQMessage#messageFlags
         */
        public void setMessageFlags(int val) {
            m_msg.messageFlags = val;
        }

        /**
         * Retrieve the value of the Message's <em>messageFlags</em> variable.
         * 
         * 
         * @return messageFlags value.
         * @see com.ibm.mq.MQMessage#messageFlags
         */
        public int getMessageFlags() {
            return m_msg.messageFlags;
        }

        /**
         * Assign a value to the Message's <em>originalLength</em> variable.
         * 
         * 
         * @param val original length
         * @see com.ibm.mq.MQMessage#originalLength
         */
        public void setOriginalLength(int val) {
            m_msg.originalLength = val;
        }

        /**
         * Retrieve the value of the Message's <em>originalLength</em>
         * variable.
         * 
         * 
         * @return originalLength value.
         * @see com.ibm.mq.MQMessage#originalLength
         */
        public int getOriginalLength() {
            return m_msg.originalLength;
        }
        
        public Object getDescriptorData(MessageDescriptors descriptor) {
            Object data;
        
            switch (descriptor) {
                case accountingToken:
                    data = accountingToken;
                    break;
                case applicationId:
                    data = applicationIdData;
                    break;
                case applicationOrigin:
                    data = applicationOriginData;
                    break;
                case backoutCount:
                    data = backoutCount;
                    break;
                case characterSet:
                    data = characterSet;
                    break;
                case correlationId:
                    data = correlationId;
                    break;
                case encoding:
                    data = encoding;
                    break;
                case expiry:
                    data = expiry;
                    break;
                case feedback:
                    data = feedback;
                    break;
                case format:
                    data = format;
                    break;
                case groupId:
                    data = groupId;
                    break;
                case messageFlags:
                    data = messageFlags;
                    break;
                case messageId:
                    data = messageId;
                    break;
                case messageSequenceNumber:
                    data = messageSequenceNumber;
                    break;
                case messageType:
                    data = messageType;
                    break;
                case offset:
                    data = offset;
                    break;
                case originalLength:
                    data = originalLength;
                    break;
                case persistence:
                    data = persistence;
                    break;
                case priority:
                    data = priority;
                    break;
                case putApplicationName:
                    data = putApplicationName;
                    break;
                case putApplicationType:
                    data = putApplicationType;
                    break;
                case putDateTime:
                    data = putDateTime;
                    break;
                case replyToQueueManager:
                    data = replyToQueueManagerName;
                    break;
                case replyToQueueName:
                    data = replyToQueueName;
                    break;
                case report:
                    data = report;
                    break;
                case userId:
                    data = userId;
                    break;
                default:
                    throw new RuntimeException(
                            I18n.msg("1800: Descriptor ''{0}'' unsupported" 
                                    + " (internal error)!",
                                    descriptor));
            }
            return data;
        }

        public void setDescriptorData(MessageDescriptors descriptor, Object data) {
            switch (descriptor) {
                case accountingToken:
                    accountingToken = (byte[]) data;
                    break;
                case applicationId:
                    applicationIdData = data.toString();
                    break;
                case applicationOrigin:
                    applicationOriginData = data.toString();
                    break;
                case backoutCount:
                    backoutCount = makeInteger(data);
                    break;
                case characterSet:
                    characterSet = makeInteger(data);
                    break;
                case correlationId:
                    correlationId = (byte[]) data;
                    break;
                case encoding:
                    encoding = makeInteger(data);
                    break;
                case expiry:
                    expiry = makeInteger(data);
                    break;
                case feedback:
                    feedback = makeInteger(data);
                    break;
                case format:
                    format = data.toString();
                    break;
                case groupId:
                    groupId = (byte[]) data;
                    break;
                case messageFlags:
                    messageFlags = makeInteger(data);
                    break;
                case messageId:
                    messageId = (byte[]) data;
                    break;
                case messageSequenceNumber:
                    messageSequenceNumber = makeInteger(data);
                    break;
                case messageType:
                    messageType = makeInteger(data);
                    break;
                case offset:
                    offset = makeInteger(data);
                    break;
                case originalLength:
                    originalLength = makeInteger(data);
                    break;
                case persistence:
                    persistence = makeInteger(data);
                    break;
                case priority:
                    priority = makeInteger(data);
                    break;
                case putApplicationName:
                    putApplicationName = data.toString();
                    break;
                case putApplicationType:
                    putApplicationType = makeInteger(data);
                    break;
                case putDateTime:
                    if (data instanceof GregorianCalendar) {
                        putDateTime = (GregorianCalendar) data;
                    } else {
                        GregorianCalendar originalTime = putDateTime;
                        putDateTime = new GregorianCalendar();
                        try {
                            putDateTime.setTime(makeCalendar(data).getTime());
                        } catch (DateTimeFormatException e) {
                            putDateTime = originalTime;
                            throw new RuntimeException(e);
                        }
                        putDateTime.getTimeInMillis(); // force recompute
                    }
                    break;
                case replyToQueueManager:
                    replyToQueueManagerName = data.toString();
                    break;
                case replyToQueueName:
                    replyToQueueName = data.toString();
                    break;
                case report:
                    report = makeInteger(data);
                    break;
                case userId:
                    userId = data.toString();
                    break;
                default:
                    throw new RuntimeException(
                            I18n.msg("1800: Descriptor ''{0}'' unsupported" 
                                    + " (internal error)!",
                                    descriptor));
            }
        }
    }

    /**
     * Facade for the data (buffer) portion of a {@link Message}.
     */
    public class MsgBody  {

        // Back reference to the host Message.
        private ExtServiceMQMessage m_msg;

        /**
         * Create a MsgBody object.
         * 
         * @param m Owner of this object.
         */
        public MsgBody(ExtServiceMQMessage m) {
            m_msg = m;
        }

        /**
         * Set content of MsgBody to the specified data.
         * 
         * @param val Data to copy to the MsgBody.
         * 
         * @throws IOException
         */
        public void setData(byte[] val) throws IOException {
            m_msg.clearMessage();
            m_msg.write(val);
        }

        /**
         * Retrieve the content of the MsgBody.
         * 
         * @return MsgBody data previously set by {@link #setData} or {@link
         *         #setDataRef}.
         * 
         * @throws EOFException 
         * @throws IOException  
         */
        public byte[] getData() throws EOFException,
                                                IOException {
            byte[] data = new byte[m_msg.getMessageLength()];
            m_msg.readFully(data);
            return data;
        }

        /**
         * Synonym for {@link #setData(byte[])}.
         */
        public void setByteArrayData(byte[] val)
                throws IOException {
            setData(val);
        }

        /**
         * Synonym for {@link #getByteArrayData()}.
         */
        public byte[] getByteArrayData()
                throws EOFException,
                       IOException {
            return getData();
        }
        
    }

    /**
     * Event m_Logger
     */
     private static final Messages mMessages = Messages.getMessages( ExtServiceMQMessage.class );
     private static Logger m_logging = Messages.getLogger( ExtServiceMQMessage.class );
    
 

 
    /**
     * Message body
     */
    private final MsgBody m_body;

    /**
     * Message header
     */
    private final MsgHeader m_hdr;

  
    /**
     * Version datum for BPEL persistence
     */ 
    private static final int C_VERSION = 1;
    
    /**
     * Syncpoint controller thru which this message was obtained. This is
     * optional information is provided by the class user; its validity is not
     * guaranteed
     */ 
    private SyncpointControl m_syncpointController;
}
