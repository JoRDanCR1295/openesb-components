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
 * @(#)MessagesGenerator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.hl7.xsdbuilder;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import com.sun.encoder.codegen.Emit;
import com.sun.encoder.hl7.util.Util;
import com.sun.encoder.hl7.HL7Encoder;
import com.sun.encoder.hl7.HL7EncoderProvider;
import com.sun.encoder.util.UnicodeFile;

/**
 * This is the generator that generates the messages.xsd and XSDs for all
 * message structures.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class MessagesGenerator extends BaseGenerator implements XSDGenerator {

    /**
     * The location of the resource that contains the SQL statement that
     * retrieves all message/event combinations for the sending activity
     */
    public static final String SNDMSGEVENTS_SQL =
        "com/sun/encoder/hl7/xsdbuilder/sndmsgevents.sql";
    
    public static final String RETMSGEVENTS_SQL =
        "com/sun/encoder/hl7/xsdbuilder/retmsgevents.sql";
    
    public static final String MSGEVENTSEGMENTS_SQL =
        "com/sun/encoder/hl7/xsdbuilder/msgeventsegments.sql";
    
    public static final String MSGSTRUCTS_SQL =
        "com/sun/encoder/hl7/xsdbuilder/msgstructs.sql";
    
    public static final String MSGSTRUCTSEGMENTS_SQL =
        "com/sun/encoder/hl7/xsdbuilder/msgstructsegments.sql";
    
    private final NameGenerator mNameGen;
    
    /**
     * Constructs from a DB connection, an HL7 version and a target location
     * for generated files.
     * 
     * @param conn the DB connection
     * @param version the HL7 version
     * @param targetLocation the target location for generated files
     * @param nameGen name generator
     */
    MessagesGenerator(Connection conn, String version,
            File targetLocation, NameGenerator nameGen) {
        super(conn, version, targetLocation);
        mNameGen = nameGen;
    }

    public void generate() throws GeneratorException {
        Set<String> processedMsgs = new HashSet<String>(); 
        Set<String> generatedMsgs = new HashSet<String>(); 
        String sqlMsgs;
        PreparedStatement pstmtMsgs = null;
        String sqlOneMsg;
        PreparedStatement pstmtMsgStructSegs = null;
        PreparedStatement pstmtMsgEvtSegs = null;
        ResultSet rsMsgs = null;
        ResultSet rsOneMsg = null;
        try {
            //Prepared statement for querying segments from message structure
            sqlOneMsg = getSQLStmt(MSGSTRUCTSEGMENTS_SQL);
            pstmtMsgStructSegs =
                mConn.prepareStatement(
                        sqlOneMsg,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            //Prepared statement for querying segments from message/event
            sqlOneMsg = getSQLStmt(MSGEVENTSEGMENTS_SQL);
            pstmtMsgEvtSegs =
                mConn.prepareStatement(
                        sqlOneMsg,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            
            //Prepared statement for querying all message/event combinations
            //for sending activities
            sqlMsgs = getSQLStmt(SNDMSGEVENTS_SQL);
            pstmtMsgs =
                mConn.prepareStatement(
                        sqlMsgs,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            pstmtMsgs.setString(1, mHL7Version);
            rsMsgs = pstmtMsgs.executeQuery();
            
            rsMsgs.beforeFirst();
            while (rsMsgs.next()) {
                String msgStruct = rsMsgs.getString("msg_struct");
                String msgType = rsMsgs.getString("msg_type");
                String evtCode = rsMsgs.getString("evt_code");
                if (msgStruct == null || msgStruct.equals("?")
                        || msgStruct.equals("NUL")) {
                    msgStruct = msgType + "_" + evtCode;
                }
                if (processedMsgs.contains(msgStruct)) {
                    continue;
                }
                pstmtMsgStructSegs.setString(1, mHL7Version);
                pstmtMsgStructSegs.setString(2, msgStruct);
                rsOneMsg = pstmtMsgStructSegs.executeQuery();
                Message message = new Message(msgStruct, mNameGen);
                if (!loadMessage(rsOneMsg, message)) {
                    //No definition found in HL7MsgStructIDSegments
                    rsOneMsg.close();
                    rsOneMsg = null;
                    pstmtMsgEvtSegs.setString(1, mHL7Version);
                    pstmtMsgEvtSegs.setString(2, msgType);
                    pstmtMsgEvtSegs.setString(3, evtCode);
                    rsOneMsg = pstmtMsgEvtSegs.executeQuery();
                    if (!loadMessage(rsOneMsg, message)) {
                        //No definition found in HL7EventMessageTypeSegments
                        rsOneMsg.close();
                        rsOneMsg = null;
                        processedMsgs.add(msgStruct);
                        continue;
                    }
                }
                rsOneMsg.close();
                rsOneMsg = null;
                System.out.println(msgStruct);
                generateOneMessage(message);
                processedMsgs.add(msgStruct);
                generatedMsgs.add(msgStruct);
            }
            rsMsgs.close();
            rsMsgs = null;
            pstmtMsgs.close();
            pstmtMsgs = null;
            
            //Prepared statement for querying all message/event combinations
            //for reply activities
            sqlMsgs = getSQLStmt(RETMSGEVENTS_SQL);
            pstmtMsgs =
                mConn.prepareStatement(
                        sqlMsgs,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            pstmtMsgs.setString(1, mHL7Version);
            rsMsgs = pstmtMsgs.executeQuery();
            
            rsMsgs.beforeFirst();
            while (rsMsgs.next()) {
                String msgStruct = rsMsgs.getString("msg_struct");
                String msgType = rsMsgs.getString("msg_type");
                String evtCode = rsMsgs.getString("evt_code");
                if (msgStruct == null || msgStruct.equals("?")
                        || msgStruct.equals("NUL")) {
                    msgStruct = msgType + "_" + evtCode;
                }
                if (processedMsgs.contains(msgStruct)) {
                    continue;
                }
                pstmtMsgStructSegs.setString(1, mHL7Version);
                pstmtMsgStructSegs.setString(2, msgStruct);
                rsOneMsg = pstmtMsgStructSegs.executeQuery();
                Message message = new Message(msgStruct, mNameGen);
                if (!loadMessage(rsOneMsg, message)) {
                    //No definition found in HL7MsgStructIDSegments
                    rsOneMsg.close();
                    rsOneMsg = null;
                    pstmtMsgEvtSegs.setString(1, mHL7Version);
                    pstmtMsgEvtSegs.setString(2, msgType);
                    pstmtMsgEvtSegs.setString(3, evtCode);
                    rsOneMsg = pstmtMsgEvtSegs.executeQuery();
                    if (!loadMessage(rsOneMsg, message)) {
                        //No definition found in HL7EventMessageTypeSegments
                        rsOneMsg.close();
                        rsOneMsg = null;
                        processedMsgs.add(msgStruct);
                        continue;
                    }
                }
                rsOneMsg.close();
                rsOneMsg = null;
                System.out.println(msgStruct);
                generateOneMessage(message);
                processedMsgs.add(msgStruct);
                generatedMsgs.add(msgStruct);
            }
            rsMsgs.close();
            rsMsgs = null;
            pstmtMsgs.close();
            pstmtMsgs = null;
            
            //Prepared statement for querying all message structures
            sqlMsgs = getSQLStmt(MSGSTRUCTS_SQL);
            pstmtMsgs =
                mConn.prepareStatement(
                        sqlMsgs,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            pstmtMsgs.setString(1, mHL7Version);
            rsMsgs = pstmtMsgs.executeQuery();
            
            rsMsgs.beforeFirst();
            while (rsMsgs.next()) {
                String msgStruct = rsMsgs.getString("msg_struct");
                if (msgStruct == null || msgStruct.equals("?")
                        || msgStruct.equals("NUL")) {
                    continue;
                }
                if (processedMsgs.contains(msgStruct)) {
                    continue;
                }
                pstmtMsgStructSegs.setString(1, mHL7Version);
                pstmtMsgStructSegs.setString(2, msgStruct);
                rsOneMsg = pstmtMsgStructSegs.executeQuery();
                Message message = new Message(msgStruct, mNameGen);
                if (!loadMessage(rsOneMsg, message)) {
                    //No definition found in HL7MsgStructIDSegments
                    rsOneMsg.close();
                    rsOneMsg = null;
                    processedMsgs.add(msgStruct);
                    continue;
                }
                rsOneMsg.close();
                rsOneMsg = null;
                System.out.println(msgStruct);
                generateOneMessage(message);
                processedMsgs.add(msgStruct);
                generatedMsgs.add(msgStruct);
            }
            rsMsgs.close();
            rsMsgs = null;
            pstmtMsgs.close();
            pstmtMsgs = null;
        } catch (IOException e) {
            throw new GeneratorException(e);
        } catch (SQLException e) {
            throw new GeneratorException(e);
        } finally {
            Exception expRet = null;
            if (rsOneMsg != null) {
                try {
                    rsOneMsg.close();
                } catch (SQLException e) {
                    expRet = e;
                }
            }
            if (pstmtMsgStructSegs != null) {
                try {
                    pstmtMsgStructSegs.close();
                } catch (SQLException e) {
                    expRet = e;
                }
            }
            if (pstmtMsgEvtSegs != null) {
                try {
                    pstmtMsgEvtSegs.close();
                } catch (SQLException e) {
                    expRet = e;
                }
            }
            if (rsMsgs != null) {
                try {
                    rsMsgs.close();
                } catch (SQLException e) {
                    expRet = e;
                }
            }
            if (pstmtMsgs != null) {
                try {
                    pstmtMsgs.close();
                } catch (SQLException e) {
                    expRet = e;
                }
            }
            if (expRet != null) {
                throw new GeneratorException(expRet);
            }
        }
        
        generateMessagesXSD(generatedMsgs);
        generateBatchXSD();
    }

    private void printMsgHeader(Emit emit, String msgName) {
        emit.emit("<?xml version =\"1.0\" encoding=\"UTF-8\"?>");
        emit.down("<!--");
        emit.emit("v2.xml Message Definitions Version v"
                + mHL7Version + "  - " + msgName);
        emit.emit("HL7® Version " + mHL7Version + ", © Health Level Seven, Inc.  All rights reserved.  HL7 and Health Level Seven are registered trademarks of Health Level Seven, Inc.");
        emit.done("-->");
        emit.down("<xsd:schema");
        emit.emit("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"");
        emit.emit("xmlns=\"" + HL7Encoder.HL7_NS + "\"");
        emit.emit("xmlns:hl7=\"" + HL7Encoder.HL7_ENCODER_NAMESPACE + "\"");
        emit.emit("targetNamespace=\"" + HL7Encoder.HL7_NS
            + "\" xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\" jaxb:version=\"2.0\">");
        emit.emit();
        emit.emit("<!-- include segment definitions for version v"
                + mHL7Version + " -->");
        emit.emit("<xsd:include schemaLocation=\"segments.xsd\"/>");
        emit.emit();
        emit.down("<xsd:annotation>");
        emit.down("<xsd:appinfo source=\"" + HL7Encoder.ENCODER_NAMESPACE + "\">");
        emit.emit("<encoding xmlns=\"" + HL7Encoder.ENCODER_NAMESPACE
            + "\" name=\"HL7 v2 Encoding\" namespace=\"" + HL7Encoder.HL7_ENCODER_NAMESPACE
            + "\" style=\"" + HL7EncoderProvider.STYLE_ID + "\"/>");
        emit.done("</xsd:appinfo>");
        emit.done("</xsd:annotation>");
        emit.emit();
        emit.down("<!--");
        emit.emit("MESSAGE " + msgName);
        emit.done("-->");
    }
    
    private void printBatchHeader(Emit emit, String msgName, String batchName) {
        emit.emit("<?xml version =\"1.0\" encoding=\"UTF-8\"?>");
        emit.down("<!--");
        emit.emit("v2.xml Message Definitions Version v"
                + mHL7Version + "  - " + batchName);
        emit.emit("HL7® Version " + mHL7Version + ", © Health Level Seven, Inc.  All rights reserved.  HL7 and Health Level Seven are registered trademarks of Health Level Seven, Inc.");
        emit.done("-->");
        emit.down("<xsd:schema");
        emit.emit("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"");
        emit.emit("xmlns=\"" + HL7Encoder.HL7_NS + "\"");
        emit.emit("targetNamespace=\"" + HL7Encoder.HL7_NS + "\">");
        emit.emit();
        emit.emit("<!-- include message definitions for version v"
                + mHL7Version + " -->");
        emit.emit("<xsd:include schemaLocation=\"" + msgName + ".xsd\"/>");
        emit.emit();
        emit.down("<xsd:annotation>");
        emit.down("<xsd:appinfo source=\"" + HL7Encoder.ENCODER_NAMESPACE + "\">");
        emit.emit("<encoding xmlns=\"" + HL7Encoder.ENCODER_NAMESPACE
            + "\" name=\"HL7 v2 Encoding\" namespace=\"" + HL7Encoder.HL7_ENCODER_NAMESPACE
            + "\" style=\"" + HL7EncoderProvider.STYLE_ID + "\"/>");
        emit.done("</xsd:appinfo>");
        emit.done("</xsd:annotation>");
        emit.emit();
        emit.down("<!--");
        emit.emit("BATCH " + batchName);
        emit.done("-->");

        emit.emit("<!-- .. definitions used in batch message -->");
        emit.down("<xsd:element name=\"" + msgName + "." + Util.MESSAGEBATCH + "\">");
        emit.down("<xsd:complexType>");
        emit.down("<xsd:sequence>");
        emit.emit("<xsd:element ref=\"" + Util.BHS + "\" minOccurs=\"0\""
                + " maxOccurs=\"1\" />");
        emit.emit("<xsd:element ref=\"" + Util.QRD + "\" minOccurs=\"0\""
                + " maxOccurs=\"1\" />");
        emit.emit("<xsd:element ref=\"" + Util.QRF + "\"  minOccurs=\"0\""
                + " maxOccurs=\"1\" />");
        emit.emit("<xsd:element ref=\"" + msgName + "." + Util.MESSAGES + "\" minOccurs=\"0\""
                + " maxOccurs=\"unbounded\" />");
        emit.emit("<xsd:element ref=\"" + Util.BTS + "\" minOccurs=\"0\""
                + " maxOccurs=\"1\" />");
        emit.done("</xsd:sequence>");
        emit.done("</xsd:complexType>");
        emit.done("</xsd:element>");
        emit.emit("<xsd:element name=\"" + msgName + "." + Util.MESSAGES + "\" type=\"" + msgName + ".CONTENT" + "\"/>");
        emit.emit("<!-- .. batch message definition -->");
        emit.emit("<xsd:element name=\"" + msgName + "." + Util.BATCH + "\" type=\"BATCH.CONTENT\">");
        emit.down("<xsd:annotation>");
        emit.down("<xsd:appinfo source=\"" + HL7Encoder.ENCODER_NAMESPACE + "\">");
        emit.emit("<top xmlns=\"" + HL7Encoder.ENCODER_NAMESPACE + "\">true</top>");
        emit.done("</xsd:appinfo>");
        emit.done("</xsd:annotation>");
        emit.done("</xsd:element>");
        emit.down("<xsd:complexType name=\"BATCH.CONTENT\">");
        emit.down("<xsd:sequence>");
        emit.emit("<xsd:element ref=\"" + Util.FHS + "\" minOccurs=\"0\""
                + " maxOccurs=\"1\" />");
        emit.emit("<xsd:element ref=\"" + msgName + "." + Util.MESSAGEBATCH + "\" minOccurs=\"1\""
                + " maxOccurs=\"unbounded\" />");
        emit.emit("<xsd:element ref=\"" + Util.FTS + "\" minOccurs=\"0\""
                + " maxOccurs=\"1\" />");
        emit.done("</xsd:sequence>");
        emit.done("</xsd:complexType>");
        emit.done("</xsd:schema>");
    }

    private void printMsgFooter(Emit emit) {
        emit.done("</xsd:schema>");
    }
    
    private void generateOneMessage(Message message)
            throws SQLException, FileNotFoundException, IOException {
        String msgName = message.getName();
        String batchName = msgName + ".BATCH";
        Writer messageWriter = getWriter(msgName);
        Writer batchWriter = getWriter(batchName);
        Emit messageEmit = new Emit(messageWriter);
        Emit batchEmit = new Emit(batchWriter);
        printMsgHeader(messageEmit, msgName);
        printBatchHeader(batchEmit, msgName, batchName);
        StringBuffer groupDefinition = new StringBuffer();
        StringBuffer contentDefinition = new StringBuffer();
        Map<String, Group> namesUsed = new HashMap<String, Group>();
        message.toXSD(groupDefinition, contentDefinition, namesUsed);
        messageEmit.emit("<!-- .. groups used in message "
                + msgName + " -->");
        messageWriter.write(groupDefinition.toString());
        messageEmit.emit("<!-- .. message definition " + msgName + " -->");
        messageWriter.write(contentDefinition.toString());
        printMsgFooter(messageEmit);
        messageEmit.close();
        batchEmit.close();
        moveFile(getTempTargetFile(msgName), getTargetFile(msgName));
        moveFile(getTempTargetFile(batchName), getTargetFile(batchName));
    }
    
    private boolean loadMessage(ResultSet rs, Message message)
            throws SQLException {
        boolean hasRecords = false;
        Stack<Group> groupStack = new Stack<Group>();
        groupStack.push(message);
        Group currentGroup = message;
        while (rs.next()) {
            //seg_code, seq_no, groupname, usage, repetitional, optional
            String segCode = rs.getString("seg_code");
            if ("?".equals(segCode)) {
                continue;
            }
            hasRecords = true;
            if ("[".equals(segCode)) {
                groupStack.push(new Sequence(message.getName(),
                        rs.getString("groupname"), mNameGen));
                currentGroup.addElement(groupStack.peek());
                currentGroup = groupStack.peek();
                currentGroup.setOptional(true);
            } else if ("[{".equals(segCode)) {
                groupStack.push(new Sequence(message.getName(),
                        rs.getString("groupname"), mNameGen));
                currentGroup.addElement(groupStack.peek());
                currentGroup = groupStack.peek();
                currentGroup.setOptional(true);
                currentGroup.setRepet(true);
            } else if ("]".equals(segCode)) {
                groupStack.pop();
                currentGroup = groupStack.peek();
            } else if ("{".equals(segCode)) {
                groupStack.push(new Sequence(message.getName(),
                        rs.getString("groupname"), mNameGen));
                currentGroup.addElement(groupStack.peek());
                currentGroup = groupStack.peek();
                currentGroup.setRepet(true);
            } else if ("|".equals(segCode)) {
                //ignore. kind of redundant in a choice group  
            } else if ("}".equals(segCode)) {
                groupStack.pop();
                currentGroup = groupStack.peek();
            } else if ("}]".equals(segCode)) {
                groupStack.pop();
                currentGroup = groupStack.peek();
            } else if ("<".equals(segCode)) {
                groupStack.push(new Choice(message.getName(),
                        rs.getString("groupname"), mNameGen));
                currentGroup.addElement(groupStack.peek());
                currentGroup = groupStack.peek();
            } else if (">".equals(segCode)) {
                groupStack.pop();
                currentGroup = groupStack.peek();
            } else {
                //A segment
                Segment seg = new Segment(segCode);
                if (rs.getBoolean("optional")) {
                    seg.setOptional(true);
                }
                if (rs.getBoolean("repetitional")) {
                    seg.setRepet(true);
                }
                currentGroup.addElement(seg);
            }
        }
        message.normalize();
        return hasRecords;
    }
    
    private void generateMessagesXSD(Set<String> processedMsgs)
            throws GeneratorException {
        
        Emit emit = null;
        Writer writer = null;
        try {
            String[] messageNames = new String[processedMsgs.size()];
            processedMsgs.toArray(messageNames);
            Arrays.sort(messageNames);
            writer = getWriter("messages");
            emit = new Emit(writer);
    
            //Print the header
            emit.emit("<?xml version = \"1.0\" encoding=\"UTF-8\"?>");
            emit.down("<!--");
            emit.emit("v2.xml Message Definitions Version v"
                    + mHL7Version + "  - messages");
            emit.emit("HL7® Version " + mHL7Version + ", © Health Level Seven, Inc.  All rights reserved.  HL7 and Health Level Seven are registered trademarks of Health Level Seven, Inc.");
            emit.done("-->");
            emit.down("<xsd:schema");
            emit.emit("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"");
            emit.emit("xmlns=\"" + HL7Encoder.HL7_NS + "\"");
            emit.emit("targetNamespace=\"" + HL7Encoder.HL7_NS + "\">");
            
            //print includes
            for (int i = 0; i < messageNames.length; i++) {
                emit.emit("<!-- include message defintions for "
                        + messageNames[i] + " -->");
                emit.emit("<xsd:include schemaLocation=\""
                        + messageNames[i] + ".xsd\"/>");
            }
    
            //print references to all messages
            emit.emit("<!-- .. all message definitions -->");
            emit.down("<xsd:group name=\"ALLMESSAGES.CONTENT\">");
            emit.down("<xsd:choice>");
    
            for (int i = 0; i < messageNames.length; i++) {
                emit.emit("<xsd:element ref=\""
                        + messageNames[i]
                        + "\" minOccurs=\"0\" maxOccurs=\"unbounded\" />");
            }
            emit.done("</xsd:choice>");
            emit.done("</xsd:group>");
            emit.done("</xsd:schema>");
            emit.close();
            writer = null;
            emit = null;
            moveFile(getTempTargetFile("messages"),
                    getTargetFile("messages"));
        } catch (IOException e) {
            throw new GeneratorException(e);
        } finally {
            if (emit != null) {
                try {
                    emit.close();
                } catch (IOException e) {
                    throw new GeneratorException(e);
                }
            }
            getTempTargetFile("messages").delete();
        }
    }
    
    private void generateBatchXSD() throws GeneratorException {
        
        Emit emit = null;
        Writer writer = null;
        try {
            writer = getWriter("batch");
            emit = new Emit(writer);
    
            //Print the header
            emit.emit("<?xml version = \"1.0\" encoding=\"UTF-8\"?>");
            emit.down("<!--");
            emit.emit("v2.xml Message Definitions Version v"
                    + mHL7Version + "  - messages");
            emit.emit("HL7® Version " + mHL7Version + ", © Health Level Seven, Inc.  All rights reserved.  HL7 and Health Level Seven are registered trademarks of Health Level Seven, Inc.");
            emit.done("-->");
            emit.down("<xsd:schema");
            emit.emit("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"");
            emit.emit("xmlns=\"" + HL7Encoder.HL7_NS + "\"");
            emit.emit("targetNamespace=\"" + HL7Encoder.HL7_NS + "\">");
            emit.emit();
            emit.emit("<!-- include segment definitions for version v"
                    + mHL7Version + " -->");
            emit.emit("<xsd:include schemaLocation=\"segments.xsd\"/>");
            emit.emit();
            emit.emit("<!-- .. definitions used in batch message -->");
            emit.down("<xsd:element name=\"" + Util.MESSAGEBATCH + "\">");
            emit.down("<xsd:complexType>");
            emit.down("<xsd:sequence>");
            emit.emit("<xsd:element ref=\"" + Util.BHS + "\" minOccurs=\"0\""
                    + " maxOccurs=\"1\" />");
            emit.emit("<xsd:element ref=\"" + Util.QRD + "\" minOccurs=\"0\""
                    + " maxOccurs=\"1\" />");
            emit.emit("<xsd:element ref=\"" + Util.QRF + "\"  minOccurs=\"0\""
                    + " maxOccurs=\"1\" />");
            emit.emit("<xsd:element ref=\"" + Util.MESSAGES + "\" minOccurs=\"0\""
                    + " maxOccurs=\"unbounded\" />");
            emit.emit("<xsd:element ref=\"" + Util.BTS + "\" minOccurs=\"0\""
                    + " maxOccurs=\"1\" />");
            emit.done("</xsd:sequence>");
            emit.done("</xsd:complexType>");
            emit.done("</xsd:element>");
            emit.emit("<xsd:element name=\"" + Util.MESSAGES + "\" type=\"xsd:string\"/>");
            emit.emit("<!-- .. batch message definition -->");
            emit.emit("<xsd:element name=\"" + Util.BATCH + "\" type=\"BATCH.CONTENT\"/>");
            emit.down("<xsd:complexType name=\"BATCH.CONTENT\">");
            emit.down("<xsd:sequence>");
            emit.emit("<xsd:element ref=\"" + Util.FHS + "\" minOccurs=\"0\""
                    + " maxOccurs=\"1\" />");
            emit.emit("<xsd:element ref=\"" + Util.MESSAGEBATCH + "\" minOccurs=\"1\""
                    + " maxOccurs=\"unbounded\" />");
            emit.emit("<xsd:element ref=\"" + Util.FTS + "\" minOccurs=\"0\""
                    + " maxOccurs=\"1\" />");
            emit.done("</xsd:sequence>");
            emit.done("</xsd:complexType>");
            emit.done("</xsd:schema>");
            emit.close();
            writer = null;
            emit = null;
            moveFile(getTempTargetFile("batch"),
                    getTargetFile("batch"));
        } catch (IOException e) {
            throw new GeneratorException(e);
        } finally {
            if (emit != null) {
                try {
                    emit.close();
                } catch (IOException e) {
                    throw new GeneratorException(e);
                }
            }
            getTempTargetFile("batch").delete();
        }
    }
    
    private Writer getWriter(String seed)
            throws FileNotFoundException, IOException {
        return UnicodeFile.makeOutputWriter(getTempTargetFile(seed));
    }
    
    private File getTempTargetFile(String seed) {
        return new File(mTargetLocation, seed + ".xsd.~temp");
    }
    
    private File getTargetFile(String seed) {
        return new File(mTargetLocation, seed + ".xsd");
    }
    
    private static abstract class Element {
        
        public static final int SEGMENT = 1;
        public static final int SEQUENCE = 2;
        public static final int CHOICE = 3;
        
        private final int mKind;
        private boolean mIsOptional = false;
        private boolean mIsRepet = false;
        
        public Element(int kind) {
            mKind = kind;
        }
        
        public int getType() {
            return mKind;
        }
        
        public abstract String getName();
        
        public boolean isOptional() {
            return mIsOptional;
        }

        public boolean isRepet() {
            return mIsRepet;
        }
        
        public void setOptional(boolean optional) {
            mIsOptional = optional;
        }
        
        public void setRepet(boolean repet) {
            mIsRepet = repet;
        }
        
        public boolean isGroup() {
            return mKind != SEGMENT;
        }
        
        /**
         * Serializes segment or group reference into XSD format, and also
         * return additional group definition (if there is any) in a string
         * buffer.
         *    
         * @param groupDefinition the string buffer to hold additional group
         *              definition
         * @param contentDefinition the string buffer to hold the content
         *        definition
         * @param groupNames group names that have been used
         * @return the segment or group reference in XSD format
         */
        public abstract String toXSD(StringBuffer groupDefinition,
                StringBuffer contentDefinition, Map<String, Group> groupNames);
    }
        
    private static class Segment extends Element {
        
        private final String mName;
        
        public Segment(String name) {
            super(Element.SEGMENT);
            if ("Zxx".equals(name)) {
                mName = "anyZSegment";
            } else if ("Hxx".equals(name)) {
                mName = "anyHL7Segment";
            } else {
                mName = name;
            }
        }

        @Override
        public String getName() {
            return mName;
        }

        @Override
        public String toXSD(StringBuffer groupDefinition,
                StringBuffer contentDefinition, Map<String, Group> groupNames) {
            StringBuffer sb = new StringBuffer();
            sb.append("<xsd:element ref=\"" + getName() + "\"");
            if (isOptional()) {
                sb.append(" minOccurs=\"0\"");
            } else {
                sb.append(" minOccurs=\"1\"");
            }
            if (isRepet()) {
                sb.append(" maxOccurs=\"unbounded\"");
            } else {
                sb.append(" maxOccurs=\"1\"");
            }
            sb.append("/>");
            return sb.toString();
        }
    }
    
    private static abstract class Group extends Element {
        
        private final List<Element> mElements = new ArrayList<Element>();
        private final String mMsgName;
        private String mGroupName;
        
        public Group(int kind, String msgName, String groupName) {
            super(kind);
            mMsgName = msgName;
            if (groupName != null) {
                groupName = groupName.trim();
                if (groupName.length() == 0) {
                    groupName = null;
                }
            }
            mGroupName = groupName;
        }

        @Override
        public String getName() {
            if (mGroupName == null) {
                return mMsgName + "." + getConcatName() + "_SUPPGRP";
            }
            return mMsgName + "." + mGroupName;
        }

        public String getConcatName() {
            Iterator<Element> iter = mElements.iterator();
            StringBuffer sb = new StringBuffer();
            while (iter.hasNext()) {
                Element elem = iter.next();
                if (elem.getType() == SEGMENT) {
                    sb.append(elem.getName());
                } else {
                    sb.append(((Group) elem).getConcatName());
                }
            }
            return sb.toString();
        }
        
        public boolean isAnonymous() {
            return mGroupName == null;
        }
        
        void addElement(Element elem) {
            mElements.add(elem);
        }
        
        protected List<Element> getElements() {
            return mElements;
        }
        
        public void normalize() {
            for (int i = 0; i < mElements.size(); i++) {
                Element elem = mElements.get(i);
                boolean needNormalize = true;
                while (elem.isGroup() && needNormalize) {
                    needNormalize = false;
                    Group group = (Group) elem;
                    if (group.mElements.size() == 0) {
                        mElements.set(i, null);
                    } else if (group.mElements.size() == 1
                            && group.mElements.get(0).isGroup()
                            && (((Group) group.mElements.get(0)).isAnonymous()
                                    || group.isAnonymous())) {
                        elem = group.mElements.get(0);
                        if (((Group) elem).isAnonymous()
                                && !group.isAnonymous()) {
                            //propagate the name
                            ((Group) elem).mGroupName = group.mGroupName;
                        }
                        if (group.isOptional()) {
                            elem.setOptional(true);
                        }
                        if (group.isRepet()) {
                            elem.setRepet(true);
                        }
                        group.mElements.remove(0);
                        mElements.set(i, elem);
                        needNormalize = true;
                    }
                }
            }
            int i = 0;
            while (i < mElements.size()) {
                Element elem = mElements.get(i);
                if (elem == null) {
                    mElements.remove(i);
                } else {
                    if (elem.isGroup()) {
                        ((Group) elem).normalize();
                    }
                    i++;
                }
            }
        }
        
        protected String crackOutName(Map<String, Group> nameUsed) {
            String name = getName();
            int i = 2;
            while(nameUsed.containsKey(name) && nameUsed.get(name) != this) {
                name = mMsgName + "." + getConcatName() + "_SUPPGRP" + i;
                i++;
            }
            nameUsed.put(name, this);
            return name;
        }
    }
    
    private static class Sequence extends Group {

        private final NameGenerator mNameGen;
        private String mName = null;
        
        public Sequence(String msgName, String groupName,
                NameGenerator nameGen) {
            super(Element.SEQUENCE, msgName, groupName);
            mNameGen = nameGen;
        }

        @Override
        public String getName() {
            if (mName != null) {
                return mName;
            } else {
                return super.getName();
            }
        }
        
        public boolean isMessage() {
            return false;
        }
        
        @Override
        public String toXSD(StringBuffer groupDefinition,
                StringBuffer contentDefinition, Map<String, Group> groupNames) {
            //Emit group definition
            StringWriter writer = new StringWriter();
            Emit emit = new Emit(writer,0, 4);
            emit.indent();
            String name = crackOutName(groupNames);
            if (!name.equals(getName())) {
                mName = name;
            }
            String ctName = getName() + ".CONTENT";
            emit.down("<xsd:complexType name=\"" + ctName + "\">");
            if (!mNameGen.nameExists(ctName)) {
                mNameGen.markUsed(ctName);
            } else {
                emit.down("<xsd:annotation>");
                printJAXBCustomization(emit, mNameGen.suggestClassName(ctName));
                emit.done("</xsd:annotation>");
            }
            emit.down("<xsd:sequence>");
            Iterator<Element> iter = getElements().iterator();
            StringBuffer sb1 = new StringBuffer();
            StringBuffer sb2 = new StringBuffer();
            while (iter.hasNext()) {
                emit.emit(iter.next().toXSD(sb1, sb2, groupNames));
                sb1.append(sb2.toString());
                sb2.setLength(0);
            }
            emit.done("</xsd:sequence>");
            emit.done("</xsd:complexType>");
            if (isMessage()) {
                emit.down("<xsd:element name=\"" + getName()
                        + "\" type=\"" + getName() + ".CONTENT\">");
                emit.down("<xsd:annotation>");
                emit.down("<xsd:appinfo source=\"" + HL7Encoder.ENCODER_NAMESPACE + "\">");
                emit.emit("<top xmlns=\"" + HL7Encoder.ENCODER_NAMESPACE + "\">true</top>");
                emit.done("</xsd:appinfo>");
                if (!mNameGen.nameExists(getName())) {
                    mNameGen.markUsed(getName());
                } else {
                    printJAXBCustomization(emit, mNameGen.suggestClassName(getName()));
                }
                emit.done("</xsd:annotation>");
                emit.done("</xsd:element>");
            } else {
                if (!mNameGen.nameExists(getName())) {
                    emit.down("<xsd:element name=\"" + getName()
                            + "\" type=\"" + getName() + ".CONTENT\"/>");
                    mNameGen.markUsed(getName());
                } else {
                    emit.down("<xsd:element name=\"" + getName()
                            + "\" type=\"" + getName() + ".CONTENT\">");
                    emit.down("<xsd:annotation>");
                    printJAXBCustomization(emit, mNameGen.suggestClassName(getName()));
                    emit.done("</xsd:annotation>");
                    emit.done("</xsd:element>");
                }
            }
            contentDefinition.append(writer.toString());
            groupDefinition.append(sb1.toString());

            //return the reference in XSD format
            sb1.setLength(0);
            sb1.append("<xsd:element ref=\"" + getName() + "\"");
            if (isOptional()) {
                sb1.append(" minOccurs=\"0\"");
            } else {
                sb1.append(" minOccurs=\"1\"");
            }
            if (isRepet()) {
                sb1.append(" maxOccurs=\"unbounded\"");
            } else {
                sb1.append(" maxOccurs=\"1\"");
            }
            sb1.append("/>");
            return sb1.toString();
        }
    }
    
    private static class Choice extends Group {
        
        private final NameGenerator mNameGen;
        private String mName = null;
        
        public Choice(String msgName, String groupName, NameGenerator nameGen) {
            super(Element.CHOICE, msgName, groupName);
            mNameGen = nameGen;
        }

        @Override
        public String getName() {
            if (mName != null) {
                return mName;
            } else {
                return super.getName();
            }
        }

        @Override
        public String toXSD(StringBuffer groupDefinition,
                StringBuffer contentDefinition, Map<String, Group> groupNames) {
            //Emit group definition
            StringWriter writer = new StringWriter();
            Emit emit = new Emit(writer,0, 4);
            emit.indent();
            String name = crackOutName(groupNames);
            if (!name.equals(getName())) {
                mName = name;
            }
            String ctName = getName() + ".CONTENT";
            emit.down("<xsd:complexType name=\"" + ctName + "\">");
            if (!mNameGen.nameExists(ctName)) {
                mNameGen.markUsed(ctName);
            } else {
                emit.down("<xsd:annotation>");
                printJAXBCustomization(emit, mNameGen.suggestClassName(ctName));
                emit.done("</xsd:annotation>");
            }
            emit.down("<xsd:choice>");
            Iterator<Element> iter = getElements().iterator();
            StringBuffer sb1 = new StringBuffer();
            StringBuffer sb2 = new StringBuffer();
            while (iter.hasNext()) {
                emit.emit(iter.next().toXSD(sb1, sb2, groupNames));
                sb1.append(sb2.toString());
                sb2.setLength(0);
            }
            emit.done("</xsd:choice>");
            emit.done("</xsd:complexType>");
            if (!mNameGen.nameExists(getName())) {
                emit.emit("<xsd:element name=\"" + getName()
                        + "\" type=\"" + getName() + ".CONTENT\"/>");
                mNameGen.markUsed(getName());
            } else {
                emit.down("<xsd:element name=\"" + getName()
                        + "\" type=\"" + getName() + ".CONTENT\">");
                emit.down("<xsd:annotation>");
                printJAXBCustomization(emit, mNameGen.suggestClassName(getName()));
                emit.done("</xsd:annotation>");
                emit.done("</xsd:element>");
            }
            contentDefinition.append(writer.toString());
            groupDefinition.append(sb1.toString());

            //return the reference in XSD format
            sb1.setLength(0);
            sb1.append("<xsd:element ref=\"" + getName() + "\"");
            if (isOptional()) {
                sb1.append(" minOccurs=\"0\"");
            } else {
                sb1.append(" minOccurs=\"1\"");
            }
            if (isRepet()) {
                sb1.append(" maxOccurs=\"unbounded\"");
            } else {
                sb1.append(" maxOccurs=\"1\"");
            }
            sb1.append("/>");
            return sb1.toString();
        }
    }
    
    private static class Message extends Sequence {
        
        private final String mName;
        
        public Message(String name, NameGenerator nameGen) {
            super(name, name, nameGen);
            mName = name;
        }
        
        @Override
        public String getName() {
            return mName;
        }

        @Override
        public boolean isMessage() {
            return true;
        }
    }
}
