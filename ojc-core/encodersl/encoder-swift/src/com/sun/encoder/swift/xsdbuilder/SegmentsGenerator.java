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
 * @(#)SegmentsGenerator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.swift.xsdbuilder;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Writer;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sun.encoder.codegen.Emit;
import com.sun.encoder.util.UnicodeFile;

/**
 * This is the generator that generates the fields.xsd.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class SegmentsGenerator extends BaseGenerator implements XSDGenerator {

    /**
     * The location of the resource that contains the SQL statement that
     * retrieves all segments
     */
    public static final String SEGMENTS_SQL =
        "com/sun/encoder/hl7/xsdbuilder/segments.sql";
    
    public static final String SEGMENTFIELDS_SQL =
        "com/sun/encoder/hl7/xsdbuilder/segmentfields.sql";
    
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
    SegmentsGenerator(Connection conn, String version,
            File targetLocation, NameGenerator nameGen) {
        super(conn, version, targetLocation);
        mNameGen = nameGen;
    }

    public void generate() throws GeneratorException {
        String sqlAllSegments;
        PreparedStatement pstmtAllSegments = null;
        String sqlOneSegment;
        PreparedStatement pstmtOneSegment = null;
        ResultSet rs = null;
        Emit emit = null;
        try {
            emit = new Emit(getWriter(), 0, 4);
            printHeader(emit);
            sqlAllSegments = getSQLStmt(SEGMENTS_SQL);
            pstmtAllSegments =
                mConn.prepareStatement(
                        sqlAllSegments,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            pstmtAllSegments.setString(1, mHL7Version);
            rs = pstmtAllSegments.executeQuery();
            
            sqlOneSegment = getSQLStmt(SEGMENTFIELDS_SQL);
            pstmtOneSegment =
                mConn.prepareStatement(
                        sqlOneSegment,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            
            List<String> allSegments = new ArrayList<String>();
            
            rs.beforeFirst();
            while (rs.next()) {
                String seg_code = rs.getString("seg_code");
                System.out.println(seg_code);
                if (printOneSegment(pstmtOneSegment, emit, seg_code)) {
                    allSegments.add(seg_code);
                }
            }
            printFooter(emit, allSegments);
            
            emit.flush();
            emit.close();
            emit = null;
            
            moveFile(getTempTargetFile(), getTargetFile());
        } catch (IOException e) {
            throw new GeneratorException(e);
        } catch (SQLException e) {
            throw new GeneratorException(e);
        } finally {
            Exception expRet = null;
            if (rs != null) {
                try {
                    rs.close();
                } catch (SQLException e) {
                    expRet = e;
                }
            }
            if (pstmtAllSegments != null) {
                try {
                    pstmtAllSegments.close();
                } catch (SQLException e) {
                    expRet = e;
                }
            }
            if (pstmtOneSegment != null) {
                try {
                    pstmtOneSegment.close();
                } catch (SQLException e) {
                    expRet = e;
                }
            }
            getTempTargetFile().delete();
            if (expRet != null) {
                throw new GeneratorException(expRet);
            }
        }
    }

    private void printHeader(Emit emit) {
        emit.emit("<?xml version =\"1.0\" encoding=\"UTF-8\"?>");
        emit.down("<!--");
        emit.emit("v2.xml Message Definitions Version v"
                + mHL7Version + "  - segments");
        emit.emit("Copyright (C) Sun Microsystems. All rights reserved.");
        emit.done("-->");
        emit.down("<xsd:schema");
        emit.emit("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"");
        emit.emit("xmlns=\"urn:hl7-org:v2xml\"");
        emit.emit("xmlns:hl7=\"urn:com.sun:encoder-hl7-1.0\"");
        emit.emit("targetNamespace=\"urn:hl7-org:v2xml\" xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\" jaxb:version=\"2.0\">");
        emit.emit();
        emit.emit("<!-- include fields definitions for version v"
                + mHL7Version + " -->");
        emit.emit("<xsd:include schemaLocation=\"fields.xsd\"/>");
        emit.emit();
        emit.down("<xsd:annotation>");
        emit.down("<xsd:appinfo source=\"urn:com.sun:encoder\">");
        emit.emit("<encoding xmlns=\"urn:com.sun:encoder\" name=\"HL7 v2 Encoding\" namespace=\"urn:com.sun:encoder-hl7-1.0\" style=\"hl7encoder-1.0\"/>");
        emit.done("</xsd:appinfo>");
        emit.done("</xsd:annotation>");
        emit.emit();
    }
    
    private void printFooter(Emit emit, List<String> allSegments) {
        emit.emit("<!-- .. all HL7 segment definitions -->");
        emit.down("<xsd:complexType name=\"anyHL7Segment.TYPE\">");
        emit.down("<xsd:choice>");
        Iterator<String> iter = allSegments.iterator();
        while (iter.hasNext()) {
            emit.emit("<xsd:element ref=\"" + iter.next()
                    + "\" minOccurs=\"0\" maxOccurs=\"unbounded\"/>");
        }
        emit.done("</xsd:choice>");
        emit.done("</xsd:complexType>");
        emit.emit("<xsd:element name=\"anyHL7Segment\""
                + " type=\"anyHL7Segment.TYPE\"/>");
        emit.emit("<!-- .. any Z segment definition -->");
        emit.down("<xsd:complexType name=\"anyZSegment.TYPE\">");
        emit.down("<xsd:sequence>");
        emit.emit("<xsd:any processContents=\"lax\" namespace=\"##any\"/>");
        emit.done("</xsd:sequence>");
        emit.done("</xsd:complexType>");
        emit.emit("<xsd:element name=\"anyZSegment\""
                + " type=\"anyZSegment.TYPE\"/>");
        emit.done("</xsd:schema>");
    }
    
    private boolean printOneSegment(PreparedStatement pstmt, Emit emit,
            String seg_code) throws SQLException {
        pstmt.setString(1, mHL7Version);
        pstmt.setString(2, seg_code);
        ResultSet rs = null;
        try {
            StringBuffer sb = new StringBuffer();
            rs = pstmt.executeQuery();
            rs.beforeFirst();
            boolean printStart = false;
            int seq_no = 1;
            while (rs.next()) {
                if (!printStart) {
                    emit.down("<!--");
                    emit.emit("SEGMENT " + seg_code);
                    emit.done("-->");
                    
                    sb.setLength(0);
                    String ctName = seg_code + ".CONTENT";
                    emit.down(
                            sb.append("<xsd:complexType name=\"").append(
                                    ctName).append("\">").toString());
                    if (!mNameGen.nameExists(ctName)) {
                        mNameGen.markUsed(ctName);
                    } else {
                        emit.down("<xsd:annotation>");
                        printJAXBCustomization(emit, mNameGen.suggestClassName(ctName));
                        emit.done("</xsd:annotation>");
                    }
                    emit.down("<xsd:sequence>");
                    printStart = true;
                }
                String elemName = seg_code + "." + seq_no;
                seq_no++;
                String minOccurs = "R".equals(rs.getString("req_opt")) ?
                                        "1" : "0";
                String maxOccurs = "Y".equals(rs.getString("repetitional")) ?
                                        "unbounded" : "1";
                sb.setLength(0);
                emit.emit(
                        sb.append("<xsd:element ref=\"").append(
                        elemName).append(
                        "\" minOccurs=\"" + minOccurs + "\" maxOccurs=\""
                        + maxOccurs + "\"/>").toString());
            }
            if (printStart) {
                emit.emit("<xsd:any processContents=\"lax\""
                        + " namespace=\"##other\" minOccurs=\"0\"/>");
                emit.done("</xsd:sequence>");
                emit.done("</xsd:complexType>");
                if (!mNameGen.nameExists(seg_code)) {
                    emit.emit("<xsd:element name=\"" + seg_code + "\" type=\""
                            + seg_code + ".CONTENT\"/>");
                    mNameGen.markUsed(seg_code);
                } else {
                    emit.down("<xsd:element name=\"" + seg_code + "\" type=\""
                            + seg_code + ".CONTENT\">");
                    emit.down("<xsd:annotation>");
                    printJAXBCustomization(emit, mNameGen.suggestClassName(seg_code));
                    emit.done("</xsd:annotation>");
                    emit.done("</xsd:element>");
                }
                return true;
            }
        } finally {
            if (rs != null) {
                rs.close();
            }
        }
        return false;
    }
    
    private Writer getWriter() throws FileNotFoundException, IOException {
        return UnicodeFile.makeOutputWriter(getTempTargetFile());
    }
    
    private File getTempTargetFile() {
        return new File(mTargetLocation, "segments.xsd.~temp");
    }
    
    private File getTargetFile() {
        return new File(mTargetLocation, "segments.xsd");
    }
}
