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
 * @(#)DataTypesGenerator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.hl7.xsdbuilder;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Writer;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.DecimalFormat;

import com.sun.encoder.codegen.Emit;
import com.sun.encoder.hl7.HL7Encoder;
import com.sun.encoder.hl7.HL7EncoderProvider;
import com.sun.encoder.util.UnicodeFile;

/**
 * This is the generator that generates the datatypes.xsd.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class DataTypesGenerator extends BaseGenerator
        implements XSDGenerator {
    
    /**
     * The location of the resource that contains the SQL statement that
     * retrieves all data types
     */
    public static final String DATATYPES_SQL =
        "com/sun/encoder/hl7/xsdbuilder/datatypes.sql";
    
    public static final String DATATYPECOMPONENTS_SQL =
        "com/sun/encoder/hl7/xsdbuilder/datatypecomponents.sql";
    
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
    DataTypesGenerator(Connection conn, String version,
            File targetLocation, NameGenerator nameGen) {
        super(conn, version, targetLocation);
        mNameGen = nameGen;
    }

    public void generate() throws GeneratorException {
        String sqlAllDataTypes;
        PreparedStatement pstmtAllDataTypes = null;
        String sqlOneDataType;
        PreparedStatement pstmtOneDataType = null;
        ResultSet rs = null;
        Emit emit1 = null;
        Emit emit2 = null;
        try {
            populateComplexDataTypes();
            emit1 = new Emit(getWriter1(), 0, 4);
            emit2 = new Emit(getWriter2(), 0, 4);
            emit2.indent();
            printHeader(emit1);
            printEscapeType(emit1);
            sqlAllDataTypes = getSQLStmt(DATATYPES_SQL);
            pstmtAllDataTypes =
                mConn.prepareStatement(
                        sqlAllDataTypes,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            pstmtAllDataTypes.setString(1, mHL7Version);
            rs = pstmtAllDataTypes.executeQuery();
            
            sqlOneDataType = getSQLStmt(DATATYPECOMPONENTS_SQL);
            pstmtOneDataType =
                mConn.prepareStatement(
                        sqlOneDataType,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            
            rs.beforeFirst();
            while (rs.next()) {
                System.out.println(rs.getString("data_structure"));
                generateOneDataType(pstmtOneDataType, emit1, emit2,
                        rs.getString("data_structure"),
                        rs.getBoolean("elementary"));
            }
            printFooter(emit2);
            
            emit1.flush();
            emit1.close();
            emit1 = null;
            emit2.flush();
            emit2.close();
            emit2 = null;
            
            moveFile(getTempTargetFile1(), getTargetFile());
            concatFile(getTempTargetFile2(), getTargetFile());
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
            if (pstmtAllDataTypes != null) {
                try {
                    pstmtAllDataTypes.close();
                } catch (SQLException e) {
                    expRet = e;
                }
            }
            if (pstmtOneDataType != null) {
                try {
                    pstmtOneDataType.close();
                } catch (SQLException e) {
                    expRet = e;
                }
            }
            getTempTargetFile1().delete();
            getTempTargetFile2().delete();
            if (expRet != null) {
                throw new GeneratorException(expRet);
            }
        }
    }
    
    private void printHeader(Emit emit) {
        emit.emit("<?xml version =\"1.0\" encoding=\"UTF-8\"?>");
        emit.down("<!--");
        emit.emit("v2.xml Message Definitions Version v"
                + mHL7Version + "  - data types");
        emit.emit("HL7® Version " + mHL7Version + ", © Health Level Seven, Inc.  All rights reserved.  HL7 and Health Level Seven are registered trademarks of Health Level Seven, Inc.");
        emit.done("-->");
        emit.down("<xsd:schema");
        emit.emit("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"");
        emit.emit("xmlns=\"" + HL7Encoder.HL7_NS + "\"");
        emit.emit("xmlns:hl7=\"" + HL7Encoder.HL7_ENCODER_NAMESPACE + "\"");
        emit.emit("targetNamespace=\"" + HL7Encoder.HL7_NS
            + "\" xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\" jaxb:version=\"2.0\">");
        emit.emit();
        emit.down("<xsd:annotation>");
        emit.down("<xsd:appinfo source=\"" + HL7Encoder.ENCODER_NAMESPACE + "\">");
        emit.emit("<encoding xmlns=\"" + HL7Encoder.ENCODER_NAMESPACE
            + "\" name=\"HL7 v2 Encoding\" namespace=\"" + HL7Encoder.HL7_ENCODER_NAMESPACE
            + "\" style=\"" + HL7EncoderProvider.STYLE_ID + "\"/>");
        emit.done("</xsd:appinfo>");
        emit.done("</xsd:annotation>");
        emit.emit();
    }
    
    private void printFooter(Emit emit) {
        emit.done("</xsd:schema>");
    }
    
    private void generateOneDataType(PreparedStatement pstmt, Emit emit1,
            Emit emit2, String datatype, boolean isElementary)
            throws SQLException {
        
        if (!isElementary) {
            StringBuffer sb = new StringBuffer();
            
            pstmt.setString(1, mHL7Version);
            pstmt.setString(2, datatype);
            ResultSet rs = null;
            try {
                rs = pstmt.executeQuery();
                rs.beforeFirst();
                boolean printStart = false;
                DecimalFormat tableNmFormatter = new DecimalFormat("0000");
                int seq_no = 1;
                while (rs.next()) {
                    if (!printStart) {
                        emit1.down("<!--");
                        emit1.emit("COMPOSITE DATATYPE " + datatype);
                        emit1.done("-->");
                        sb.setLength(0);
                        emit1.down(
                                sb.append("<xsd:complexType name=\"").append(
                                datatype).append("\">").toString());
                        if (!mNameGen.nameExists(datatype)) {
                            mNameGen.markUsed(datatype);
                        } else {
                            emit1.down("<xsd:annotation>");
                            printJAXBCustomization(emit1, mNameGen.suggestClassName(datatype));
                            emit1.done("</xsd:annotation>");
                        }
                        emit1.down("<xsd:sequence>");
                        printStart = true;
                    }
                    String elemName = datatype + "." + seq_no;
                    seq_no++;
                    sb.setLength(0);
                    emit1.emit(
                            sb.append("<xsd:element ref=\"").append(
                            elemName).append(
                            "\" minOccurs=\"0\" maxOccurs=\"1\"/>").toString());
                    String tableId = null;
                    if (rs.getInt("table_id") != 0) {
                        tableId = "HL7" + tableNmFormatter.format(
                                rs.getInt("table_id"));
                    }
                    String dataType = rs.getString("data_type_code");
                    String dataStruct = rs.getString("data_structure");
                    printSubElement(emit2, elemName,
                            dataType,
                            dataStruct,
                            rs.getString("subdesc"),
                            tableId,
                            !mCompTypeSet.contains(dataStruct)
                                && !mCompTypeSet.contains(dataType)
                                && !mAllowEscapeSet.contains(dataType));
                }
                if (printStart) {
                    emit1.done("</xsd:sequence>");
                    emit1.done("</xsd:complexType>");
                    return;
                }
            } finally {
                if (rs != null) {
                    rs.close();
                }
            }
        }
        
        //must be either elementary or sub component
        printPrimType(emit1, datatype);
    }
    
    private void printPrimType(Emit emit, String dataType) {
        emit.down("<!--");
        emit.emit("PRIMITIVE DATATYPE " + dataType);
        emit.done("-->");
        if (mAllowEscapeSet.contains(dataType)) {
            emit.down("<xsd:complexType name=\"" + dataType
                    + "\" mixed=\"true\">");
            emit.down("<xsd:sequence>");
            emit.emit("<xsd:element name=\"escape\" minOccurs=\"0\""
                    + " maxOccurs=\"unbounded\" type=\"escapeType\"/>");
            emit.done("</xsd:sequence>");
            emit.done("</xsd:complexType>");
        } else if ("varies".equals(dataType)) {
            emit.down("<xsd:complexType name=\"varies\">");
            emit.down("<xsd:complexContent>");
            emit.emit("<xsd:extension base=\"xsd:anyType\"/>");
            emit.done("</xsd:complexContent>");
            emit.done("</xsd:complexType>");
        } else {
            emit.down("<xsd:simpleType name=\"" + dataType + "\">");
            emit.emit("<xsd:restriction base=\"xsd:string\"/>");
            emit.done("</xsd:simpleType>");
        }
    }
    
    private void printSubElement(Emit emit, String elemName, String dataType,
            String dataStruct, String desc, String tableId,
            boolean elementary) {
        emit.down("<!--");
        emit.emit("COMPONENT " + elemName);
        emit.done("-->");
        emit.down("<xsd:attributeGroup name=\"" + elemName + ".ATTRIBUTES\">");
        emit.emit("<xsd:attribute name=\"Type\" type=\"xsd:string\" fixed=\""
                + dataType + "\"/>");
        if (tableId != null) {
            emit.emit(
                    "<xsd:attribute name=\"Table\" type=\"xsd:string\" fixed=\""
                    + tableId + "\"/>");
        }
        emit.emit("<xsd:attribute name=\"LongName\" "
                + "type=\"xsd:string\" fixed=\""
                + encData(desc) + "\"/>");
        emit.done("</xsd:attributeGroup>");
        String ctName = elemName + ".CONTENT";
        emit.down("<xsd:complexType name=\"" + ctName + "\">");
        emit.down("<xsd:annotation>");
        emit.emit("<xsd:documentation xml:lang=\"en\">"
                + encData(desc) + "</xsd:documentation>");
        if (!mNameGen.nameExists(ctName)) {
            mNameGen.markUsed(ctName);
        } else {
            printJAXBCustomization(emit, mNameGen.suggestClassName(ctName));
        }
        emit.down("<xsd:appinfo source=\"" + HL7Encoder.ENCODER_NAMESPACE + "\">");
        emit.emit("<hl7:Type>" + dataType + "</hl7:Type>");
        if (tableId != null) {
            emit.emit("<hl7:Table>" + tableId + "</hl7:Table>");
        }
        emit.emit("<hl7:LongName>" + encData(desc) + "</hl7:LongName>");
        emit.done("</xsd:appinfo>");
        emit.done("</xsd:annotation>");
        if (elementary) {
            emit.down("<xsd:simpleContent>");
        } else {
            emit.down("<xsd:complexContent>");
        }
        if (dataStruct.startsWith("CE_")) {
            emit.down("<xsd:extension base=\"" + dataType + "\">");
        } else {
            emit.down("<xsd:extension base=\"" + dataStruct + "\">");
        }
        emit.emit("<xsd:attributeGroup ref=\"" + elemName + ".ATTRIBUTES\"/>");
        emit.done("</xsd:extension>");
        if (elementary) {
            emit.done("</xsd:simpleContent>");
        } else {
            emit.done("</xsd:complexContent>");
        }
        emit.done("</xsd:complexType>");
        if (!mNameGen.nameExists(elemName)) {
            emit.emit("<xsd:element name=\"" + elemName + "\" type=\""
                    + elemName + ".CONTENT\"/>");
            mNameGen.markUsed(elemName);
        } else {
            emit.down("<xsd:element name=\"" + elemName + "\" type=\""
                    + elemName + ".CONTENT\">");
            emit.down("<xsd:annotation>");
            printJAXBCustomization(emit, mNameGen.suggestClassName(elemName));
            emit.done("</xsd:annotation>");
            emit.done("</xsd:element>");
        }
    }
    
    private void printEscapeType(Emit emit) {
        emit.down("<xsd:complexType name=\"escapeType\">");
        emit.down("<xsd:simpleContent>");
        emit.down("<xsd:extension base=\"xsd:string\">");
        emit.emit("<xsd:attribute name=\"V\" type=\"xsd:string\"/>");
        emit.done("</xsd:extension>");
        emit.done("</xsd:simpleContent>");
        emit.done("</xsd:complexType>");
    }
    
    private Writer getWriter1() throws FileNotFoundException, IOException {
        return UnicodeFile.makeOutputWriter(getTempTargetFile1());
    }
    
    private Writer getWriter2() throws FileNotFoundException, IOException {
        return UnicodeFile.makeOutputWriter(getTempTargetFile2());
    }
    
    private File getTempTargetFile1() {
        return new File(mTargetLocation, "datatypes.xsd.~temp1");
    }
    
    private File getTempTargetFile2() {
        return new File(mTargetLocation, "datatypes.xsd.~temp2");
    }
    
    private File getTargetFile() {
        return new File(mTargetLocation, "datatypes.xsd");
    }
}
