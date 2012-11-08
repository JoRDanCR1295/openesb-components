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
 * @(#)FieldsGenerator.java 
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
import java.text.DecimalFormat;

import com.sun.encoder.codegen.Emit;
import com.sun.encoder.util.UnicodeFile;

/**
 * This is the generator that generates the fields.xsd.
 * 
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class FieldsGenerator extends BaseGenerator implements XSDGenerator {

    /**
     * The location of the resource that contains the SQL statement that
     * retrieves all fields
     */
    public static final String FIELDS_SQL =
        "com/sun/encoder/hl7/xsdbuilder/fields.sql";
    
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
    FieldsGenerator(Connection conn, String version,
            File targetLocation, NameGenerator nameGen) {
        super(conn, version, targetLocation);
        mNameGen = nameGen;
    }

    public void generate() throws GeneratorException {
        String sql;
        PreparedStatement pstmt = null;
        ResultSet rs = null;
        Emit emit = null;
        try {
            populateComplexDataTypes();
            
            emit = new Emit(getWriter(), 0, 4);
            DecimalFormat tableNmFormatter = new DecimalFormat("0000");
            printHeader(emit);
            sql = getSQLStmt(FIELDS_SQL);
            pstmt =
                mConn.prepareStatement(
                        sql,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            pstmt.setString(1, mHL7Version);
            rs = pstmt.executeQuery();
            rs.beforeFirst();
            String lastSecCode = "";
            String secCode;
            int seq_no = 1;
            while (rs.next()) {
                secCode = rs.getString("seg_code"); 
                if (!secCode.equals(lastSecCode)) {
                    seq_no = 1;
                }
                String fieldName =
                     secCode + "." + seq_no;
                seq_no++;
                System.out.println(fieldName);
                String tableId = null;
                if (rs.getInt("table_id") != 0) {
                    tableId = "HL7" + tableNmFormatter.format(
                            rs.getInt("table_id"));
                }
                String dataStruct = rs.getString("data_structure");
                String dataType = rs.getString("data_type_code");
                boolean isElementary = rs.getBoolean("elementary");
                if (isElementary) {
                    if (mAllowEscapeSet.contains(dataType)
                            || mVariantDataTypes.contains(dataType)) {
                        isElementary = false;
                    }
                } else {
                    if (!mCompTypeSet.contains(dataStruct)
                            && !mCompTypeSet.contains(dataType)
                            && !mAllowEscapeSet.contains(dataType)
                            && !mVariantDataTypes.contains(dataType)) {
                        isElementary = true;
                    }
                }
                printOneField(emit, fieldName,
                        rs.getInt("data_item"), rs.getString("description"),
                        dataStruct, dataType, tableId, isElementary);
                lastSecCode = secCode;
            }
            printFooter(emit);
            
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
            if (pstmt != null) {
                try {
                    pstmt.close();
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
                + mHL7Version + "  - fields");
        emit.emit("Copyright (C) Sun Microsystems. All rights reserved.");
        emit.done("-->");
        emit.down("<xsd:schema");
        emit.emit("xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"");
        emit.emit("xmlns=\"urn:hl7-org:v2xml\"");
        emit.emit("xmlns:hl7=\"urn:com.sun:encoder-hl7-1.0\"");
        emit.emit("targetNamespace=\"urn:hl7-org:v2xml\" xmlns:jaxb=\"http://java.sun.com/xml/ns/jaxb\" jaxb:version=\"2.0\">");
        emit.emit();
        emit.emit("<!-- include datatypes definitions for version v"
                + mHL7Version + " -->");
        emit.emit("<xsd:include schemaLocation=\"datatypes.xsd\"/>");
        emit.emit();
        emit.down("<xsd:annotation>");
        emit.down("<xsd:appinfo source=\"urn:com.sun:encoder\">");
        emit.emit("<encoding xmlns=\"urn:com.sun:encoder\" name=\"HL7 v2 Encoding\" namespace=\"urn:com.sun:encoder-hl7-1.0\" style=\"hl7encoder-1.0\"/>");
        emit.done("</xsd:appinfo>");
        emit.done("</xsd:annotation>");
        emit.emit();
    }
    
    private void printFooter(Emit emit) {
        emit.done("</xsd:schema>");
    }
    
    private void printOneField(Emit emit, String fieldName, int dataItem,
            String desc, String dataStruct, String dataType, String tableId,
            boolean isElementary) {
        emit.down("<!--");
        emit.emit("FIELD " + fieldName);
        emit.done("-->");
        emit.down("<xsd:attributeGroup name=\"" + fieldName + ".ATTRIBUTES\">");
        emit.emit("<xsd:attribute name=\"Item\" type=\"xsd:string\" fixed=\""
                + dataItem + "\"/>");
        emit.emit("<xsd:attribute name=\"Type\" type=\"xsd:string\" fixed=\""
                + dataType + "\"/>");
        if (tableId != null) {
            emit.emit("<xsd:attribute name=\"Table\""
                    + " type=\"xsd:string\" fixed=\"" + tableId + "\"/>");
        }
        emit.emit("<xsd:attribute name=\"LongName\""
                + " type=\"xsd:string\" fixed=\"" + encData(desc) + "\"/>");
        emit.done("</xsd:attributeGroup>");
        String ctName = fieldName + ".CONTENT";
        emit.down("<xsd:complexType name=\"" + ctName + "\">");
        emit.down("<xsd:annotation>");
        emit.emit("<xsd:documentation xml:lang=\"en\">"
                + encData(desc) + "</xsd:documentation>");
        if (!mNameGen.nameExists(ctName)) {
            mNameGen.markUsed(ctName);
        } else {
            printJAXBCustomization(emit, mNameGen.suggestClassName(ctName));
        }
        emit.down("<xsd:appinfo source=\"urn:com.sun:encoder\">");
        emit.emit("<hl7:Item>" + dataItem + "</hl7:Item>");
        emit.emit("<hl7:Type>" + dataType + "</hl7:Type>");
        if (tableId != null) {
            emit.emit("<hl7:Table>" + tableId + "</hl7:Table>");
        }
        emit.emit("<hl7:LongName>" + encData(desc) + "</hl7:LongName>");
        emit.done("</xsd:appinfo>");
        emit.done("</xsd:annotation>");
        if (isElementary) {
            emit.down("<xsd:simpleContent>");
        } else {
            emit.down("<xsd:complexContent>");
        }
        if (dataStruct.startsWith("CE_")) {
            emit.down("<xsd:extension base=\"" + dataType + "\">");
        } else {
            emit.down("<xsd:extension base=\"" + dataStruct + "\">");
        }
        emit.emit("<xsd:attributeGroup ref=\"" + fieldName + ".ATTRIBUTES\"/>");
        emit.done("</xsd:extension>");
        if (isElementary) {
            emit.done("</xsd:simpleContent>");
        } else {
            emit.done("</xsd:complexContent>");
        }
        emit.done("</xsd:complexType>");
        if (!mNameGen.nameExists(fieldName)) {
            emit.emit("<xsd:element name=\"" + fieldName + "\" type=\""
                    + fieldName + ".CONTENT\"/>");
            mNameGen.markUsed(fieldName);
        } else {
            emit.down("<xsd:element name=\"" + fieldName + "\" type=\""
                    + fieldName + ".CONTENT\">");
            emit.down("<xsd:annotation>");
            printJAXBCustomization(emit, mNameGen.suggestClassName(fieldName));
            emit.done("</xsd:annotation>");
            emit.done("</xsd:element>");
        }
    }
    
    private Writer getWriter() throws FileNotFoundException, IOException {
        return UnicodeFile.makeOutputWriter(getTempTargetFile());
    }
    
    private File getTempTargetFile() {
        return new File(mTargetLocation, "fields.xsd.~temp");
    }
    
    private File getTargetFile() {
        return new File(mTargetLocation, "fields.xsd");
    }
}
