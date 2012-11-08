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
 * @(#)BaseGenerator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.hl7.xsdbuilder;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.Set;

import com.sun.encoder.codegen.Emit;

public abstract class BaseGenerator implements XSDGenerator {

    public static final String COMPLEXDATATYPES_SQL =
        "com/sun/encoder/hl7/xsdbuilder/complexdatatypes.sql";
    
    /**
     * Holds the names of all complex content data types 
     */
    protected final Set<String> mCompTypeSet = new HashSet<String>();
    
    /**
     * DB connection to the HL7 MS Access database
     */
    protected final Connection mConn;
    
    /**
     * Target location on the file system where generated file(s) should go
     */
    protected final File mTargetLocation;
    
    /**
     * HL7 verison
     */
    protected final String mHL7Version;
    
    /**
     * Holds the data type names that allow escaping
     */
    protected final Set<String> mAllowEscapeSet = new HashSet<String>();
    
    /**
     * Holds the data type names that are of variant data type
     */
    protected final Set<String> mVariantDataTypes = new HashSet<String>();
    
    /**
     * Constructs from a DB connection, an HL7 version and a target location
     * for generated files.
     * 
     * @param conn the DB connection
     * @param version the HL7 version
     * @param targetLocation the target location for generated files
     */
    BaseGenerator(Connection conn, String version, File targetLocation) {
        mConn = conn;
        mHL7Version = version;
        mTargetLocation = targetLocation;
        mAllowEscapeSet.add("FT");
        mAllowEscapeSet.add("TX");
        mVariantDataTypes.add("*");
        mVariantDataTypes.add("varies");
        mVariantDataTypes.add("var");
    }

    protected String getSQLStmt(String location) throws IOException {

        Reader reader = null;
        try {
            reader =
                new InputStreamReader(
                    getClazzLoader().getResourceAsStream(location), "UTF-8");
            char[] readBlock = new char[512]; 
            StringBuffer buffer = new StringBuffer();
            int count;
            while ((count = reader.read(readBlock)) >= 0) {
                buffer.append(readBlock, 0, count);
            }
            return buffer.toString();
        } finally {
            if (reader != null) {
                reader.close();
            }
        }
    }
    
    protected ClassLoader getClazzLoader() {
        ClassLoader cl = this.getClass().getClassLoader();
        if (cl != null) {
            return cl;
        }
        //This step should never be reached unless this class
        //is included in JDK
        return ClassLoader.getSystemClassLoader();
    }
    
    protected void moveFile(File source, File dest)
            throws IOException {
        boolean isRenamed = source.renameTo(dest);
        if (isRenamed) {
            return;
        }
        InputStream in = null;
        OutputStream out = null;
        try {
            in = new FileInputStream(source);
            out = new FileOutputStream(dest);
            int count;
            byte[] buffer = new byte[1024];
            while ((count = in.read(buffer)) >= 0) {
                out.write(buffer, 0, count);
            }
            out.flush();
        } finally {
            if (out != null) {
                try {
                    out.close();
                } catch (IOException e) {
                    if (in != null) {
                        in.close();
                    }
                }
            }
            if (in != null) {
                in.close();
            }
            source.delete();
        }
    }
    
    protected void concatFile(File source, File dest)
            throws IOException {
        InputStream in = null;
        OutputStream out = null;
        try {
            in = new FileInputStream(source);
            out = new FileOutputStream(dest, true);
            int count;
            byte[] buffer = new byte[1024];
            while ((count = in.read(buffer)) >= 0) {
                out.write(buffer, 0, count);
            }
            out.flush();
        } finally {
            if (out != null) {
                try {
                    out.close();
                } catch (IOException e) {
                    if (in != null) {
                        in.close();
                    }
                }
            }
            if (in != null) {
                in.close();
            }
            source.delete();
        }
    }
    
    protected void populateComplexDataTypes() throws IOException, SQLException {
        
        PreparedStatement pstmt = null;
        ResultSet rs = null;
        try {
            String sql = getSQLStmt(COMPLEXDATATYPES_SQL);
            pstmt =
                mConn.prepareStatement(
                        sql,
                        ResultSet.TYPE_SCROLL_INSENSITIVE,
                        ResultSet.CONCUR_READ_ONLY);
            pstmt.setString(1, mHL7Version);
            rs = pstmt.executeQuery();
            
            mCompTypeSet.clear();
            rs.beforeFirst();
            while (rs.next()) {
                mCompTypeSet.add(rs.getString("data_structure"));
            }
        } finally {
            SQLException expRet = null;
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
            if (expRet != null) {
                throw expRet;
            }
        }
    }
    
    protected String encData(String pcdata) {
        return pcdata.replace("&", "&amp;").replace(
                "'", "&apos;").replace("\"", "&quot;").replace(
                        "<", "&lt;").replace(">", "&gt;");
    }
    
    public static void printJAXBCustomization(Emit emit, String className) {
        emit.down("<xsd:appinfo source=\"http://java.sun.com/xml/ns/jaxb\">");
        emit.emit("<jaxb:class name=\"" + className + "\"/>");
        emit.done("</xsd:appinfo>");
    }
}
