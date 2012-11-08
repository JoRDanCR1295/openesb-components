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
 * @(#)BPELSEDeployerUtil.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel;
import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Statement;
import java.sql.Timestamp;

import javax.sql.rowset.serial.SerialBlob;

/**
 *
 * @author nnahata
 */
public class BPELSEDeployerUtil {
            
    /**
     * Creates a new instance of BPELSEDeployerUtil
     */
    public BPELSEDeployerUtil() {
    }
    
    /**
     * Store a service unit in database.
     * Creates a zip archive of the contents of the folder specified by suPath parameter. Then 
     * stores the zip file as a BLOB in the database.
     * 
     * @param dbcon java.sql.Connection, connection to the Database 
     * @param suName String, service unit name
     * @param suPath String, path to the folder which contains the service unit contents
     */
    public static void storeSUToDB(Connection dbcon, String suName, String suPath) throws Exception {
        
      	//first remove any existing data for the same su
    	deleteSUFromDB(dbcon, suName);
    
        String srcFolder = suPath;

        java.io.File f = new java.io.File(suPath);
        String parentPath = f.getParent();
        String destFolderName = parentPath + File.separator + "Temp";
        File destFolder = new File(destFolderName);
        if ( !destFolder.exists() ){
            destFolder.mkdirs();
        }
        
        String zipFileName =  suName + ".zip";
        String zipFilePath = createZip(srcFolder, destFolderName, zipFileName);
        
        storeZipBlob(dbcon, suName, zipFilePath);
        
        File zipFile = new File(zipFilePath);
        zipFile.delete();
        destFolder.delete();
        
    }
    
    public static void deleteSUFromDB(Connection con, String suName) throws Exception {
       //first remove any existing data for the same su
    	Statement stmt = null;
    	try{
            String delStmt = "DELETE from SERVICEUNIT where " +
                    "SUNAME = '" + suName + "'";
             stmt = con.createStatement();
             stmt.execute( delStmt );
    	} finally {
    		if (stmt != null) {
    			stmt.close();
    		}
    	}
    }
        
    private static String createZip(String srcFolder, String destFolder, String zipFileName){
        String zipFilePath = ZipUtil.zipFolder(srcFolder, destFolder, zipFileName);    
        return zipFilePath;
    }
    
    private static void storeZipBlob(Connection con, String suName, String zipFilePath) 
    	throws Exception {  
    	PreparedStatement pStmt = null;
    	try{
            String strStmt = "INSERT INTO SERVICEUNIT (SUNAME, SUZIPARCHIVE, LASTUPDATETIME) " +
                    "values (?, ?, ?)";
            File zipFile = new File(zipFilePath);
            byte[] bytes = getByteArray(zipFile);
            //SerialBlob sBlob = new SerialBlob(bytes);
            pStmt = con.prepareStatement(strStmt);
            pStmt.setString(1, suName);
            //setBlob() doesn't work on oracle. It causes a cast exception.
            //use setBytes() instead.
            //pStmt.setBlob(2, sBlob);
            pStmt.setBytes(2, bytes);
            pStmt.setTimestamp(3, new Timestamp(System.currentTimeMillis()));
            pStmt.executeUpdate();
        } finally {
        	pStmt.close();
        }
    }
    
    private static byte[] getByteArray(File zipFile) {
        InputStream is = null;
        byte[] byteArray = null;
        try{
            long zipSize= zipFile.length();
            if (zipSize > Integer.MAX_VALUE) {
                // File is too large
                throw new Exception("Could not create byte[] for SU BLOB. zipFile size > Integer.MAX_VALUE");
            }
            byteArray = new byte[(int)zipSize];
            is = new BufferedInputStream(new FileInputStream(zipFile));
            int len = is.read(byteArray);
        } catch (Exception e){
            //e.printStackTrace();
        } finally {
            try{
                if (is != null){
                    is.close();
                }
            } catch (Exception e){}
                        
        }
        return byteArray;
    } 
}
