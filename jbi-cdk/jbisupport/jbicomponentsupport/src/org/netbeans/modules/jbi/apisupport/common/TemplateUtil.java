/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 *
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */

package org.netbeans.modules.jbi.apisupport.common;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringWriter;
import java.io.Writer;
import java.text.DateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.MapFormat;

/**
 *
 * @author  chikkala
 */
public class TemplateUtil {
    
    /** Creates a new instance of AUBuildScript */
    public TemplateUtil() {
    }
    
    public static MapFormat createTokenFormatter(Map tokenMap ) {
        MapFormat formatter = new MapFormat(tokenMap);
        formatter.setExactMatch(true);
        formatter.setLeftBrace("__");
        formatter.setRightBrace("__");
        return formatter;
    }
    
    /**
     * translate the tokens in the template file and writes the the string buffer
     */
    public static Writer formatData(BufferedReader templateReader,
            MapFormat formatter, PrintWriter result ) {
        String current = null;
        try {
            
            while ((current = templateReader.readLine()) != null) {
                result.println(formatter.format(current));
            }
        } catch (Exception readIOEx ) {
            // readline exception.
            // ignore
            System.out.println("Exception processing template line \n" + current);
            readIOEx.printStackTrace();
        }
        return result;
    }
    
    /**
     * translate the tokens in the template file and writes the the string buffer
     */
    public static StringBuffer loadFromTemplate(FileObject templateFO, Map tokenMap) {
        return loadFromTemplate(templateFO, tokenMap, new StringBuffer());
    }
    
    /**
     * translate the tokens in the template file and writes the the string buffer
     */
    public static StringBuffer loadFromTemplate(FileObject templateFO,
            Map tokenMap, StringBuffer result ) {
        
        MapFormat formatter = createTokenFormatter(tokenMap);
        
        StringWriter formattedData = new StringWriter();
        
        InputStream templateIS = null;
        InputStreamReader inReader = null;
        BufferedReader reader = null;
        
        PrintWriter writer = null;
        BufferedWriter buffWriter = null;
        
        try {
            templateIS = templateFO.getInputStream();
            inReader = new InputStreamReader(templateIS);
            reader = new BufferedReader(inReader);
            
            buffWriter = new BufferedWriter(formattedData);
            writer = new PrintWriter(buffWriter);
            
            formatData(reader, formatter, writer);
            
            
        } catch (IOException foEx ) {
            // unable to create the inputstream from template fo
            // ignore
            foEx.printStackTrace();
            // can not format.
        } finally {
            closeWriter(writer);
            closeWriter(buffWriter);
            
            closeReader(reader);
            closeReader(inReader);
            closeInputStream(templateIS);
            
        }
        
        result.append(formattedData);
        return result;
    }
    
    /**
     * translate the tokens in the template file and writes to the file
     */
    public static FileObject updateFromTemplate(FileObject templateFO,
            Map tokenMap, FileObject resultFO ) {
        MapFormat formatter = createTokenFormatter(tokenMap);
        
        InputStream  tempIS = null;
        InputStreamReader isReader = null;
        BufferedReader reader = null;
        
        
        FileLock resultLock  = null;
        OutputStream resultOS = null;
        OutputStreamWriter osWriter = null;
        BufferedWriter buffWriter = null;
        PrintWriter writer = null;
        
        try {
            tempIS = templateFO.getInputStream();
            isReader = new InputStreamReader(tempIS);
            reader = new BufferedReader(isReader);
            
            resultLock = resultFO.lock();
            resultOS = resultFO.getOutputStream(resultLock);
            osWriter = new OutputStreamWriter(resultOS);
            buffWriter = new BufferedWriter(osWriter);
            writer = new PrintWriter(buffWriter);
            
            formatData(reader, formatter, writer);
            
        } catch (IOException foEx ) {
            foEx.printStackTrace();
            // can not format.
        } finally {
            closeWriter(writer);
            closeWriter(buffWriter);
            closeWriter(osWriter);
            closeOutputStream(resultOS);
            
            if ( resultLock != null ) {
                resultLock.releaseLock();
            }
            
            closeReader(reader);
            closeReader(isReader);
            closeInputStream(tempIS);
            
        }
        
        return resultFO;
        
    }
    
    public static void saveToFileObject(FileObject outFO, StringBuffer srcBuff) {
        FileLock outLock = null;
        OutputStream outS = null;
        InputStream inS = null;
        
        try {
            inS = new ByteArrayInputStream(srcBuff.toString().getBytes());
            outLock = outFO.lock();
            outS = outFO.getOutputStream(outLock);
            FileUtil.copy(inS, outS);
        } catch ( Exception ex) {
            ex.printStackTrace();
        } finally {
            TemplateUtil.closeInputStream(inS);
            TemplateUtil.closeOutputStream(outS);
            if ( outLock != null ) {
                outLock.releaseLock();
            }
        }
    }
    
    public static void replaceTokens(FileObject srcFO, Map tokenMap) {
        StringBuffer formattedBuff = loadFromTemplate(srcFO, tokenMap);
        saveToFileObject(srcFO, formattedBuff);
    }
    
    /**
     * translate the tokens in the template file and writes to the file
     */
    public static FileObject createFromTemplate(FileObject templateFO,
            Map tokenMap, FileObject folderFO ) {
        
        return createFromTemplate(templateFO, tokenMap, folderFO, templateFO.getName(), templateFO.getExt());
    }
    
    /**
     * translate the tokens in the template file and writes to the file
     */
    public static FileObject createFromTemplate(FileObject templateFO,
            Map tokenMap, FileObject folderFO, String name) {
        return createFromTemplate(templateFO, tokenMap, folderFO, name, templateFO.getExt());
    }
    
    /**
     * translate the tokens in the template file and writes to the file
     */
    public static FileObject createFromTemplate(FileObject templateFO,
            Map tokenMap, FileObject folderFO, String name, String ext) {
        
        FileObject resultFO = null;
        try {
            String fileName = name;
            if ( ext != null && ext.trim().length() > 0 ) {
                fileName += "." + ext.trim();
            }
            resultFO = FileUtil.createData(folderFO, fileName);
        } catch ( Exception ex) {
            ex.printStackTrace();
            // ignore
        }
        
        if ( resultFO != null ) {
            updateFromTemplate(templateFO, tokenMap, resultFO);
        }
        
        return resultFO;
    }
    
    
    public static Map createDefaultTokenMap(String fileName, String fileExt) {
        Map map = new HashMap();
        map.put("NAME", fileName + "." + fileExt); // NOI18N
        
        Date now = new Date();
        map.put("DATE", DateFormat.getDateInstance(DateFormat.LONG).format(now)); // NOI18N
        map.put("TIME", DateFormat.getTimeInstance(DateFormat.SHORT).format(now)); // NOI18N
        map.put("USER", System.getProperty("user.name")); // NOI18N
        return map;
    }
    
    public static void closeReader(Reader reader) {
        if ( reader == null ) {
            return;
        }
        try {
            reader.close();
        } catch (Exception ex) {
            // ingore exception
            ex.printStackTrace();
        }
    }
    
    public static void closeWriter(Writer writer) {
        if ( writer == null ) {
            return;
        }
        try {
            writer.close();
        } catch (Exception ex) {
            // ingore exception
            ex.printStackTrace();
        }
    }
    
    public static void closeInputStream(InputStream inStream) {
        if ( inStream == null ) {
            return;
        }
        try {
            inStream.close();
        } catch (Exception ex) {
            // ingore exception
            ex.printStackTrace();
        }
    }
    
    public static void closeOutputStream(OutputStream outStream ) {
        if ( outStream == null ) {
            return;
        }
        try {
            outStream.close();
        } catch (Exception ex) {
            // ingore exception
            ex.printStackTrace();
        }
    }
    
    
    
}
