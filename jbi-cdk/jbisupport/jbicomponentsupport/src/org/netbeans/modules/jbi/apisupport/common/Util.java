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

import java.awt.Component;
import java.awt.Container;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.Repository;

/**
 *
 * @author  chikkala
 */
public class Util {
    public static final boolean disablePartialEditors = true;
    
    private static final String JBI_SUPPORT_FOLDER_PATH = "/JBISupport";
    private static FileObject sJbiSupportFolder;
    
    private static Logger sLogger;
    private static final boolean DEBUG = false;
    /** Creates a new instance of AUBuildScript */
    public Util() {
    }
    
    public static Logger getLogger() {
        
        if ( sLogger != null ) {
            sLogger = Logger.getLogger(Util.class.getPackage().getName(), null);
        }
        return sLogger;
    }
    
    public static void logInfo(String msg) {
        if ( DEBUG ) {
            System.out.println(msg);
        }
//        getLogger().info(msg);
    }
    
    public static void logVerbose(String msg) {
        if ( DEBUG ) {
            System.out.println(msg);
        }
        
//        getLogger().fine(msg);
        
    }
    
    public static void logWarning(Object logObj) {
        if ( DEBUG ) {
            System.out.println(logObj);
        }
        
//        if ( logObj instanceof Throwable) {
//            getLogger().log(Level.WARNING, ((Throwable)logObj).getMessage(), (Throwable)logObj);
//        } else {
//            getLogger().warning(logObj.toString());
//        }
    }
    
    public static void logError(Object logObj) {
        
        if ( DEBUG ) {
            System.out.println(logObj);
        }
        
//        if ( logObj instanceof Throwable) {
//            getLogger().log(Level.SEVERE, ((Throwable)logObj).getMessage(), (Throwable)logObj);
//        } else {
//            getLogger().severe(logObj.toString());
//        }
    }
    
    public static void logDebug(Object logObj) {
        
        if ( DEBUG ) {
            System.out.println(logObj);
        }
        
//        if ( logObj instanceof Throwable) {
//            getLogger().log(Level.FINER, ((Throwable)logObj).getMessage(), (Throwable)logObj);
//        } else {
//            getLogger().finer(logObj.toString());
//        }
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
    
    public static FileObject getJBISupportFolder() {
        if ( sJbiSupportFolder != null  ) {
            return sJbiSupportFolder; }
        FileObject fo =
                Repository.getDefault()
                .getDefaultFileSystem().findResource(JBI_SUPPORT_FOLDER_PATH); // NOI18N
        if (fo != null && fo.isFolder()) {
            sJbiSupportFolder = fo;
        }
        return sJbiSupportFolder;
    }
    
    public static String[] getLibraryList(String classPath) {
        String[] paths = PropertyUtils.tokenizePath(classPath);
        if ( paths == null ) {
            return new String[0];
        }
        ArrayList libList = new ArrayList();
        for (int i=0; i < paths.length; ++i ) {
            String libName = (new File(paths[i])).getName();
            if ( libName.length() == 0 || ".".equals(libName) || "..".equals(libName) ) {
                //skip empty, . or ..
                continue;
            }
            libList.add(libName);
        }
        return (String[])libList.toArray(new String[libList.size()]);
    }
    
    public static void unZipArchive(InputStream source, FileObject projectRoot) throws IOException {
        try {
            ZipInputStream str = new ZipInputStream(source);
            ZipEntry entry;
            while ((entry = str.getNextEntry()) != null) {
                if (entry.isDirectory()) {
                    FileUtil.createFolder(projectRoot, entry.getName());
                } else {
                    FileObject fo = FileUtil.createData(projectRoot, entry.getName());
                    FileLock lock = fo.lock();
                    try {
                        OutputStream out = fo.getOutputStream(lock);
                        try {
                            FileUtil.copy(str, out);
                        } finally {
                            out.close();
                        }
                    } finally {
                        lock.releaseLock();
                    }
                }
            }
        } finally {
            try {
                source.close();
            } catch (IOException ex) {
//                ex.printStackTrace();
            }
        }
    }
    
    public static void setEnabledRecursively(Container root, boolean enabled) {
        root.setEnabled(enabled);
        for (Component c : root.getComponents()) {
            if (c instanceof Container) {
                setEnabledRecursively((Container) c, enabled);
            }
            else {
                c.setEnabled(enabled);
            }
        }
    }
    
    public static void copyFile(FileObject destFO, URL sourceURL) {
        
        FileLock outLock = null;
        OutputStream outS = null;
        InputStream inS = null;
        
        try {
            inS = sourceURL.openStream();
            outLock = destFO.lock();
            outS = destFO.getOutputStream(outLock);
            FileUtil.copy(inS, outS);
        } catch ( Exception ex) {
            ex.printStackTrace();
        } finally {
            closeInputStream(inS);
            closeOutputStream(outS);
            if ( outLock != null ) {
                outLock.releaseLock();
            }
        }        
    }
    
    /**
     * use it to create jbi.xml for slib, bc, se, sa, sus
     */
    public static FileObject createJbiXmlFileFromTemplate(FileObject dirFO, String compType, Map tokenMap) {
        
        String templatePath = null;
        if ( JbiDescriptor.SERVICE_ENGINE_TYPE.equalsIgnoreCase(compType) ) {
            templatePath = "codegen/components/engine/jbi.xml";
        } else if ( JbiDescriptor.BINDING_COMPONENT_TYPE.equalsIgnoreCase(compType) ) {
            templatePath = "codegen/components/binding/jbi.xml";
        }
        
        FileObject jbiXmlFO = null;
        if ( templatePath != null ) {
            FileObject jbiSupportFolder = Util.getJBISupportFolder();
            FileObject templateFO = jbiSupportFolder.getFileObject(templatePath);
            String fileName = "jbi.xml";
            HashMap map = new HashMap();
            Map defMap = TemplateUtil.createDefaultTokenMap("jbi", "xml");
            map.putAll(defMap);
            map.putAll(tokenMap);
        
            jbiXmlFO = TemplateUtil.createFromTemplate(templateFO, map, dirFO, "jbi.xml");
        }
        return jbiXmlFO;
    }
    
    public static void deleteFile(FileObject folderFO, String name) {
        FileObject fo = folderFO.getFileObject(name);
        if ( fo != null ) {
            deleteFile(fo);
        }
    }
    
    public static void deleteFile(FileObject fo) {
        if ( fo != null ) {
            try {
            fo.delete();
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }
    }
    
    public static  void markFileObjectAsFreeMarkerTemplate(FileObject fo, boolean rec) {
        try {
        if ( rec && fo.isFolder()) {
            for(Enumeration en = fo.getData(true); en.hasMoreElements();) {
                FileObject data = (FileObject) en.nextElement();
                data.setAttribute("template", "true");
                data.setAttribute("javax.script.ScriptEngine", "freemarker");
            }            
        } else {
                fo.setAttribute("template", "true");
                fo.setAttribute("javax.script.ScriptEngine", "freemarker");
        }
        } catch (Exception ex) {
            ex.printStackTrace();
        }

//        FileObject WSDLExtValidatorJavaFO = zipValidatorFO.getFileObject("WSDLExtValidator.java");
//        if ( WSDLExtValidatorJavaFO != null ) {
//            markFileObjectAsFreeMarkerTemplate(WSDLExtValidatorJavaFO, false);
//        } else {
//            System.out.println("Could not find WSDLExtValidator.java in " + zipValidatorFO.getPath());
//        }
//        Map<String, Object> tokenMap = new HashMap<String, Object>();
//        tokenMap.put("packagemodel", packageName + ".model");
//        zipValidatorFolder.createFromTemplate(DataFolder.findFolder(pkgFolder), null, tokenMap);

    }
    
    public static FileObject getArchiveRoot(FileObject zipFO) {
        FileObject zipRootFO = null;
        if ( FileUtil.isArchiveFile(zipFO)) {
            try {
            System.out.println("###@@@@ BC Plugin Archive file recognized " + zipFO.getURL());
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        } else {
            System.out.println("###@@@@ BC Plugin Archive file NOT Recognized " );
            return null;
        }

        zipRootFO = FileUtil.createMemoryFileSystem().getRoot();
        try {
            Util.unZipArchive(zipFO.getInputStream(), zipRootFO);
        } catch (Exception ex) {
            ex.printStackTrace();
            return null;
        }
        
//        // mark all files as templates
//        try {
//            for(Enumeration en = zipRootFO.getData(true); en.hasMoreElements();) {
//                FileObject fo = (FileObject) en.nextElement();
//                fo.setAttribute("template", "true");
//                fo.setAttribute("javax.script.ScriptEngine", "freemarker");
//            }
//        } catch (Exception ex) {
//            ex.printStackTrace();
//        }
        return zipRootFO;
    }
    
}
