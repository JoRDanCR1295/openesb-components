/*
 * FileUtil.java
 * 
 * Created on May 11, 2007, 6:11:50 PM
 * 
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */
package com.sun.jbi.filebc.util;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;

import javax.jbi.messaging.MessageExchange;

import com.sun.jbi.internationalization.Messages;
import java.io.FilenameFilter;
import java.io.IOException;
import java.util.Arrays;
import java.util.Comparator;
import java.util.concurrent.atomic.AtomicInteger;

/**
 *
 * @author jfu
 */
public class FileUtil {

    private static final Messages mMessages = Messages.getMessages(FileUtil.class);
    private static final String SUFFIX_ERROR_LOCALE_KEY = "error";
    private static final String ERROR_FILE_SUFFIX = mMessages.getString(SUFFIX_ERROR_LOCALE_KEY);
    public static final String DEFAULT_ERRORS_DIR_NAME = "errors";

    private FileUtil() {}

    public static String getErrorFileSuffix() {
        return ERROR_FILE_SUFFIX;
    }

    public static void tag(File file, String tag) throws Exception {
        String to = file.getAbsolutePath() + tag;
        file.renameTo(new File(to));
    }

    /**
     * Writes MessageExchange error details to a text file at the given path.
     * 
     * @param exchange
     * @param errFilePath
     * @throws Exception
     */
    public static void createErrorFile(String errFilePath, MessageExchange exchange) throws Exception {

        File errFile = new File(errFilePath);
        FileWriter fWriter = new FileWriter(errFile);
        PrintWriter out = new PrintWriter(new BufferedWriter(fWriter));
        String serviceName = (exchange.getEndpoint() == null) ? "" : ((exchange.getEndpoint().getServiceName() == null) ? "" : exchange.getEndpoint().getServiceName().toString());
        String endpointName = (exchange.getEndpoint() == null) ? "" : ((exchange.getEndpoint().getEndpointName() == null) ? "" : exchange.getEndpoint().getEndpointName().toString());

        out.println("Service: " + serviceName);
        out.println("Endpoint: " + endpointName);
        out.println("Operation: " + exchange.getOperation());
        out.println("faultcode: " + exchange.getProperty(MessageUtil.PROP_FAULTCODE));
        out.println("faultactor: " + exchange.getProperty(MessageUtil.PROP_FAULTACTOR));
        out.println("faultstring: " + exchange.getProperty(MessageUtil.PROP_FAULTSTRING));
        out.println("faultdetail: " + exchange.getProperty(MessageUtil.PROP_FAULTDETAIL));
        out.println("Error: " + exchange.getError());

        out.flush();
        out.close();
    }

    /**
     * Writes the given error message and throwable to a file at the given path.
     *  
     * @param errFilePath
     * @param errMsg
     * @param t
     * @throws Exception
     */
    public static void createErrorFile(String errFilePath, String errMsg, Throwable t) throws Exception {

        File errFile = new File(errFilePath);
        FileWriter fWriter = new FileWriter(errFile);
        PrintWriter out = new PrintWriter(new BufferedWriter(fWriter));

        out.println("Error Details: " + errMsg);
        if (t != null) {
            out.println("Cause: " + t);
        }
        out.flush();
        out.close();
    }

    /**
     * helper
     * @param dir - the directory where the concerned file will be written to
     * @return the path represents the concerned file
     */
    public static String fabricateFilePath(File dir, String subDir, String fileName) throws IOException {
        StringBuffer sb = new StringBuffer();
        sb.append(dir.getCanonicalPath()).append(File.separator).append(subDir).append(File.separator).append(fileName);
        return sb.toString();
    }

    /**
     * Helper to extract first N flat files within the given root directory <code>rootDir</code>
     *
     * @param rootDir - the base directory where directory listing is performed
     * @param fileName - the file name (can be literal/pattern/regex)
     * @param fileFilter - file name filter using fileName as pattern / regex, is null when fileName indicates a literal
     * @param dirFilter - dir filter, is null when recursive = false
     * @param maxFiles - when matched files reach this limit, return to the caller
     * @param recursive - if file matching goes into sub-dirs
     * @return an array of java.io.File objects which matched the filtering criteria
     * the result can be null or empty meaning there is no matched files or
     * the target does not exists yet.
     *
     * Note, when recursive = true, a width first scan is used, i.e.
     * process those flat files at the current level first, when there is not
     * enough flat files at current level, dive into sub directories...
     *
     * this algorithm favors those files close to the base directory.
     *
     */
    public static File[] extractFiles(File rootDir, String fileName, FilenameFilter fileFilter, FilenameFilter dirFilter, AtomicInteger maxFiles, int maxCC) {
        File[] files = null;
        // list flat files at base directory level
        if (fileFilter != null) {
            files = rootDir.listFiles(fileFilter);
            //Modified by Vishnu/SOLDEVILA Fabien
        	//To process the files sequentially
            Arrays.sort(files, new FileUtil.FileComparator());
        } else {
            // with the fix to open esb issue #2359
            // the extractFiles() is always invoked with fileFilter != null;
            //
            // no file name filter - looking for literal file names
            // under the root dir
            File f = new File(rootDir, fileName);
            if ( f.exists() && f.isFile()) {
                files = new File[1];
                files[0] = f;
            }
        }

        if (dirFilter != null && maxFiles.intValue() > 0) {
            // looking into sub dirs
            File[] dirs = rootDir.listFiles(dirFilter);
            Arrays.sort(dirs, new FileUtil.FileComparator());
            for (int i = 0; i < dirs.length; i++) {
                File[] subfiles = extractFiles(dirs[i], fileName, fileFilter, dirFilter, maxFiles,maxCC);
                if (subfiles != null && subfiles.length > 0) {
                    if (files != null && files.length > 0) {
                        File[] tmp = new File[files.length + subfiles.length];
                        System.arraycopy(files, 0, tmp, 0, files.length);
                        System.arraycopy(subfiles, 0, tmp, files.length, subfiles.length);
                        files = tmp;
                    } else {
                        files = subfiles;
                    }
                    if (maxFiles.intValue() <= 0) {
                        break;
                    }
                }
            }
        }
        
        
        if(files != null && files.length > 0 && maxCC > 0){
        	
        	File[] f = new File[files.length > maxCC ? maxCC : files.length];
        	System.arraycopy(files, 0, f, 0, f.length);
        	return f;
    	}else{
    		return files;
    	}
        
    }
    
    public static class FileComparator implements Comparator<File>{

    	public int compare(File f1, File f2)
        {
            int returnCompare = Long.valueOf(f1.lastModified()).compareTo(f2.lastModified());
            if (returnCompare==0) return f1.getName().compareTo(f2.getName());
            else return returnCompare;
        }
    	
    }
}
