package it.imolinfo.jbi4corba.test.config;

import java.io.File;

/**
 * Helper class for the tests file operations.
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a> 
 */
public final class FileHelper {
	
    /**
     * recursively removes all the directory
     * @param dir
     * @return
     */
    public static boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
                boolean success = deleteDir(new File(dir, children[i]));
                if (!success) {
                    return false;
                }
            }
        }    
        // The directory is now empty so delete it
        return dir.delete();
    }  
    
    /**
     * recursively removes all the directory
     * @param path
     * @return
     */
    public static boolean deleteDir(String path) {
    	File dir = new File(path);
    	return deleteDir(dir);        
    }      
          

}
