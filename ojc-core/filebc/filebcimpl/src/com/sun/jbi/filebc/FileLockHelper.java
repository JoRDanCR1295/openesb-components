/**
 * deprecated
 */
package com.sun.jbi.filebc;

/**
 * @author Sujit Biswas
 * 
 */
public class FileLockHelper {
//    private static HashMap<String, Lock> map = new HashMap<String, Lock>();
//
//    public static synchronized Lock getLock(Endpoint endpoint, File file) throws FileNotFoundException {
//
//	Lock lock = null;
//
//	String key = endpoint.getServiceName().toString() + endpoint.getEndpointName();
//	lock = map.get(key);
//
//	if (lock == null) {
//		File parentDir = file.getParentFile();
//		if(parentDir!=null && !parentDir.exists()){
//			parentDir.mkdirs();
//		}
//	    String s = file.getAbsolutePath() + ".lck";
//	    lock = new Lock(new FileOutputStream(s), new ReentrantLock(), s);
//	    map.put(key, lock);
//	}
//
//	return lock;
//
//    }
//    
//    public static synchronized Lock getLockPerFile(Endpoint endpoint, File file) throws FileNotFoundException {
//
//	Lock lock = null;
//
//	String key = endpoint.getServiceName().toString() + endpoint.getEndpointName() + file.getName();
//	lock = map.get(key);
//
//	if (lock == null) {
//		File parentDir = file.getParentFile();
//		if(parentDir!=null && !parentDir.exists()){
//			parentDir.mkdirs();
//		}
//	    String s = file.getAbsolutePath() + ".lck";
//	    lock = new Lock(new FileOutputStream(s), new ReentrantLock(), s);
//	    map.put(key, lock);
//	}
//
//	return lock;
//
//    }
//
//
//    public static synchronized void removeKey(Endpoint endpoint) throws IOException {
//	String key = endpoint.getServiceName().toString() + endpoint.getEndpointName();
//	Lock fc = map.remove(key);
//	if (fc != null) {
//	    fc.getFileOutputStream().close();
//	    String s = fc.getLockFilePath();
//	    File f = new File(s);
//	    if (f.exists()) {
//		f.delete();
//	    }
//
//	}
//	
//	Iterator<String> iter = map.keySet().iterator();
//	while (iter.hasNext()) {
//	    String string = (String) iter.next();
//	    if(string.startsWith(key)){
//		Lock fc1 = map.remove(string);
//		if (fc1 != null) {
//		    fc1.getFileOutputStream().close();
//		    String s = fc1.getLockFilePath();
//		    File f = new File(s);
//		    if (f.exists()) {
//			f.delete();
//		    }
//
//		}
//		
//	    }
//	    
//	}
//
//    }
}
