package test.jbi.integration.testbc.core;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;

public class SandboxClassLoader extends ClassLoader {

	static public final String EXECUTOR_CLASS;
	static private boolean isExecutorLoaded = false;
	static private byte[] executorBytes;
	static private final ClassLoader PARENT_CLASSLOADER = SandboxClassLoader.class.getClassLoader(); 
	private String mWkDir;
	static{
		String name = SandboxClassLoader.class.getName();
		EXECUTOR_CLASS = name.substring(0, name.lastIndexOf('.')) + ".SandboxImpl";
		InputStream in = null;
		try{
			in = PARENT_CLASSLOADER.getResourceAsStream(EXECUTOR_CLASS.replace('.', '/') + ".class");
			if(in != null){
				executorBytes = Util.readBytes(in);
				isExecutorLoaded = true;
			}
		} catch (Throwable e) {
		}finally{
			Util.closeStream(in);
		}
	}
	private Proxy mProxy;
	
	public SandboxClassLoader(String wkDir){
		super(PARENT_CLASSLOADER);
		if(!isExecutorLoaded)
			throw new RuntimeException("Could not instantiate CustomClassLoader");
		mWkDir = wkDir;
		if(!mWkDir.endsWith(File.separator)){
			mWkDir += File.separator;
		}
	}
	
	public void setProxy(Proxy p){
		mProxy = p;
	}

	@Override
	protected synchronized Class<?> loadClass(String name, boolean resolve)
			throws ClassNotFoundException {
		if(name.equals(EXECUTOR_CLASS)){
			return findClass(name);
		}else{
			return super.loadClass(name, resolve);
		}
	}

	@Override
	protected Class<?> findClass(String name) throws ClassNotFoundException {
		byte[] clsBytes;
		if(name.equals(EXECUTOR_CLASS)){
			clsBytes = executorBytes;
		}else{
			GetClassCommand c = new GetClassCommand();
			c.setResourceName(name.replace('.', '/') + ".class");
			try {
				byte[] request = Util.serializeObject(c);
				byte[] reply = mProxy.sendRequestReply(request);
				clsBytes = (byte[])Util.deserializeObject(reply);
			} catch (Throwable e) {
				throw new ClassNotFoundException("Could not load class " + name, e);
			}
		}
		return defineClass(name, clsBytes, 0, clsBytes.length);
	}
	
	private HashMap<String, URL> cachedResources = new HashMap<String, URL>();
	
	@Override
	protected URL findResource(String name) {
		synchronized (this) {
			URL url = cachedResources.get(name);
			if(url != null)
				return url;
			
			GetClassCommand c = new GetClassCommand();
			c.setResourceName(name);
			byte[] data = null;
			try {
				byte[] request = Util.serializeObject(c);
				byte[] reply = mProxy.sendRequestReply(request);
				data = (byte[])Util.deserializeObject(reply);
			} catch (Throwable e) {}
			if(data == null)
				return null;
			
			int i = name.lastIndexOf('/');
			String dir = mWkDir + name.substring(0, i);
			String fileName = name.substring(i+1);
			File f = new File(dir);
			f.mkdirs();
			f = new File(dir + File.separator + fileName);
			try {
				FileOutputStream fout = new FileOutputStream(f);
				fout.write(data);
				fout.close();
				url = f.toURL();
				cachedResources.put(name, url);
				return url;
			} catch (IOException e) {
				return null;
			}
		}
	}
}
