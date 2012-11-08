package test.jbi.integration.testbc.core;

import java.io.InputStream;

public class SandboxClassLoader extends ClassLoader {

	static public final String EXECUTOR_CLASS;
	static private boolean isExecutorLoaded = false;
	static private byte[] executorBytes;
	static private final ClassLoader PARENT_CLASSLOADER = SandboxClassLoader.class.getClassLoader(); 
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
	
	public SandboxClassLoader(){
		super(PARENT_CLASSLOADER);
		if(!isExecutorLoaded)
			throw new RuntimeException("Could not instantiate CustomClassLoader");
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
			c.setClassName(name);
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
	
}
