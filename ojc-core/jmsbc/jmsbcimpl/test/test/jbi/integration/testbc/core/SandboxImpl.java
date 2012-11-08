package test.jbi.integration.testbc.core;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.PrivilegedExceptionAction;

import javax.jbi.component.ComponentContext;


public class SandboxImpl implements Sandbox{
	private static final byte[] BAD_REPLY;
	private static final ClassLoader CLASSLOADER;
	
	static{
		BAD_REPLY = new byte[2];
		BAD_REPLY[0] = 0;
		BAD_REPLY[0] = 0;
		CLASSLOADER = SandboxImpl.class.getClassLoader();
	}
	
	public byte[] execute(byte[] data, final Object handler) {
		byte[] reply = null;
		ClassLoader cls = Thread.currentThread().getContextClassLoader();
		try{
			Thread.currentThread().setContextClassLoader(CLASSLOADER);
			final Command c = deserializeObject(data);
			reply = Util.serializeObject(c.execute((ComponentContext)handler));
		} catch (Throwable e) {
			e.fillInStackTrace();
			try {
				reply = Util.serializeObject(e);
			} catch (IOException e1) {}
		}finally{
			Thread.currentThread().setContextClassLoader(cls);
		}
		if(reply == null)
			reply = BAD_REPLY;
		return reply;
		
	}
	
	private static Command deserializeObject(byte[] data) throws IOException, ClassNotFoundException{
		ObjectInputStream oin = null;
		Command c;
		try{
			oin = new ObjectInputStream(new ByteArrayInputStream(data));
			c = (Command)oin.readObject();
		}finally{
			Util.closeStream(oin);
		}
		return c;
	}
	
}
