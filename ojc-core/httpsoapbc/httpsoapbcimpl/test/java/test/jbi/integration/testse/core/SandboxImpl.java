package test.jbi.integration.testse.core;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;

import javax.jbi.component.ComponentContext;


public class SandboxImpl implements Sandbox{
	private static final byte[] BAD_REPLY;
	
	static{
		BAD_REPLY = new byte[2];
		BAD_REPLY[0] = 0;
		BAD_REPLY[0] = 0;
	}
	
	public byte[] execute(byte[] data, Object handler) {
		byte[] reply = null;
		try{
			Command c = deserializeObject(data);
			reply = Util.serializeObject(c.execute((ComponentContext)handler));
		} catch (Throwable e) {
			try {
				reply = Util.serializeObject(e);
			} catch (IOException e1) {}
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
