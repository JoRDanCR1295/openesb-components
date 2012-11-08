package test.jbi.integration.testbc.core;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

final public class Util {

	public static void closeStream(Closeable s){
		if(s == null)
			return;
		try{
			s.close();
		}catch(Throwable t){}
	}
	
	public static byte[] readBytes(InputStream fin) throws IOException {
		ByteArrayOutputStream bout = new ByteArrayOutputStream(1028);
		try {
			byte[] buf = new byte[1028];
			int len;
			while ((len = fin.read(buf)) != -1) {
				bout.write(buf, 0, len);
			}
			byte[] clsBytes = bout.toByteArray();
			return clsBytes;
		} finally {
			closeStream(bout);
		}
	}
	public static Object deserializeObject(byte[] data) throws IOException, ClassNotFoundException{
		ObjectInputStream oin = null;
		Object c;
		try{
			oin = new ObjectInputStream(new ByteArrayInputStream(data));
			c = oin.readObject();
		}finally{
			closeStream(oin);
		}
		return c;
	}

	public static byte[] serializeObject(Object obj) throws IOException{
		ObjectOutputStream oout = null;
		ByteArrayOutputStream bout = new ByteArrayOutputStream(1028);
		try{
			oout = new ObjectOutputStream(bout);
			oout.writeObject(obj);
			return bout.toByteArray();
		}finally{
			closeStream(oout);
		}
	}
}
