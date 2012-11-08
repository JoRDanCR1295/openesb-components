package org.apache.commons.net.ftp.ssl.channel;

import java.nio.ByteBuffer;

public class Utils {
    public static String bufferToString(ByteBuffer bb) {
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < bb.limit(); i++) {
			sb.append((char)bb.get(i));
		}
		return sb.toString();
	}
}
