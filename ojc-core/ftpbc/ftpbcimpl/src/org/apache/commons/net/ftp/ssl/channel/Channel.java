package org.apache.commons.net.ftp.ssl.channel;

import org.apache.commons.net.ftp.ssl.ChannelSelectorDispatcher;
import org.apache.commons.net.ftp.ssl.ReadWriteHandler;

import java.io.IOException;
import java.nio.ByteBuffer;

public interface Channel extends ReadWriteHandler {
	public int read(ByteBuffer bb) throws IOException; 
	public int write(ByteBuffer bb) throws IOException;
	public void registerForRead() throws IOException;
	public void unregisterForRead() throws IOException;
	public void registerForWrite() throws IOException;
	public void unregisterForWrite() throws IOException;
	public void close() throws IOException;
	public int getPacketBufSz();
	public int getAppBufSz();
        public void establishSSLSession() throws IOException;
	public ChannelSelectorDispatcher getDispatcher();
}
