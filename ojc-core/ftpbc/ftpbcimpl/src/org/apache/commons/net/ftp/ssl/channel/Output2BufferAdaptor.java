package org.apache.commons.net.ftp.ssl.channel;

import java.io.ByteArrayOutputStream;
import java.io.FilterOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;


public class Output2BufferAdaptor extends FilterOutputStream {
	public static final int DEFAULT_BUFSZ = 1024;
	private ChannelListener mAppChannel;
	private final ByteArrayOutputStream mBytePool = new ByteArrayOutputStream(DEFAULT_BUFSZ);
	private boolean mClosed;
	
	public Output2BufferAdaptor(ChannelListener listener)
    {
		super(null);
		mAppChannel = listener;
    }

	public void open() {
		mClosed = false;
		mBytePool.reset();
	}
	
    public void write(int ch) throws IOException
    {
    	if ( mClosed )
    		throw new IllegalStateException("Invalid operation on closed Output2BufferAdaptor.");
    	mBytePool.write(ch);
    	if ( mBytePool.size() == DEFAULT_BUFSZ ) {
    		ByteBuffer bb = ByteBuffer.allocate(mBytePool.size());
    		bb.put(mBytePool.toByteArray());
    		bb.flip();
    		mAppChannel.queueOutput(bb);
    		mBytePool.reset();
    	}
    }

    public void write(byte buffer[]) throws IOException
    {
    	write(buffer, 0, buffer.length);
    }

    public void write(byte buffer[], int offset, int length) throws IOException
    {
    	if ( mClosed )
    		throw new IllegalStateException("Invalid operation on closed Output2BufferAdaptor.");
    	int start = offset;
    	int len = length;
    	int space = 0;
    	while ( len > 0 ) {
	    	if ( mBytePool.size() < DEFAULT_BUFSZ ) {
	    		space = DEFAULT_BUFSZ - mBytePool.size();
	    		if ( len >= space ) {
	    			mBytePool.write(buffer, start, space);
	    			start += space;
	    			len -= space;
	    		}
	    		else { 
	    			mBytePool.write(buffer, start, len);
	    			len = 0;
	    		}
	    	}
	    	if ( mBytePool.size() >= DEFAULT_BUFSZ ) {
	    		ByteBuffer bb = ByteBuffer.allocate(mBytePool.size());
	    		bb.put(mBytePool.toByteArray());
	    		bb.flip();
	    		mAppChannel.queueOutput(bb);
	    		mBytePool.reset();
	    	}
    	}
    }

    public void close() throws IOException
    {
    	flush();
    	// indicate EOF - end of data
    	mAppChannel.queueOutput(ByteBuffer.allocate(0));
    	mClosed = true;
    	// for command channel - we usually do not close
    	// 
    	mAppChannel.waitSSLChannelClosure();
    }

    public void flush() throws IOException {
    	if ( mClosed )
    		throw new IllegalStateException("Invalid operation on closed Output2BufferAdaptor.");
    	if ( mBytePool.size() > 0 ) {
        	mAppChannel.queueOutput(ByteBuffer.wrap(mBytePool.toByteArray()));
        	mBytePool.reset();
    	}
    }
}
