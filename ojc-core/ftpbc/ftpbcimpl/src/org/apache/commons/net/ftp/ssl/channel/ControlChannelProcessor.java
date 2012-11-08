package org.apache.commons.net.ftp.ssl.channel;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.concurrent.Semaphore;
import java.util.logging.Logger;

import org.apache.commons.net.ftp.ssl.ErrorHandler;

public class ControlChannelProcessor implements ChannelListener, ErrorHandler {
	private final static Logger log = Logger.getLogger("ControlChannel");

	private Channel mChannel;
	private ByteBuffer mInBuf;
	private ByteBuffer mOutBuf;
	private PipedOutputStream mResponseSink;
	private PipedInputStream mResponseSource;
	private Output2BufferAdaptor mRequestSink;
	
	private LinkedList<ByteBuffer> mCommandQueue;
	
	private Semaphore mSema;
	private Semaphore mSSLChannelSema;
	private Exception mAsyncError;
	
	public ControlChannelProcessor() throws IOException {
		mSema = new Semaphore(1);
		mSSLChannelSema = new Semaphore(0);
		mRequestSink = new Output2BufferAdaptor(this);
		mResponseSink = new PipedOutputStream();
		mResponseSource = new PipedInputStream(mResponseSink);
		mCommandQueue = new LinkedList();
	}
	
	/**
	 * read the response for a command
	 */
	public void processInbound() {
		try {
			if ( mInBuf == null )
				mInBuf = ByteBuffer.allocate(mChannel.getAppBufSz());

			int len = mChannel.read(mInBuf);
			if ( len == -1 ) {
				// EOF - the server closed the stream
				mChannel.close();
				mResponseSink.close();
			}
			
			if ( len == 0 ) {
				// read further
				mChannel.registerForRead();
				return;
			}
			
			mInBuf.flip();
			// readLine should recognize a complete response
			mResponseSink.write(mInBuf.array(), 0, mInBuf.limit());
			mResponseSink.flush();
			mInBuf.clear();
		}
		catch (IOException e) {
			// notify the client that socket closed
			e.printStackTrace();
			try {
				mChannel.close();
			}
			catch (Exception e1) {
				e1.printStackTrace();
			}
		}
	}

	/**
	 * write through the secured channel if there is 
	 * application data - FTP commands
	 */
	public void processOutbound() {
		try {
			mSema.acquire();
			mOutBuf = mCommandQueue.peek();
			if ( mOutBuf != null ) {
				int len = mChannel.write(mOutBuf);
				if ( mOutBuf.hasRemaining() ) {
					// the buffer should stay in the Q
					// for further write attempt
					mChannel.registerForWrite();
				}
				else {
					// deQ
					mCommandQueue.removeFirst();
					if ( !mCommandQueue.isEmpty() )
						mChannel.registerForWrite();
				}
				// write done
				mOutBuf = null;
			}
			else {
				mChannel.registerForWrite();
			}
		}
		catch (IOException e) {
			try {
				mChannel.close();
			}
			catch (Exception e1) {
				e1.printStackTrace();
			}
		}
		catch (InterruptedException e) {
			e.printStackTrace();
		}
		finally {
			mSema.release();
		}
	}

	public void setSecureChannel(Channel channel) {
		mChannel = channel;
	}

	public void close() {
		try {
			mChannel.close();
		}
		catch (Exception e) {
			// ignore
			e.printStackTrace();
		}
	}

	public InputStream getInputStream() {
		return mResponseSource;
	}

	public boolean queueOutput(ByteBuffer buffer) throws IOException {
		boolean result = true;
		try {
			mSema.acquire();
			mCommandQueue.add(buffer);
			mChannel.registerForWrite();
		}
		catch (InterruptedException e) {
			e.printStackTrace();
			result = false;
		}
		finally {
			mSema.release();
		}
		return result;
	}

	public OutputStream getOutputStream() {
		return mRequestSink;
	}

	public void getInput() throws IOException {
		mChannel.registerForRead();
	}
	
	public void handleError(Exception ex) {
		mAsyncError = ex;
	}

	public void notifySSLChannelClosure() {
		mSSLChannelSema.tryAcquire();
		mSSLChannelSema.release();
	}

	public void waitSSLChannelClosure() {
		try {
			mSSLChannelSema.acquire();
		}
		catch (InterruptedException e) {
			e.printStackTrace();
			mSSLChannelSema.release();
		}
	}
}
