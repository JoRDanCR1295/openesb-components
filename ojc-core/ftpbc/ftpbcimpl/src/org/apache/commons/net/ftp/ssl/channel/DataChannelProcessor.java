package org.apache.commons.net.ftp.ssl.channel;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.nio.ByteBuffer;
import java.util.LinkedList;
import java.util.concurrent.Semaphore;
import java.util.logging.Logger;

import javax.net.ssl.SSLException;

import org.apache.commons.net.ftp.ssl.ErrorHandler;

public class DataChannelProcessor implements ChannelListener, ErrorHandler {
	private final static Logger log = Logger.getLogger("DataChannel");

	private Channel mChannel;
	private ByteBuffer mInBuf;
	private ByteBuffer mOutBuf;
	private PipedOutputStream mResponseSink;
	private PipedInputStream mResponseSource;
	private Output2BufferAdaptor mRequestSink;
	private LinkedList<ByteBuffer> mDataChunks;
	
	private Semaphore mSema;
	private Semaphore mSSLChannelSema;

	private Exception mAsyncError;

	public DataChannelProcessor() throws IOException {
		mSema = new Semaphore(1);
		mSSLChannelSema = new Semaphore(0);
		mRequestSink = new Output2BufferAdaptor(this);
		mResponseSink = new PipedOutputStream();
		mResponseSource = new PipedInputStream(mResponseSink);
		mDataChunks = new LinkedList<ByteBuffer>();
	}
	
	/**
	 * read incoming data from peer, make it available 
	 * for InputStream reading;
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
				return;
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
			mChannel.registerForRead();
		}
		catch (IOException e) {
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
	 * write data chunks to the peer
	 */
	public void processOutbound() {
		try {
			mSema.acquire();
			mOutBuf = mDataChunks.peek();
			if ( mOutBuf != null ) {
				if ( mOutBuf.capacity() == 0 ) {
					// EOF indicator
					mChannel.close();
					return;
				}
				if ( mOutBuf.hasRemaining() ) {
					int len = mChannel.write(mOutBuf);
					if ( mOutBuf.hasRemaining() ) {
						// the buffer should stay in the Q
						// for further write attempt
						mChannel.registerForWrite();
						mChannel.getDispatcher().getSelector().wakeup();
					}
					else {
						// deQ
						mDataChunks.removeFirst();
						if ( !mDataChunks.isEmpty() )
							mChannel.registerForWrite();
					}
				}
				else {
					// app indicate a EOF
					// deQ
					mDataChunks.removeFirst();
				}
				// write done
				mOutBuf = null;
				mChannel.getDispatcher().getSelector().wakeup();
			}
		}
		catch (IOException e) {
			e.printStackTrace();
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
			mDataChunks.add(buffer);
			mChannel.registerForWrite();
			mChannel.getDispatcher().getSelector().wakeup();
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

	public void handleError(Exception ex) {
		mAsyncError = ex;
	}

	public void getInput() throws IOException {
		mChannel.registerForRead();
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
