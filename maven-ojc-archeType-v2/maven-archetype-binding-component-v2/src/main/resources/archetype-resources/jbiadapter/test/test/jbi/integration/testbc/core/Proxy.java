#set( $symbol_pound = '#' )
#set( $symbol_dollar = '$' )
#set( $symbol_escape = '\' )
package test.jbi.integration.testbc.core;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Proxy {
	
	private static final Logger mLog = Logger.getLogger(Proxy.class.getName());
	
	private Socket s;
	private DataOutputStream out;
	private DataInputStream in;
	private Hashtable<Integer, MessageListener> conversations = new Hashtable<Integer, MessageListener>();
	private ThreadPoolExecutor pool;
	private static final byte RUN_COMMAND = 11;
	private static final byte GOT_REPLY = 22;
	private static final byte CLOSE = 33;
	private Sandbox sandbox;
	private int conversationCounter = 1;
	private List<CloseEventListener> closeEventListeners = new ArrayList<CloseEventListener>();
	private boolean isClosed = false;
	private Object mHandler;
	
	public Proxy(Socket s, Sandbox sb, Object handler) throws IOException{
		this.s = s;
		sandbox = sb;
		this.mHandler = handler;
		out = new DataOutputStream(s.getOutputStream());
		in = new DataInputStream(s.getInputStream());
		pool = new ThreadPoolExecutor(10, 10, 10, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
	}
	
	public void start(){
		new Thread(new ReaderThread()).start();
	}
	
	synchronized public byte[] sendRequestReply(byte[] data) throws IOException{
		if(isClosed)
			throw new IOException("Connection is closed.");

		Message msg = new Message(conversationCounter++, data, RUN_COMMAND);
		final Object[] result = new Object[1];
		result[0] = null;
		conversations.put(new Integer(msg.getConversationId()), new MessageListener() {
			public void gotReply(byte[] msg) {
				synchronized (result) {
					result[0] = msg;
					result.notify();
				}
			}
		});
		write(msg);
		synchronized (result) {
			if(result[0]==null){
				try {
					result.wait();
				} catch (InterruptedException e) {
				}
			}
		}
		return (byte[])result[0];
	}

	public void sendClose() throws IOException{
		synchronized (out) {
			out.writeByte(CLOSE);
		}
	}
	
	synchronized public void close(){
		if(isClosed)
			return;
		
		isClosed = true;
		//Close the socket
		try{
			s.shutdownOutput();
		}catch(Throwable t){}
		try{
			s.shutdownInput();
		}catch(Throwable t){}
		try{
			s.close();
		}catch(Throwable t){}
		notifyCloseEventListeners();
		pool.shutdown();
	}
	
	public boolean isClosed(){
		return isClosed;
	}
	
	private void write(Message msg) throws IOException {
		synchronized (out) {
			out.writeByte(msg.getTaskId());
			out.writeInt(msg.getConversationId());
			out.writeInt(msg.getData().length);
			out.write(msg.getData());
		}
	}

	synchronized public void addCloseEventListener(CloseEventListener l){
		closeEventListeners.add(l);
	}
	
	synchronized private void notifyCloseEventListeners(){
		for(Iterator<CloseEventListener> iter=closeEventListeners.iterator(); iter.hasNext();){
			CloseEventListener l = iter.next();
			l.socketClosed(s);
			iter.remove();
		}
	}

	private class ReaderThread implements Runnable{
		
		public void run(){
			
			for(;;){
				
				//read the task first
				try {
					byte taskId = in.readByte();
					if(taskId == CLOSE){
						//Send close in return if possible 
						//and then close socket
						sendClose();
						//Close the socket
						close();
						break;
					}
					if(taskId == -1){
						mLog.info("Socket is closed ." );
						break;
					}
					int conversationId = in.readInt();
					int len = in.readInt();
					byte[] data =  new byte[len];
					int i = in.read(data);
					int offset = 0;
					while(i < len){
						if(i == -1){
							break;
						}
						//make sure to read all data. this is important
						len -= i;
						offset += i;
						i = in.read(data, offset, len);
					}
					
					if(i == -1){
						mLog.info("Socket is closed while still reading data." );
						data = null;
						break;
					}
					
					final Message msg = new Message(conversationId, data, taskId);
					if(taskId == RUN_COMMAND){
						pool.execute(new Runnable() {
							public void run() {
								byte[] output = sandbox.execute(msg.getData(), mHandler);
								msg.setData(output);
								msg.setTaskId(GOT_REPLY);
								//send the reply back
								try {
									write(msg);
								} catch (IOException e) {
									mLog.log(Level.WARNING, "Could not sent the reply back for Message = " + msg, e);
								}
							}
						});
					}else if(taskId == GOT_REPLY){
						pool.execute(new Runnable() {
							public void run() {
								MessageListener l = conversations.remove(new Integer(msg.getConversationId()));
								if(l == null){
									mLog.warning("No one was waiting for this reply. = " + msg); //ignore
								}else{
									l.gotReply(msg.getData());
								}
							}
						});
					}else{
						mLog.warning("Invalid task ID received. Message = " + msg);
					}
				} catch (IOException e) {
					mLog.warning("Socket is closed abruptly. " + e.getMessage());
					close();
					break;
				}
				
				
			}
		}
	}
	
	public interface CloseEventListener{
		public void socketClosed(Socket s);
	}
	
	private interface MessageListener {
		void gotReply(byte[] msg);
	}

	private class Message {
		
		private int conversationId;
		private byte[] data;
		private byte taskId;
		
		public Message(int conversationId, byte[] data, byte taskId){
			this.conversationId = conversationId;
			this.data = data;
			this.taskId = taskId;
		}

		public byte[] getData() {
			return data;
		}

		public void setData(byte[] data) {
			this.data = data;
		}

		public byte getTaskId() {
			return taskId;
		}

		public void setTaskId(byte taskId) {
			this.taskId = taskId;
		}

		public int getConversationId() {
			return conversationId;
		}

		@Override
		public String toString() {
			return ("ConversationId=" + conversationId + ", TaskId=" + taskId);
		}
		
	}	
}
