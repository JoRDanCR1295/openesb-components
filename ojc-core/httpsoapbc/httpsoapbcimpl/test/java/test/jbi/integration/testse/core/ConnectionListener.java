package test.jbi.integration.testse.core;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;


public class ConnectionListener {

	private static final Logger mLog = Logger.getLogger(ConnectionListener.class.getName());
	
	private int listenerPort;
	private ServerSocket ss;
	private ThreadPoolExecutor pool;
	private Object mHandlerObject;
	private List<Socket> clientSockets = new Vector<Socket>();

	public ConnectionListener(int port, Object handler) throws IOException{
		this.listenerPort = port;
		this.mHandlerObject = handler;
		pool = new ThreadPoolExecutor(10, 10, 10, TimeUnit.SECONDS, new LinkedBlockingQueue<Runnable>());
		ss = new ServerSocket(this.listenerPort);
	}
	
	public void start(){
		new Thread(new ListenerThread()).start();
	}

	public void close(){
		try {
			ss.close();
		} catch (Throwable e) {}
		//also close any open sockets
		for(Iterator<Socket> iter=clientSockets.iterator(); iter.hasNext();){
			try{
				iter.next().close();
			}catch(Throwable t){}
		}
		pool.shutdown();
	}
	
	public class ListenerThread implements Runnable{
		public void run(){
			for(;;){
				Socket s = null;
				try {
					s = ss.accept();
				} catch (IOException e) {
					if(ss.isClosed()){
						mLog.log(Level.INFO, "Listener is closed at port " + listenerPort);
						break;
					}
					mLog.log(Level.SEVERE, "Error accepting at port " + listenerPort, e);
				}
				
				if(s == null)
					continue;
				
				final Socket cs = s;
				clientSockets.add(cs);
				pool.execute(new Runnable() {
					public void run() {
						//Create new Customclassloader
						SandboxClassLoader clsLoader = new SandboxClassLoader();
						Sandbox sandbox = null;
						try {
							sandbox = (Sandbox) (clsLoader
									.loadClass(SandboxClassLoader.EXECUTOR_CLASS)
									.newInstance());
						} catch (Exception e) {
							mLog.log(Level.SEVERE, "Could not instanciate sandbox " + cs.getPort());
						}
						try {
							final Proxy proxy = new Proxy(cs, sandbox, mHandlerObject);
							proxy.addCloseEventListener(new Proxy.CloseEventListener() {
								public void socketClosed(Socket s) {
									clientSockets.remove(s);
								}
							});
							clsLoader.setProxy(proxy);
							proxy.start();
						} catch (IOException e) {
							mLog.log(Level.SEVERE, "Could not open connection at port " + cs.getPort(), e);
							return;
						}
						
					}
				});
				
			}
		}
	}
	
}
