package test.jbi.integration.testse.core;

import java.io.IOException;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;


public class Connection {

	private static final Logger mLog = Logger.getLogger(Connection.class.getName());
	
	private int serverPort;
	private Proxy proxy;
	private Sandbox sandbox;
	private Socket s;
	private String hostname;
	private boolean isClosed;
	
	public Connection(String hostname, int port){
		this.serverPort = port;
		this.hostname = hostname;
		isClosed = false;
	}

	public void start() throws IOException{
		s = new Socket(hostname, serverPort);
		sandbox = new SandboxImpl();
		proxy = new Proxy(s, sandbox, null);
		proxy.addCloseEventListener(new Proxy.CloseEventListener() {
			public void socketClosed(Socket s) {
				isClosed = true;
				try{
					s.close();
				}catch(Throwable t){}
			}
		});
		proxy.start();
	}

	public Object execute(Command cmd) throws Throwable{
		byte[] data = Util.serializeObject(cmd);
		byte[] reply = proxy.sendRequestReply(data);
		Object obj = Util.deserializeObject(reply);
		if(obj instanceof Throwable)
			throw (Throwable)obj;
		return obj;
	}
	
	public boolean isClosed(){
		return isClosed;
	}
	
	public void close(){
		if(isClosed)
			return;
		
		try{
			proxy.sendClose();
		}catch(Throwable t){
			mLog.log(Level.WARNING, "Could not close connection properly.", t);
		}
	}
	
	
}
