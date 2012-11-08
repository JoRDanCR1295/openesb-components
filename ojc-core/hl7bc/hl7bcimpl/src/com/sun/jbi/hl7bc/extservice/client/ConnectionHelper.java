package com.sun.jbi.hl7bc.extservice.client;

import java.net.InetSocketAddress;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.mina.common.ConnectFuture;
import org.apache.mina.common.IoConnector;
import org.apache.mina.common.IoHandler;
import org.apache.mina.common.IoServiceConfig;
import org.apache.mina.transport.socket.nio.SocketConnector;

import com.sun.jbi.hl7bc.connection.ConnectionInfo;
import com.sun.jbi.hl7bc.extensions.HL7Address;
import com.sun.jbi.hl7bc.extensions.HL7CommunicationControl;
import com.sun.jbi.hl7bc.extservice.ProtocolInfo;
import com.sun.jbi.hl7bc.HL7Constants;
import com.sun.jbi.hl7bc.I18n;

public class ConnectionHelper {

    private static final Logger mLog = Logger.getLogger(ConnectionHelper.class.getName());
	

	public ConnectFuture getConnection(String hostName, int port, IoHandler ioHandler, IoServiceConfig config, String retryLogicString ) throws Exception{
        IoConnector connector = new SocketConnector();   
        ConnectFuture future = null;
        
        if(retryLogicString != null && !"".equals(retryLogicString)){
        	mLog.info(I18n.msg("I0158: Executing MAX_CONNECT_RETRY communication control."));
	        //retryLogicString will be of the form n1;n2,n1;n2,... where n1 is the retry count and n2 is retry interval in seconds
	        String s[] = retryLogicString.split(",");
	        for(String s1 : s){
	        	
	        	String s2[] = s1.split(";");
	        	if(s2.length == 1){ //if the retryLogicString is not of the expected regular expression, execute connection only once
	        		mLog.info(I18n.msg("I0159: The value for MAX_CONNECT_RETRY  is not of the format n1;n2,n1;n2,..... Connecting only once"));
	    			future = connector.connect(new InetSocketAddress(hostName, port), ioHandler, config);
	    			future.join();
	    			break;
	    			
	        	}
	        	int n1 = Integer.valueOf(s2[0]);
	        	int n2 = Integer.valueOf(s2[1]);
	        	n2 = n2 * 1000;
                int count = 0;
	        	if(n1 < 0){ //if n1 is -1 try to connect infinitely until the external system is available.
	        		while(true){
	            		try {
	    					future = connector.connect(new InetSocketAddress(hostName, port), ioHandler, config);
	    					future.join();
	    					future.getSession();
	    					break;
	    				} catch (Exception e) {
                            count++;
	    					mLog.log(Level.WARNING, I18n.msg("W0127: Connection not available. Retrying to connect..[{0}]", count));
	    					try {
								Thread.sleep(n2);
							} catch (InterruptedException e1) {
								//ignore
							}
	    				}
	        		}
	        	}else{
		        	for(int i = 0; i < n1 ; i++){
		        		try {
							future = connector.connect(new InetSocketAddress(hostName, port), ioHandler, config);
							future.join();
							future.getSession();
							break;
						} catch (Exception e) {
							if(i == n1){
								throw e;
							}
	    					mLog.log(Level.WARNING, I18n.msg("W0127: Connection not available. Retrying to connect..[{0}]", count));
							try {
								Thread.sleep(n2);
							} catch (InterruptedException e1) {
								//ignore
							}
						}
		        	}
	        	}
	        }
        }else{
        	future = connector.connect(new InetSocketAddress(hostName, port), ioHandler, config);
        	future.join();
        	future.getSession();
        }
        return future;
	}
	public ConnectFuture getConnection(ConnectionInfo connectionInfo) throws Exception{
		return getConnection(connectionInfo.getHost(),connectionInfo.getPort(),connectionInfo.getIoHandler(),connectionInfo.getIoServiceConfig(),connectionInfo.getRetryLogicString());
	}
}
