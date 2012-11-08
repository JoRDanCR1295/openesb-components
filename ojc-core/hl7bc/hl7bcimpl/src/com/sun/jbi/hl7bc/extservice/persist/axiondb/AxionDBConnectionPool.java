/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)AxionDBConnectionPool.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.persist.axiondb;

import java.sql.Connection;
import java.sql.DriverManager;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.hl7bc.HL7RuntimeException;
import com.sun.jbi.hl7bc.I18n;

public class AxionDBConnectionPool {

	private static Logger mLogger = Logger.getLogger(AxionDBConnectionPool.class.getName());

	private int maxPoolSize = 10;
	
	private static AxionDBConnectionPool _instance = null;
	
	private static String dummy = "dummy";
	
    private static final String AXION_DRIVER_CLASS = "org.axiondb.jdbc.AxionDriver";
    private static final String AXION_DB_NAME = "HL7BCDB";

	private String connectString = null;
	private ArrayList<AxionDBConnection> connections = null;;
	private ConnectionReaper reaper = null;
    final private long timeout=60000;
    
    
    
	public static AxionDBConnectionPool getInstance(String dbLocation){
		synchronized(dummy){
			if( _instance == null){
				_instance = new AxionDBConnectionPool(dbLocation);
			}
		}
		return _instance;
	}
	
	private AxionDBConnectionPool(String dbLocation){
		this.connectString = "jdbc:axiondb:" + AXION_DB_NAME + ":" + dbLocation + "\\axiondb";
		connections = new ArrayList<AxionDBConnection>(maxPoolSize);
		reaper = new ConnectionReaper(this);
		reaper.start();
	}
	
   public synchronized void reapConnections() {

	      long stale = System.currentTimeMillis() - timeout;
	      Iterator<AxionDBConnection> connlist = connections.iterator();	    
	      while(connlist.hasNext()) {
	    	  AxionDBConnection conn = connlist.next();

	          if((conn.inUse()) && (stale > conn.getLastUse()) && (!conn.validate())) {
	        	  removeConnection(conn);
	         }
	      }
	   }

	   public synchronized void closeConnections() {
	        
	      Iterator<AxionDBConnection> connlist = connections.iterator();

	      while(connlist.hasNext()) {
	    	  AxionDBConnection conn = (AxionDBConnection)connlist.next();
	          removeConnection(conn);
	      }
	   }

	   private synchronized void removeConnection(AxionDBConnection conn) {
	       connections.remove(conn);
	   }


	   public synchronized AxionDBConnection getConnection() throws HL7RuntimeException{

		
		   
		   //find a free connection
		   AxionDBConnection c;
	       for(int i = 0; i < connections.size(); i++) {
	           c = (AxionDBConnection)connections.get(i);
	           if (c.lease()) {
	              return c;
	           }
	       }
	       
	       //No free connections found. Get a new connection
	       try {
	    	   Class.forName(AXION_DRIVER_CLASS);
	    	   Connection conn = DriverManager.getConnection(connectString);
	    	   c = new AxionDBConnection(conn, this);
			} catch (Exception e) {
				mLogger.log(Level.SEVERE, I18n.msg("E0295: Unable to get Axion DB Connection"), e);
				throw new HL7RuntimeException(I18n.msg("E0295: Unable to get Axion DB Connection"), e);
			}
	       
	       c.lease();
	       connections.add(c);
	       return c;
	  } 

	   public synchronized void returnConnection(AxionDBConnection conn) {
	      conn.expireLease();
	   }
	
	   class ConnectionReaper extends Thread {

		    private AxionDBConnectionPool pool;
		    private final long delay=300000;

		    ConnectionReaper(AxionDBConnectionPool pool) {
		        this.pool=pool;
		    }

		    public void run() {
		        while(true) {
		           try {
		              sleep(delay);
		           } catch( InterruptedException e) { }
		           pool.reapConnections();
		        }
		    }
		}
	   
	
}
